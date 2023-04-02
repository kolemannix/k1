use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{BufReader, Read};

use TokenKind::*;

use crate::ast::*;
use crate::lex::*;
use crate::trace;

#[cfg(test)]
mod parse_test;

pub type ParseResult<A> = Result<A, ParseError>;

#[derive(Debug)]
pub enum ParseError {
    ExpectedToken(TokenKind, Token, Option<Box<ParseError>>),
    ExpectedNode(String, Token, Option<Box<ParseError>>),
    Msg(String, Token),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}
impl std::error::Error for ParseError {}

pub struct Source<'a> {
    content: &'a str,
    lines: Vec<&'a str>,
}
impl<'a> Source<'a> {
    fn line(&self, num: u32) -> &'a str {
        &self.lines[num as usize]
    }
}

struct Parser<'a> {
    tokens: TokenIter,
    source: Source<'a>,
}

impl<'a> Parser<'a> {
    fn check<A>(value: Option<A>) -> ParseResult<Option<A>> {
        match value {
            None => Result::Ok(None),
            Some(a) => Result::Ok(Some(a)),
        }
    }
    fn expect<A>(what: &str, current: Token, value: ParseResult<Option<A>>) -> ParseResult<A> {
        match value {
            Ok(None) => Err(ParseError::ExpectedNode(what.to_string(), current, None)),
            Ok(Some(a)) => Ok(a),
            Err(e) => Err(e),
        }
    }
}

impl<'a> Parser<'a> {
    fn peek(&self) -> Token {
        self.tokens.peek()
    }
    fn make(tokens: TokenIter, source: &'a str) -> Parser<'a> {
        let lines = source.lines().collect();
        Parser { tokens, source: Source { content: source, lines } }
    }
    fn current_line(&self) -> &str {
        let span = self.peek().span;
        self.source.line(span.line)
    }
    fn chars_at(&self, start: u32, end: u32) -> &'a str {
        &self.source.content[start as usize..end as usize]
    }
    fn chars_at_span(&self, span: Span) -> &'a str {
        self.chars_at(span.start, span.end)
    }
    fn tok_chars(&self, tok: Token) -> &'a str {
        let s = self.chars_at_span(tok.span);
        trace!("{} chars '{}'", tok.kind, s);
        s
    }

    fn eat_token(&mut self, target_token: TokenKind) -> Option<Token> {
        let tok = self.peek();
        if tok.kind == LineComment {
            self.tokens.advance();
            self.eat_token(target_token)
        } else if tok.kind == target_token {
            self.tokens.advance();
            trace!("eat_token SUCCESS '{}'", target_token);
            Some(tok)
        } else {
            trace!("eat_token MISS '{}'", target_token);
            None
        }
    }

    fn expect_eat_token(&mut self, target_token: TokenKind) -> ParseResult<()> {
        let result = self.eat_token(target_token);
        match result {
            None => {
                let actual = self.peek();
                Err(ParseError::ExpectedToken(target_token, actual, None))
            }
            Some(_) => Ok(()),
        }
    }

    fn eat_ident(&mut self) -> Option<String> {
        if let Some(t) = self.eat_token(Text) {
            let ident_slice = self.tok_chars(t);
            Some(ident_slice.to_string())
        } else {
            None
        }
    }

    fn expect_eat_ident(&mut self) -> ParseResult<String> {
        let result = self.eat_ident();
        match result {
            None => {
                let actual = self.peek();
                Err(ParseError::ExpectedToken(TokenKind::Text, actual, None))
            }
            Some(ident) => Ok(ident),
        }
    }

    fn parse_literal(&mut self) -> ParseResult<Option<Literal>> {
        let (first, second) = self.tokens.peek_two();
        trace!("parse_literal {} {}", first.kind, second.kind);
        return match (first.kind, second.kind) {
            (DoubleQuote, Text) => {
                trace!("parse_literal string");
                self.tokens.advance();
                self.tokens.advance();
                let close = self.tokens.peek();
                if close.kind == DoubleQuote {
                    self.tokens.advance();
                    let text = self.tok_chars(second);
                    let mut span = first.span;
                    span.end = close.span.end;
                    Ok(Some(Literal::String(text.to_string(), span)))
                } else {
                    Err(ParseError::ExpectedToken(DoubleQuote, close, None))
                }
            }
            (Text, _) => {
                let text = self.tok_chars(first);
                if text == "true" {
                    self.tokens.advance();
                    Ok(Some(Literal::Bool(true, first.span)))
                } else if text == "false" {
                    self.tokens.advance();
                    Ok(Some(Literal::Bool(false, first.span)))
                } else {
                    match text.chars().next() {
                        Some(c) if c.is_numeric() => {
                            let s = text.to_string();
                            self.tokens.advance();
                            Ok(Some(Literal::Numeric(s, first.span)))
                        }
                        _ => Ok(None),
                    }
                }
            }
            _ => Ok(None),
        };
    }

    fn parse_type_expression(&mut self) -> ParseResult<Option<TypeExpression>> {
        let tok = self.peek();
        if let Text = tok.kind {
            let ident = self.tok_chars(tok);
            if ident == "Int" {
                self.tokens.next();
                Ok(Some(TypeExpression::Primitive(TypePrimitive::Int)))
            } else if ident == "Bool" {
                self.tokens.next();
                Ok(Some(TypeExpression::Primitive(TypePrimitive::Bool)))
            } else {
                Err(ParseError::Msg(
                    format!("Unimplemented eat_type_expression; got {}", ident),
                    tok,
                ))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_fn_arg(&mut self) -> ParseResult<Option<FnArg>> {
        let (one, two) = self.tokens.peek_two();
        let named = if one.kind == Text && two.kind == Equals {
            self.tokens.advance();
            self.tokens.advance();
            true
        } else {
            false
        };
        match self.parse_expression() {
            Ok(Some(expr)) => {
                let name = if named { Some(self.tok_chars(one).to_string()) } else { None };
                Ok(Some(FnArg { name, value: expr }))
            }
            Ok(None) => {
                if named {
                    Err(ParseError::Msg("expected expression".to_string(), self.peek()))
                } else {
                    Ok(None)
                }
            }
            Err(e) => Err(e),
        }
    }

    fn expect_fn_arg(&mut self) -> ParseResult<FnArg> {
        let res = self.parse_fn_arg();
        Parser::expect("fn_arg", self.peek(), res)
    }

    fn parse_expression(&mut self) -> ParseResult<Option<Expression>> {
        let (first, second) = self.tokens.peek_two();
        trace!("parse_expression {} {}", first.kind, second.kind);
        let single_result = if let Some(lit) = self.parse_literal()? {
            Ok(Some(Expression::Literal(lit)))
        } else if let Text = first.kind {
            // FnCall
            if second.kind == OpenParen {
                trace!("parse_expression FnCall");
                self.tokens.advance();
                // Eat the OpenParen
                self.tokens.advance();
                match self.eat_delimited(Comma, CloseParen, |p| Parser::expect_fn_arg(p)) {
                    Ok((args, args_span)) => {
                        let mut span = first.span;
                        span.end = args_span.end + 1;
                        Ok(Some(Expression::FnCall(FnCall {
                            name: Ident(self.tok_chars(first).to_string()),
                            args,
                            span,
                        })))
                    }
                    Err(e) => Err(ParseError::ExpectedNode(
                        "function arguments".to_string(),
                        self.peek(),
                        Some(Box::new(e)),
                    )),
                }
            } else {
                // The last thing it can be is a simple variable reference expression
                self.tokens.advance();
                Ok(Some(Expression::Variable(Variable {
                    ident: Ident(self.tok_chars(first).to_string()),
                    span: first.span,
                })))
            }
        } else if first.kind == OpenBrace {
            match self.parse_block()? {
                None => Err(ParseError::ExpectedNode("block".to_string(), self.peek(), None)),
                Some(block) => Ok(Some(Expression::Block(block))),
            }
        } else {
            // TODO: Structs and Tuples
            Ok(None)
        }?;
        if let Some(expr) = single_result {
            return if self.peek().kind.is_binary_operator() {
                let op_token = self.tokens.next();
                let op_kind = BinaryOpKind::from_tokenkind(op_token.kind);
                match op_kind {
                    None => {
                        Err(ParseError::ExpectedNode("Binary Operator".to_string(), op_token, None))
                    }
                    Some(op_kind) => {
                        let operand2 = Parser::expect(
                            "rhs of binary op",
                            self.peek(),
                            self.parse_expression(),
                        )?;
                        let mut span = expr.get_span();
                        span.end = operand2.get_span().end;
                        return Ok(Some(Expression::BinaryOp(BinaryOp {
                            operation: op_kind,
                            operand1: Box::new(expr),
                            operand2: Box::new(operand2),
                            span,
                        })));
                    }
                }
            } else {
                Ok(Some(expr))
            };
        };
        Ok(single_result)
    }

    fn parse_mut(&mut self) -> ParseResult<Option<ValDef>> {
        self.parse_val(true)
    }

    fn parse_val(&mut self, mutable: bool) -> ParseResult<Option<ValDef>> {
        trace!("parse_val");
        let keyword = if mutable { KeywordMut } else { KeywordVal };
        if let Some(eaten_keyword) = self.eat_token(keyword) {
            let ident = self.expect_eat_ident()?;
            let typ = match self.eat_token(Colon) {
                None => Ok(None),
                Some(_) => self.parse_type_expression(),
            }?;
            self.expect_eat_token(Equals)?;
            let initializer_expression =
                Parser::expect("expression", self.peek(), self.parse_expression())?;
            let mut span = eaten_keyword.span;
            span.end = initializer_expression.get_span().end;
            ParseResult::Ok(Some(ValDef {
                name: Ident(ident),
                typ,
                value: initializer_expression,
                is_mutable: mutable,
                span,
            }))
        } else {
            Ok(None)
        }
    }

    fn parse_const(&mut self) -> ParseResult<Option<ConstVal>> {
        trace!("parse_const");
        let val = self.eat_token(KeywordVal);
        if val.is_none() {
            return Ok(None);
        }

        let ident = self.expect_eat_ident()?;
        let _colon = self.expect_eat_token(Colon);
        let typ = Parser::expect("type_expression", self.peek(), self.parse_type_expression())?;
        self.expect_eat_token(Equals)?;
        let value = Parser::expect("expression", self.peek(), self.parse_expression())?;
        ParseResult::Ok(Some(ConstVal { name: Ident(ident), typ, value }))
    }

    fn parse_assignment(&mut self) -> ParseResult<Option<Assignment>> {
        let (ident_token, eq) = self.tokens.peek_two();
        trace!("parse_assignment {} {}", ident_token.kind, eq.kind);
        let is_assignment = ident_token.kind == Text && eq.kind == Equals;
        if !is_assignment {
            return Ok(None);
        }
        self.tokens.advance();
        self.tokens.advance();
        let expr = Parser::expect(
            "assignment RHS expected an expression",
            self.peek(),
            self.parse_expression(),
        )?;
        let mut span = ident_token.span;
        span.end = expr.get_span().end;
        let ident = self.tok_chars(ident_token).to_string();
        Ok(Some(Assignment { ident: Ident(ident), expr, span }))
    }

    fn eat_fn_arg_def(&mut self) -> ParseResult<FnArgDef> {
        trace!("eat_fn_arg_def");
        let ident = self.expect_eat_ident()?;
        self.expect_eat_token(Colon)?;
        let typ = Parser::expect("type_expression", self.peek(), self.parse_type_expression())?;
        Ok(FnArgDef { name: Ident(ident), typ })
    }

    fn eat_fndef_args(&mut self) -> ParseResult<(Vec<FnArgDef>, Span)> {
        self.eat_delimited(Comma, CloseParen, |p| Parser::eat_fn_arg_def(p))
    }

    fn eat_delimited<T, F>(
        &mut self,
        delim: TokenKind,
        terminator: TokenKind,
        parse: F,
    ) -> ParseResult<(Vec<T>, Span)>
    where
        F: Fn(&mut Parser) -> ParseResult<T>,
    {
        trace!("eat_delimited delim='{}' terminator='{}'", delim, terminator);
        // TODO @Allocation Use smallvec
        let mut v = Vec::with_capacity(32);
        let mut span = self.peek().span;

        loop {
            if let Some(terminator) = self.eat_token(terminator) {
                trace!("eat_delimited found terminator after {} results.", v.len());
                span.end = terminator.span.end;
                break Ok((v, span));
            }
            match parse(self) {
                Ok(parsed) => {
                    v.push(parsed);
                    trace!("eat_delimited got result {}", v.len());
                    if let Some(terminator) = self.eat_token(terminator) {
                        trace!("eat_delimited found terminator after {} results.", v.len());
                        span.end = terminator.span.end;
                        break Ok((v, span));
                    }
                    let found_delim = self.eat_token(delim);
                    if found_delim.is_none() {
                        trace!("eat_delimited missing delimiter.");
                        break Err(ParseError::ExpectedToken(delim, self.peek(), None));
                    }
                }
                Err(e) => {
                    // trace!("eat_delimited got err from 'parse': {}", e);
                    break Err(ParseError::ExpectedNode(
                        "eat_delimited encountered error parsing element".to_string(),
                        self.peek(),
                        Some(Box::new(e)),
                    ));
                }
            }
        }
    }

    fn eat_if_expr(&mut self) -> ParseResult<Option<IfExpr>> {
        Ok(None)
    }

    fn parse_statement(&mut self) -> ParseResult<Option<BlockStmt>> {
        trace!("eat_statement");
        if let Some(mut_def) = self.parse_mut()? {
            Ok(Some(BlockStmt::ValDef(mut_def)))
        } else if let Some(val_def) = self.parse_val(false)? {
            Ok(Some(BlockStmt::ValDef(val_def)))
        } else if let Some(return_token) = self.eat_token(KeywordReturn) {
            if let Some(ret_val) = self.parse_expression()? {
                let mut span = return_token.span;
                span.end = ret_val.get_span().end;
                let return_stmt = ReturnStmt { expr: ret_val, span };
                Ok(Some(BlockStmt::ReturnStmt(return_stmt)))
            } else {
                Err(ParseError::ExpectedNode(
                    "return statement".to_string(),
                    self.tokens.next(),
                    None,
                ))
            }
        } else if let Some(if_expr) = self.eat_if_expr()? {
            Ok(Some(BlockStmt::If(if_expr)))
        } else if let Some(assgn) = self.parse_assignment()? {
            Ok(Some(BlockStmt::Assignment(assgn)))
        } else if let Some(expr) = self.parse_expression()? {
            Ok(Some(BlockStmt::LoneExpression(expr)))
        } else {
            Ok(None)
        }
    }

    fn parse_block(&mut self) -> ParseResult<Option<Block>> {
        if let Some(block_start) = Parser::check(self.eat_token(OpenBrace))? {
            let closure =
                |p: &mut Parser| Parser::expect("statement", p.peek(), Parser::parse_statement(p));
            let (block_statements, _statements_span) =
                self.eat_delimited(Semicolon, CloseBrace, closure)?;
            let mut span = block_start.span;
            span.end = _statements_span.end;
            Ok(Some(Block { stmts: block_statements, span }))
        } else {
            Ok(None)
        }
    }

    fn parse_fndef(&mut self) -> ParseResult<Option<FnDef>> {
        trace!("parse_fndef");
        if let Some(fn_keyword) = Parser::check(self.eat_token(KeywordFn))? {
            let ident = self.expect_eat_ident()?;
            self.expect_eat_token(OpenParen)?;
            let (args, args_span) = self.eat_fndef_args()?;
            self.expect_eat_token(Colon)?;
            let ret_type = self.parse_type_expression()?;
            let block = self.parse_block()?;
            let mut span = fn_keyword.span;
            span.end = block.as_ref().map(|b| b.span.end).unwrap_or(args_span.end);
            Ok(Some(FnDef { name: Ident(ident), args, ret_type, block, span }))
        } else {
            Ok(None)
        }
    }

    fn parse_definition(&mut self) -> ParseResult<Option<Definition>> {
        if let Some(const_def) = self.parse_const()? {
            let sem = self.eat_token(Semicolon);
            if sem.is_none() {
                return Err(ParseError::ExpectedToken(Semicolon, self.peek(), None));
            }
            Ok(Some(Definition::Const(const_def)))
        } else if let Some(fn_def) = self.parse_fndef()? {
            Ok(Some(Definition::FnDef(fn_def)))
        } else {
            Ok(None)
        }
    }

    fn eat_module(&mut self, filename: &str) -> ParseResult<Module> {
        let mut defs: Vec<Definition> = vec![];

        while let Some(def) = self.parse_definition()? {
            defs.push(def)
        }
        Ok(Module { name: Ident(filename.to_string()), defs })
    }
}

fn print_tokens(content: &str, tokens: &[Token]) {
    let mut line_idx = 0;
    for tok in tokens.iter() {
        if tok.span.line > line_idx {
            line_idx += 1;
            println!()
        }
        if tok.kind == Text {
            print!("{}", &content[tok.span.start as usize..tok.span.end as usize]);
        } else if tok.kind.is_keyword() {
            print!("{} ", tok.kind);
        } else {
            print!("{}", tok.kind);
        }
    }
    println!()
}

// Eventually I want to keep the tokens around, and return them from here somehow, either in the
// ast::Module or just separately
pub fn parse_text(text: &str, module_name: &str) -> ParseResult<Module> {
    let mut lexer = Lexer::make(text);

    let token_vec = lexer.run();

    let tokens: TokenIter = TokenIter::make(token_vec);

    let mut parser = Parser::make(tokens, text);

    parser.eat_module(module_name)
}

pub fn parse_file(path: &str) -> ParseResult<Module> {
    let file = File::open(path).unwrap_or_else(|_| panic!("file not found {}", path));
    let mut buf_read = BufReader::new(file);
    let mut content = String::new();
    buf_read.read_to_string(&mut content).expect("read failed");
    parse_text(&content, path)
}
