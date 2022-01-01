use std::cell::RefCell;
use std::fs::File;
use std::io::{BufReader, Read};

use TokenKind::*;

use crate::ast::*;
use crate::lex::*;

#[cfg(test)]
mod parse_test;

const TRACE: bool = true;

#[macro_export]
macro_rules! trace {
    () => (println!("\n"));
    ($($arg:tt)*) => ({
        if (TRACE) {
          println!($($arg)*);
        }
    })
}

pub type ParseResult<A> = Result<A, ParseError>;

#[derive(Debug)]
pub enum ParseError {
    ExpectedToken(TokenKind, Token, Option<Box<ParseError>>),
    ExpectedNode(String, Token, Option<Box<ParseError>>),
    Msg(String, Token),
}

fn parse_literal(text: &str) -> Option<Literal> {
    let num = text.parse::<i32>().ok()?;
    Some(Literal::I32(num))
}

struct Parser<'a> {
    parse_stack: RefCell<Vec<&'static str>>,
    tokens: Tokens,
    source: &'a str,
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
    fn make(tokens: Tokens, source: &'a str) -> Parser<'a> {
        Parser {
            tokens,
            source,
            parse_stack: RefCell::new(Vec::new()),
        }
    }
    fn chars_at(&self, start: usize, end: usize) -> &str {
        &self.source[start..end]
    }
    fn tok_chars(&self, tok: Token) -> &str {
        let s = self.chars_at(tok.start, tok.start + tok.len);
        trace!("tok chars {} '{}'", tok.kind, s);
        s
    }

    fn eat_token(&mut self, target_token: TokenKind) -> Option<()> {
        let tok = self.peek();
        if tok.kind == target_token {
            self.tokens.advance();
            trace!("eat_token SUCCESS '{}'", target_token);
            Some(())
        } else {
            trace!("eat_token MISS '{}'", target_token);
            None
        }
    }

    fn expect_eat_token(&mut self, target_token: TokenKind) -> Result<(), ParseError> {
        let result = self.eat_token(target_token);
        match result {
            None => {
                // We don't mind doing a repetitive `peek` in the error flow
                let actual = self.peek();
                Err(ParseError::ExpectedToken(target_token, actual, None))
            }
            Some(_) => Ok(()),
        }
    }

    fn eat_ident(&mut self) -> Option<String> {
        let tok = self.peek();
        if Text == tok.kind {
            self.tokens.advance();
            let ident = self.tok_chars(tok);
            Some(String::from(ident))
        } else {
            None
        }
    }

    fn expect_eat_ident(&mut self) -> Result<String, ParseError> {
        let result = self.eat_ident();
        match result {
            None => {
                // We don't mind doing a repetitive `peek` in the error flow
                let actual = self.peek();
                Err(ParseError::ExpectedToken(TokenKind::Text, actual, None))
            }
            Some(ident) => Ok(ident),
        }
    }

    fn parse_type_expression(&mut self) -> Result<Option<TypeExpression>, ParseError> {
        let tok = self.peek();
        if let Text = tok.kind {
            let ident = self.tok_chars(tok);
            if ident == "Int" {
                self.tokens.next();
                Ok(Some(TypeExpression::Primitive(TypePrimitive::I32)))
            } else {
                Err(ParseError::Msg(
                    format!("Unimplemented eat_type_expression; got {}", ident).to_string(),
                    tok,
                ))
            }
        } else {
            Ok(None)
        }
    }

    fn check_eat_literal(&mut self) -> Option<Literal> {
        let tok = self.peek();
        if let Text = tok.kind {
            let text = self.tok_chars(tok);
            parse_literal(text)
        } else {
            None
        }
    }

    fn parse_fn_arg(&mut self) -> ParseResult<Option<FnArg>> {
        let (one, two) = self.tokens.peek_two();
        let named;
        if one.kind == Text && two.kind == Equals {
            self.tokens.advance();
            self.tokens.advance();
            named = true;
        } else {
            named = false;
        }
        match self.parse_expression() {
            Ok(Some(expr)) => Ok(Some(FnArg {
                name: Some(self.tok_chars(one).to_string()),
                value: expr,
            })),
            Ok(None) if !named => Ok(None),
            Ok(None) => Err(ParseError::Msg(
                "expected expression".to_string(),
                self.peek(),
            )),
            Err(e) => Err(e),
        }
    }

    fn expect_fn_arg(&mut self) -> ParseResult<FnArg> {
        let res = self.parse_fn_arg();
        Parser::expect("fn_arg", self.peek(), res)
    }

    fn parse_expression(&mut self) -> ParseResult<Option<Expression>> {
        let (tok, next) = self.tokens.peek_two();
        trace!("eat_expression {} {}", tok.kind, next.kind);
        if let Text = tok.kind {
            if let Some(lit) = parse_literal(self.tok_chars(tok)) {
                self.tokens.advance();
                Ok(Some(Expression::Literal(lit)))
            } else {
                // FnCall
                if next.kind == OpenParen {
                    trace!("eat_expression FnCall");
                    self.tokens.advance();
                    // Eat the OpenParen
                    self.tokens.advance();
                    match self.eat_delimited(Comma, CloseParen, |p| Parser::expect_fn_arg(p)) {
                        Ok(args) => Ok(Some(Expression::FnCall(FnCall {
                            name: Ident(self.tok_chars(tok).to_string()),
                            args: args,
                        }))),
                        Err(e) => Err(ParseError::ExpectedNode(
                            "function arguments".to_string(),
                            self.peek(),
                            Some(Box::new(e)),
                        )),
                    }
                } else {
                    // Plain Reference
                    self.tokens.advance();
                    Ok(Some(Expression::Variable(Ident(
                        self.tok_chars(tok).to_string(),
                    ))))
                }
            }
        } else if tok.kind == OpenBrace {
            match self.parse_block()? {
                None => Err(ParseError::ExpectedNode(
                    "block".to_string(),
                    self.peek(),
                    None,
                )),
                Some(block) => Ok(Some(Expression::Block(block))),
            }
        } else {
            // TODO: Tuples
            Ok(None)
        }
    }

    fn parse_mut(&mut self) -> ParseResult<Option<MutDef>> {
        // mut foo[: Int] := 42
        let is_mut = Parser::check(self.eat_token(KeywordMut))?;
        if is_mut.is_none() {
            return Ok(None);
        }
        let ident = self.expect_eat_ident()?;
        let typ = match self.eat_token(Colon) {
            None => Ok(None),
            Some(_) => self.parse_type_expression(),
        }?;
        let _col_eq = self.expect_eat_token(Colon)?;
        let _col_eq = self.expect_eat_token(Equals)?;
        let value = Parser::expect("expression", self.peek(), self.parse_expression())?;
        return Ok(Some(MutDef {
            name: Ident(ident),
            typ: typ,
            value: value,
        }));
    }

    fn parse_val(&mut self) -> ParseResult<Option<ValDef>> {
        trace!("parse_val");
        let val = self.eat_token(KeywordVal);
        if val.is_none() {
            return Ok(None);
        }

        let ident = self.expect_eat_ident()?;
        let typ = match self.eat_token(Colon) {
            None => Ok(None),
            Some(_) => self.parse_type_expression(),
        }?;
        let _col_eq = self.expect_eat_token(Colon)?;
        let _col_eq = self.expect_eat_token(Equals)?;
        let value = Parser::expect("expression", self.peek(), self.parse_expression())?;
        return ParseResult::Ok(Some(ValDef {
            name: Ident(ident),
            typ: typ,
            value: value,
        }));
    }

    fn parse_assignment(&mut self) -> ParseResult<Option<Assignment>> {
        let (ident, col, eq) = self.tokens.peek_three();
        trace!("parse_assignment {} {} {}", ident.kind, col.kind, eq.kind);
        let is_assignment = ident.kind == Text && col.kind == Colon && eq.kind == Equals;
        if !is_assignment {
            return Ok(None);
        }
        self.tokens.advance();
        self.tokens.advance();
        self.tokens.advance();
        let ident = self.tok_chars(ident).to_string();
        let expr_result = Parser::expect(
            "assignment RHS expected an expression",
            self.peek(),
            self.parse_expression(),
        );
        expr_result.map(|expr| {
            Some(Assignment {
                ident: Ident(ident.to_string()),
                expr: expr,
            })
        })
    }

    fn eat_fn_arg_def(&mut self) -> ParseResult<FnArgDef> {
        trace!("eat_fn_arg_def");
        let ident = self.expect_eat_ident()?;
        let _colon = self.expect_eat_token(Colon)?;
        let typ = Parser::expect("type_expression", self.peek(), self.parse_type_expression())?;
        return Ok(FnArgDef {
            name: Ident(ident),
            typ: typ,
            default: None,
        });
    }

    fn eat_fndef_args(&mut self) -> ParseResult<Vec<FnArgDef>> {
        self.eat_delimited(Comma, CloseParen, |p| Parser::eat_fn_arg_def(p))
    }

    fn eat_delimited<T, F>(
        &mut self,
        delim: TokenKind,
        terminator: TokenKind,
        parse: F,
    ) -> ParseResult<Vec<T>>
    where
        F: Fn(&mut Parser) -> ParseResult<T>,
    {
        trace!(
            "eat_delimited delim='{}' terminator='{}'",
            delim,
            terminator
        );
        // TODO @Allocation Need to figure out how we use all the small vecs without allocating all the time wastefully
        let mut v = Vec::with_capacity(32);
        loop {
            if self.eat_token(terminator).is_some() {
                trace!("eat_delimited found terminator after {} results.", v.len());
                break Ok(v);
            }
            match parse(self) {
                Ok(parsed) => {
                    v.push(parsed);
                    trace!("eat_delimited got result {}", v.len());
                    if self.eat_token(terminator).is_some() {
                        trace!("eat_delimited found terminator after {} results.", v.len());
                        break Ok(v);
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
            Ok(Some(BlockStmt::MutDef(mut_def)))
        } else if let Some(val_def) = self.parse_val()? {
            Ok(Some(BlockStmt::ValDef(val_def)))
        } else if self.eat_token(KeywordReturn).is_some() {
            if let Some(ret_val) = self.parse_expression()? {
                Ok(Some(BlockStmt::ReturnStmt(ret_val)))
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
        let _block_start = Parser::check(self.eat_token(OpenBrace))?;
        let closure =
            |p: &mut Parser| Parser::expect("statement", p.peek(), Parser::parse_statement(p));
        let block_statements = self.eat_delimited(Semicolon, CloseBrace, closure)?;
        Ok(Some(Block {
            stmts: block_statements,
        }))
    }

    fn parse_fndef(&mut self) -> ParseResult<Option<FnDef>> {
        trace!("parse_fndef");
        let _fn = Parser::check(self.eat_token(KeywordFn))?;
        let ident = self.expect_eat_ident()?;
        let _open_paren = self.expect_eat_token(OpenParen)?;
        let args = self.eat_fndef_args()?;
        let _colon = self.expect_eat_token(Colon)?;
        let ret_type = self.parse_type_expression()?;
        let block = self.parse_block()?;
        Ok(Some(FnDef {
            name: Ident(ident),
            args,
            ret_type: ret_type,
            type_args: None,
            block: block,
        }))
    }

    fn parse_definition(&mut self) -> Result<Option<Definition>, ParseError> {
        let val_result = self.parse_val();
        if let Some(val_def) = val_result? {
            Ok(Some(Definition::ValDef(val_def)))
        } else if let Some(fn_def) = self.parse_fndef()? {
            Ok(Some(Definition::FnDef(fn_def)))
        } else {
            Ok(None)
        }
    }

    fn eat_module(&mut self) -> Result<Module, String> {
        let mut defs: Vec<Definition> = vec![];

        while let Ok(Some(def)) = self.parse_definition() {
            defs.push(def)
        }
        Ok(Module {
            name: Ident("module1".to_string()),
            defs: defs,
        })
    }
}

fn print_tokens(content: &str, tokens: &[Token]) {
    let mut line_idx = 0;
    for tok in tokens.iter() {
        if tok.line_num > line_idx {
            line_idx += 1;
            println!()
        }
        if tok.kind == Text {
            print!("{}", &content[tok.start..tok.start + tok.len]);
        } else if tok.kind.is_keyword() {
            print!("{} ", tok.kind);
        } else {
            print!("{}", tok.kind);
        }
    }
    println!()
}

pub fn parse_text(text: &str) -> Result<Module, String> {
    let mut lexer = Lexer::make(&text);

    let token_vec = tokenize(&mut lexer);
    print_tokens(&text, &token_vec);

    // TODO @Allocation: Just RC the original tokens so everything can look at them
    let tokens: Tokens = Tokens::make(token_vec.clone());

    let mut parser = Parser::make(tokens, &text);

    let module = parser.eat_module();

    module
}

pub fn parse_file(path: &str) -> Result<Module, String> {
    let file = File::open(path).expect(&format!("file not found {}", path));
    let mut buf_read = BufReader::new(file);
    let mut content = String::new();
    buf_read.read_to_string(&mut content).expect("read failed");
    parse_text(&content)
}
