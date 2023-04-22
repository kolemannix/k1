use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{BufReader, Read};

use TokenKind::*;

use crate::lex::*;
use crate::trace;

#[cfg(test)]
mod parse_test;

#[derive(Debug)]
pub enum Literal {
    Numeric(String, Span),
    Bool(bool, Span),
    String(String, Span),
}

impl Literal {
    pub fn get_span(&self) -> Span {
        match self {
            Literal::Numeric(_, span) => *span,
            Literal::Bool(_, span) => *span,
            Literal::String(_, span) => *span,
        }
    }
}

#[derive(Debug)]
pub struct Ident(pub String);

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

#[derive(Debug)]
pub struct FnArg {
    pub name: Option<String>,
    pub value: Expression,
}

#[derive(Debug)]
pub struct FnCall {
    pub name: Ident,
    pub args: Vec<FnArg>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ValDef {
    pub name: Ident,
    pub ty: Option<TypeExpression>,
    pub value: Expression,
    pub is_mutable: bool,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinaryOpKind {
    Add,
    Multiply,
    And,
    Or,
}

impl BinaryOpKind {
    pub fn from_tokenkind(kind: TokenKind) -> Option<BinaryOpKind> {
        match kind {
            TokenKind::Plus => Some(BinaryOpKind::Add),
            TokenKind::Asterisk => Some(BinaryOpKind::Multiply),
            TokenKind::KeywordAnd => Some(BinaryOpKind::And),
            TokenKind::KeywordOr => Some(BinaryOpKind::Or),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct BinaryOp {
    pub operation: BinaryOpKind,
    pub operand1: Box<Expression>,
    pub operand2: Box<Expression>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Variable {
    pub ident: Ident,
    pub span: Span,
}

#[derive(Debug)]
pub struct FieldAccess {
    pub base: Box<Expression>,
    pub target: Ident,
    pub span: Span,
}

#[derive(Debug)]
pub struct RecordField {
    pub name: Ident,
    pub expr: Expression,
}

#[derive(Debug)]
pub struct Record {
    pub fields: Vec<RecordField>,
    pub span: Span,
}

#[derive(Debug)]
pub enum Expression {
    BinaryOp(BinaryOp),
    Literal(Literal),
    FnCall(FnCall),
    Variable(Variable),
    FieldAccess(FieldAccess),
    Block(Block),
    If(IfExpr),
    Record(Record),
}

impl Expression {
    pub fn is_literal(e: &Expression) -> bool {
        matches!(e, Expression::Literal(_))
    }
    pub fn get_span(&self) -> Span {
        match self {
            Expression::BinaryOp(op) => op.span,
            Expression::Literal(lit) => lit.get_span(),
            Expression::FnCall(call) => call.span,
            Expression::Variable(var) => var.span,
            Expression::FieldAccess(acc) => acc.span,
            Expression::Block(block) => block.span,
            Expression::If(if_expr) => if_expr.span,
            Expression::Record(record) => record.span,
        }
    }
}

#[derive(Debug)]
pub struct Assignment {
    pub ident: Ident,
    pub expr: Expression,
    pub span: Span,
}

#[derive(Debug)]
pub struct IfExpr {
    pub cond: Box<Expression>,
    pub cons: Box<Expression>,
    // TODO: Add 'binding' Ifs, for optionals and failures
    // if some_optional { value => }
    // if get_file() { result => }
    pub alt: Option<Box<Expression>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub expr: Expression,
    pub span: Span,
}

#[derive(Debug)]
pub enum BlockStmt {
    ValDef(ValDef),
    /// return keyword will only be allowed to denote explicit early returns
    ReturnStmt(ReturnStmt),
    Assignment(Assignment),
    LoneExpression(Expression),
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<BlockStmt>,
    pub span: Span,
}

#[derive(Debug)]
pub struct RecordTypeField {
    pub name: Ident,
    pub ty: TypeExpression,
}

#[derive(Debug)]
pub struct RecordType {
    pub fields: Vec<RecordTypeField>,
    pub span: Span,
}

#[derive(Debug)]
pub enum TypeExpression {
    Int(Span),
    Bool(Span),
    Record(RecordType),
    Name(Ident, Span),
}

impl TypeExpression {
    #[inline]
    pub fn get_span(&self) -> Span {
        match self {
            TypeExpression::Int(span) => *span,
            TypeExpression::Bool(span) => *span,
            TypeExpression::Record(record) => record.span,
            TypeExpression::Name(_, span) => *span,
        }
    }
}

#[derive(Debug)]
pub struct FnDef {
    pub name: Ident,
    pub args: Vec<FnArgDef>,
    pub ret_type: Option<TypeExpression>,
    pub block: Option<Block>,
    pub span: Span,
}

#[derive(Debug)]
pub struct FnArgDef {
    pub name: Ident,
    pub ty: TypeExpression,
}

#[derive(Debug)]
pub struct ConstVal {
    pub name: Ident,
    pub ty: TypeExpression,
    pub value_expr: Expression,
    pub span: Span,
}

#[derive(Debug)]
pub struct TypeDefn {
    pub name: Ident,
    pub value_expr: TypeExpression,
    pub span: Span,
}

#[derive(Debug)]
pub enum Definition {
    FnDef(FnDef),
    Const(ConstVal),
    Type(TypeDefn),
}

#[derive(Debug)]
pub struct Module {
    pub name: Ident,
    pub defs: Vec<Definition>,
}

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
        // FIXME: The way we handle line comments is broken
        // It works OK here, but we do a lot of peeking at the next 1-3 tokens
        // If any are line comments, that peeking code will not produce
        // the correct result!
        // Instead, we should probably filter the comments out after lexing
        // Since the spans will remain correct
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

    fn expect_eat_token(&mut self, target_token: TokenKind) -> ParseResult<Token> {
        let result = self.eat_token(target_token);
        match result {
            None => {
                let actual = self.peek();
                Err(ParseError::ExpectedToken(target_token, actual, None))
            }
            Some(t) => Ok(t),
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

    fn parse_record_type_field(&mut self) -> ParseResult<Option<RecordTypeField>> {
        let ident = self.expect_eat_ident()?;
        self.expect_eat_token(Colon);
        let typ_expr =
            Parser::expect("Type expression", self.peek(), self.parse_type_expression())?;
        Ok(Some(RecordTypeField { name: Ident(ident), ty: typ_expr }))
    }

    fn parse_type_expression(&mut self) -> ParseResult<Option<TypeExpression>> {
        let tok = self.peek();
        if tok.kind == Text {
            let ident = self.tok_chars(tok);
            if ident == "Int" {
                self.tokens.advance();
                Ok(Some(TypeExpression::Int(tok.span)))
            } else if ident == "Bool" {
                self.tokens.advance();
                Ok(Some(TypeExpression::Bool(tok.span)))
            } else {
                self.tokens.advance();
                Ok(Some(TypeExpression::Name(Ident(ident.to_string()), tok.span)))
            }
        } else if tok.kind == OpenBrace {
            let open_brace = self.expect_eat_token(OpenBrace)?;
            let (fields, fields_span) = self.eat_delimited(Comma, CloseBrace, |p| {
                let field_res = Parser::parse_record_type_field(p);
                Parser::expect("Record Field", open_brace, field_res)
            })?;
            let mut record_span = tok.span;
            record_span.end = fields_span.end;
            let record = RecordType { fields, span: record_span };
            Ok(Some(TypeExpression::Record(record)))
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

    fn parse_record(&mut self) -> ParseResult<Option<Record>> {
        let Some(open_brace) = self.eat_token(OpenBrace) else {
            return Ok(None);
        };
        let (fields, fields_span) = self.eat_delimited(Comma, CloseBrace, |parser| {
            let name = parser.expect_eat_ident()?;
            parser.expect_eat_token(Colon)?;
            let expr = Parser::expect("expression", parser.peek(), parser.parse_expression())?;
            Ok(RecordField { name: Ident(name), expr })
        })?;
        let mut span = open_brace.span;
        span.end = fields_span.end;
        Ok(Some(Record { fields, span }))
    }

    fn parse_expression_postfix_op(&mut self, expr: Expression) -> ParseResult<Expression> {
        let mut result = expr;
        // Looping for postfix ops inspired by Jakt's parser
        loop {
            let next = self.peek();
            if next.kind.is_postfix_operator() {
                if next.kind == Dot {
                    // Field access syntax; a.b
                    self.tokens.advance();
                    let target_ident = self.expect_eat_ident()?;
                    let next = self.peek();
                    if next.kind == OpenParen {
                        // Method call
                        todo!("method call parsing")
                    } else {
                        let span = result.get_span().extended(&next.span);
                        result = Expression::FieldAccess(FieldAccess {
                            base: Box::new(result),
                            target: Ident(target_ident),
                            span,
                        })
                    }
                }
            } else {
                return Ok(result);
            }
        }
    }

    fn parse_expression_binary_op(&mut self, expr: Expression) -> ParseResult<Expression> {
        if self.peek().kind.is_binary_operator() {
            let op_token = self.tokens.next();
            let op_kind = BinaryOpKind::from_tokenkind(op_token.kind);
            match op_kind {
                None => {
                    Err(ParseError::ExpectedNode("Binary Operator".to_string(), op_token, None))
                }
                Some(op_kind) => {
                    let operand2 =
                        Parser::expect("rhs of binary op", self.peek(), self.parse_expression())?;
                    let mut span = expr.get_span();
                    span.end = operand2.get_span().end;
                    return Ok(Expression::BinaryOp(BinaryOp {
                        operation: op_kind,
                        operand1: Box::new(expr),
                        operand2: Box::new(operand2),
                        span,
                    }));
                }
            }
        } else {
            Ok(expr)
        }
    }

    fn parse_expression(&mut self) -> ParseResult<Option<Expression>> {
        let (first, second, third) = self.tokens.peek_three();
        trace!("parse_expression {} {}", first.kind, second.kind);
        let parsed_expression = if let Some(lit) = self.parse_literal()? {
            Ok(Some(Expression::Literal(lit)))
        } else if first.kind == Text {
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
            // The syntax {} means empty record, not empty block
            // If you want a void or empty block, the required syntax is { () }
            trace!("parse_expr {:?} {:?} {:?}", first, second, third);
            if second.kind == CloseBrace {
                let mut span = first.span;
                span.end = second.span.end;
                Ok(Some(Expression::Record(Record { fields: vec![], span })))
            } else if second.kind == Text && third.kind == Colon {
                let record = Parser::expect("record", first, self.parse_record())?;
                Ok(Some(Expression::Record(record)))
            } else {
                match self.parse_block()? {
                    None => Err(ParseError::ExpectedNode("block".to_string(), self.peek(), None)),
                    Some(block) => Ok(Some(Expression::Block(block))),
                }
            }
        } else if first.kind == KeywordIf {
            let if_expr = Parser::expect("If Expression", first, self.parse_if_expr())?;
            Ok(Some(Expression::If(if_expr)))
        } else {
            // More expression types
            Ok(None)
        }?;
        // Forward search for binary and postfix ops
        if let Some(expr) = parsed_expression {
            let result = self.parse_expression_postfix_op(expr)?;
            let result = self.parse_expression_binary_op(result)?;
            Ok(Some(result))
        } else {
            Ok(None)
        }
    }

    fn parse_mut(&mut self) -> ParseResult<Option<ValDef>> {
        self.parse_val(true)
    }

    fn parse_val(&mut self, mutable: bool) -> ParseResult<Option<ValDef>> {
        trace!("parse_val");
        let keyword = if mutable { KeywordMut } else { KeywordVal };
        let Some(eaten_keyword) = self.eat_token(keyword) else {
            return Ok(None);
        };
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
        Ok(Some(ValDef {
            name: Ident(ident),
            ty: typ,
            value: initializer_expression,
            is_mutable: mutable,
            span,
        }))
    }

    fn parse_const(&mut self) -> ParseResult<Option<ConstVal>> {
        trace!("parse_const");
        let Some(keyword_val_token) = self.eat_token(KeywordVal) else {
            return Ok(None);
        };
        let ident = self.expect_eat_ident()?;
        let _colon = self.expect_eat_token(Colon);
        let typ = Parser::expect("type_expression", self.peek(), self.parse_type_expression())?;
        self.expect_eat_token(Equals)?;
        let value_expr = Parser::expect("expression", self.peek(), self.parse_expression())?;
        let mut span = keyword_val_token.span;
        span.end = value_expr.get_span().end;
        ParseResult::Ok(Some(ConstVal { name: Ident(ident), ty: typ, value_expr, span }))
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
        Ok(FnArgDef { name: Ident(ident), ty: typ })
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
                        format!("eat_delimited for {delim} encountered error parsing element"),
                        self.peek(),
                        Some(Box::new(e)),
                    ));
                }
            }
        }
    }

    fn parse_if_expr(&mut self) -> ParseResult<Option<IfExpr>> {
        if let Some(if_keyword) = self.eat_token(TokenKind::KeywordIf) {
            let condition_expr =
                Parser::expect("conditional expression", if_keyword, self.parse_expression())?;
            let consequent_expr =
                Parser::expect("block following condition", if_keyword, self.parse_expression())?;
            let else_peek = self.peek();
            let alt = if else_peek.kind == KeywordElse {
                self.tokens.advance();
                let alt_result = Parser::expect("else block", else_peek, self.parse_expression())?;
                Some(Box::new(alt_result))
            } else {
                None
            };
            let mut span = if_keyword.span;
            span.end =
                alt.as_ref().map(|a| a.get_span().end).unwrap_or(consequent_expr.get_span().end);
            let if_expr =
                IfExpr { cond: condition_expr.into(), cons: consequent_expr.into(), alt, span };
            Ok(Some(if_expr))
        } else {
            Ok(None)
        }
    }

    fn parse_statement(&mut self) -> ParseResult<Option<BlockStmt>> {
        trace!("eat_statement {:?}", self.peek());
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
        } else if let Some(assgn) = self.parse_assignment()? {
            Ok(Some(BlockStmt::Assignment(assgn)))
        } else if let Some(expr) = self.parse_expression()? {
            Ok(Some(BlockStmt::LoneExpression(expr)))
        } else {
            Ok(None)
        }
    }

    fn parse_block(&mut self) -> ParseResult<Option<Block>> {
        let Some(block_start) = Parser::check(self.eat_token(OpenBrace))? else {
            return Ok(None);
        };
        let closure =
            |p: &mut Parser| Parser::expect("statement", p.peek(), Parser::parse_statement(p));
        let (block_statements, statements_span) =
            self.eat_delimited(Semicolon, CloseBrace, closure)?;
        let mut span = block_start.span;
        span.end = statements_span.end;
        Ok(Some(Block { stmts: block_statements, span }))
    }

    fn parse_function(&mut self) -> ParseResult<Option<FnDef>> {
        trace!("parse_fndef");
        let Some(fn_keyword) = Parser::check(self.eat_token(KeywordFn))? else {
            return Ok(None);
        };
        let ident = self.expect_eat_ident()?;
        self.expect_eat_token(OpenParen)?;
        let (args, args_span) = self.eat_fndef_args()?;
        self.expect_eat_token(Colon)?;
        let ret_type = self.parse_type_expression()?;
        let block = self.parse_block()?;
        let mut span = fn_keyword.span;
        span.end = block.as_ref().map(|b| b.span.end).unwrap_or(args_span.end);
        Ok(Some(FnDef { name: Ident(ident), args, ret_type, block, span }))
    }

    fn parse_typedef(&mut self) -> ParseResult<Option<TypeDefn>> {
        let keyword_type = self.eat_token(KeywordType);
        if let Some(keyword_type) = keyword_type {
            let ident = self.expect_eat_ident()?;
            let equals = self.expect_eat_token(Equals)?;
            let type_expr =
                Parser::expect("Type expression", equals, self.parse_type_expression())?;
            let mut span = keyword_type.span;
            span.end = type_expr.get_span().end;
            Ok(Some(TypeDefn { name: Ident(ident), value_expr: type_expr, span }))
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
        } else if let Some(fn_def) = self.parse_function()? {
            Ok(Some(Definition::FnDef(fn_def)))
        } else if let Some(type_def) = self.parse_typedef()? {
            Ok(Some(Definition::Type(type_def)))
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
