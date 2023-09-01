use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use string_interner::Symbol;

use TokenKind::*;

use crate::ir::IntrinsicFunctionType;
use crate::lex::*;

pub type AstId = u32;
use log::trace;

#[cfg(test)]
mod parse_test;

#[derive(Debug)]
pub struct ArrayExpr {
    pub elements: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug)]
pub enum Literal {
    Unit(Span),
    Numeric(String, Span),
    Bool(bool, Span),
    String(String, Span),
}

impl Literal {
    pub fn get_span(&self) -> Span {
        match self {
            Literal::Unit(span) => *span,
            Literal::Numeric(_, span) => *span,
            Literal::Bool(_, span) => *span,
            Literal::String(_, span) => *span,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct IdentifierId(string_interner::symbol::SymbolU32);

impl From<IdentifierId> for usize {
    fn from(value: IdentifierId) -> Self {
        value.0.to_usize()
    }
}

impl Display for IdentifierId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.to_usize())
    }
}

// We use the default StringInterner, which uses a contiguous string as its backend
// and u32 symbols
#[derive(Debug, Default)]
pub struct Identifiers {
    intern_pool: string_interner::StringInterner,
}
impl Identifiers {
    pub fn intern(&mut self, s: impl AsRef<str>) -> IdentifierId {
        let s = self.intern_pool.get_or_intern(&s);
        IdentifierId(s)
    }
    pub fn get_name(&self, id: IdentifierId) -> &str {
        self.intern_pool.resolve(id.0).expect("failed to resolve identifier")
    }
}

#[derive(Debug)]
pub struct FnArg {
    pub name: Option<IdentifierId>,
    pub value: Expression,
}

#[derive(Debug)]
pub struct FnTypeArg {
    pub name: Option<IdentifierId>,
    pub value: TypeExpression,
}

#[derive(Debug)]
pub struct FnCall {
    pub name: IdentifierId,
    pub type_args: Option<Vec<FnTypeArg>>,
    pub args: Vec<FnArg>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ValDef {
    pub name: IdentifierId,
    pub ty: Option<TypeExpression>,
    pub value: Expression,
    pub is_mutable: bool,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum BinaryOpKind {
    Add,
    Multiply,
    And,
    Or,
    Equals,
}

impl BinaryOpKind {
    pub fn precedence(&self) -> usize {
        match self {
            BinaryOpKind::Add => 0,
            BinaryOpKind::Multiply => 1,
            BinaryOpKind::And => 1,
            BinaryOpKind::Or => 1,
            BinaryOpKind::Equals => 2,
        }
    }
    pub fn from_tokenkind(kind: TokenKind) -> Option<BinaryOpKind> {
        match kind {
            TokenKind::Plus => Some(BinaryOpKind::Add),
            TokenKind::Asterisk => Some(BinaryOpKind::Multiply),
            TokenKind::KeywordAnd => Some(BinaryOpKind::And),
            TokenKind::KeywordOr => Some(BinaryOpKind::Or),
            TokenKind::EqualsEquals => Some(BinaryOpKind::Equals),
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
    pub ident: IdentifierId,
    pub span: Span,
}

#[derive(Debug)]
pub struct FieldAccess {
    pub base: Box<Expression>,
    pub target: IdentifierId,
    pub span: Span,
}

#[derive(Debug)]
pub struct RecordField {
    pub name: IdentifierId,
    pub expr: Expression,
}

#[derive(Debug)]
/// Example:
/// { foo: 1, bar: false }
///   ^................^ fields
pub struct Record {
    pub fields: Vec<RecordField>,
    pub span: Span,
}

#[derive(Debug)]
/// Example: users  [42]
///          ^target ^index_value
pub struct IndexOperation {
    pub target: Box<Expression>,
    pub index_expr: Box<Expression>,
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
    IndexOperation(IndexOperation),
    Array(ArrayExpr),
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
            Expression::IndexOperation(op) => op.span,
            Expression::Array(array_expr) => array_expr.span,
        }
    }
}

#[derive(Debug)]
pub struct Assignment {
    pub ident: IdentifierId,
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
    pub name: IdentifierId,
    pub ty: TypeExpression,
}

#[derive(Debug)]
pub struct RecordType {
    pub fields: Vec<RecordTypeField>,
    pub span: Span,
}

#[derive(Debug)]
pub struct TypeApplication {
    pub base: IdentifierId,
    pub params: Vec<TypeExpression>,
    pub span: Span,
}

#[derive(Debug)]
pub enum TypeExpression {
    Unit(Span),
    Int(Span),
    Bool(Span),
    Record(RecordType),
    Name(IdentifierId, Span),
    TypeApplication(TypeApplication),
}

impl TypeExpression {
    #[inline]
    pub fn get_span(&self) -> Span {
        match self {
            TypeExpression::Unit(span) => *span,
            TypeExpression::Int(span) => *span,
            TypeExpression::Bool(span) => *span,
            TypeExpression::Record(record) => record.span,
            TypeExpression::Name(_, span) => *span,
            TypeExpression::TypeApplication(app) => app.span,
        }
    }
}

#[derive(Debug)]
pub struct TypeParamDef {
    pub ident: IdentifierId,
    pub span: Span,
}

#[derive(Debug)]
pub struct FnDef {
    pub name: IdentifierId,
    pub type_args: Option<Vec<TypeParamDef>>,
    pub args: Vec<FnArgDef>,
    pub ret_type: Option<TypeExpression>,
    pub block: Option<Block>,
    pub span: Span,
    // TODO: bitflags
    pub is_intrinsic: bool,
}

#[derive(Debug)]
pub struct FnArgDef {
    pub name: IdentifierId,
    pub ty: TypeExpression,
}

#[derive(Debug)]
pub struct ConstVal {
    pub name: IdentifierId,
    pub ty: TypeExpression,
    pub value_expr: Expression,
    pub span: Span,
}

#[derive(Debug)]
pub struct TypeDefn {
    pub name: IdentifierId,
    pub value_expr: TypeExpression,
    pub span: Span,
}

#[derive(Debug)]
pub enum Definition {
    FnDef(Box<FnDef>),
    Const(Box<ConstVal>),
    Type(Box<TypeDefn>),
}

#[derive(Debug)]
pub struct AstModule {
    pub name: String,
    pub name_id: IdentifierId,
    pub defs: Vec<Definition>,
    pub source: Rc<Source>,
    /// Using RefCell here just so we can mutably access
    /// the identifiers without having mutable access to
    /// the entire AST module. Lets me wait to decide
    /// where things actually live
    pub identifiers: Rc<RefCell<Identifiers>>,
}

impl AstModule {
    pub fn ident_id(&self, ident: &str) -> IdentifierId {
        self.identifiers.borrow_mut().intern(ident)
    }
    pub fn get_ident_name(&self, id: IdentifierId) -> impl std::ops::Deref<Target = str> + '_ {
        std::cell::Ref::map(self.identifiers.borrow(), |idents| idents.get_name(id))
    }

    pub fn get_defn(&self, ast_id: AstId) -> &Definition {
        &self.defs[ast_id as usize]
    }

    pub fn defns_iter(&self) -> impl Iterator<Item = (AstId, &Definition)> {
        self.defs.iter().enumerate().map(|(idx, def)| (idx as u32, def))
    }
}

pub type ParseResult<A> = Result<A, ParseError>;

#[derive(Debug)]
pub struct ParseError {
    expected: String,
    token: Token,
    cause: Option<Box<ParseError>>,
}

impl ParseError {
    pub fn span(&self) -> Span {
        self.token.span
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}
impl std::error::Error for ParseError {}

#[derive(Debug)]
pub struct Source {
    pub content: String,
    /// This is an inefficient copy but we need the lines cached because utf8
    /// Eventually it can be references not copies
    pub lines: Vec<String>,
}

impl Source {
    pub fn get_span_content(&self, span: Span) -> &str {
        &self.content[span.start as usize..span.end as usize]
    }

    pub fn get_line_by_index(&self, line_index: u32) -> &str {
        &self.lines[line_index as usize]
    }
}

struct Parser {
    tokens: TokenIter,
    source: Rc<Source>,
    identifiers: Rc<RefCell<Identifiers>>,
}

impl Parser {
    pub fn ident_id(&mut self, s: impl AsRef<str>) -> IdentifierId {
        self.identifiers.borrow_mut().intern(s.as_ref())
    }
    pub fn get_ident_name(&self, id: IdentifierId) -> impl std::ops::Deref<Target = str> + '_ {
        std::cell::Ref::map(self.identifiers.borrow(), |idents| idents.get_name(id))
    }

    pub fn print_error(&self, parse_error: &ParseError) {
        let span = parse_error.span();
        let line_text = self.source.get_line_by_index(parse_error.span().line);
        let span_text = &self.source.get_span_content(span);
        use colored::*;
        println!(
            "{} on line {}. Expected '{}', but got '{}'",
            "parse error".red(),
            span.line,
            parse_error.expected.blue(),
            parse_error.token.kind.as_ref().red()
        );
        println!();
        println!("{line_text}");
        println!("{span_text}");
    }
    // pub fn get_span_line(&self, span: Span) -> &str {
    //     &self.source.get_line_by_index(span.line)
    // }

    fn check<A>(value: Option<A>) -> ParseResult<Option<A>> {
        match value {
            None => Ok(None),
            Some(a) => Ok(Some(a)),
        }
    }
    fn expect<A>(what: &str, current: Token, value: ParseResult<Option<A>>) -> ParseResult<A> {
        match value {
            Ok(None) => Err(ParseError { expected: what.to_string(), token: current, cause: None }),
            Ok(Some(a)) => Ok(a),
            Err(e) => Err(e),
        }
    }
}

impl Parser {
    fn peek(&self) -> Token {
        self.tokens.peek()
    }
    fn make(tokens: TokenIter, source: String, _use_prelude: bool) -> Parser {
        let lines: Vec<_> = source.lines().map(|l| l.to_owned()).collect();
        Parser {
            tokens,
            source: Rc::new(Source { content: source, lines }),
            identifiers: Rc::new(RefCell::new(Identifiers::default())),
        }
    }
    fn chars_at(&self, start: u32, end: u32) -> &str {
        &self.source.content[start as usize..end as usize]
    }
    fn chars_at_span(&self, span: Span) -> &str {
        self.chars_at(span.start, span.end)
    }
    fn tok_chars(&self, tok: Token) -> &str {
        let s = self.chars_at_span(tok.span);
        trace!("{} chars '{}'", tok.kind, s);
        s
    }
    // not a member fn; doesn't borrow all of self. <3 Rust
    fn tok_chars_noself(source: &str, tok: Token) -> &str {
        &source[tok.span.start as usize..tok.span.end as usize]
    }

    fn eat_token(&mut self, target_token: TokenKind) -> Option<Token> {
        let tok = self.peek();
        // FIXME: The way we handle line comments is broken
        // It works OK here, but we do a lot of peeking at the next 1-3 tokens
        // If any are line comments, that peeking code will not produce
        // the correct result!
        // Instead, we need to filter the comments out after lexing
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

    fn error(expected: impl AsRef<str>, token: Token) -> ParseError {
        ParseError { expected: expected.as_ref().to_owned(), token, cause: None }
    }
    fn error_cause(expected: impl AsRef<str>, token: Token, cause: ParseError) -> ParseError {
        ParseError { expected: expected.as_ref().to_owned(), token, cause: Some(Box::new(cause)) }
    }

    fn expect_eat_token(&mut self, target_token: TokenKind) -> ParseResult<Token> {
        let result = self.eat_token(target_token);
        match result {
            None => {
                let actual = self.peek();
                return Err(Parser::error(target_token, actual));
            }
            Some(t) => Ok(t),
        }
    }

    fn intern_ident_token(&mut self, token: Token) -> IdentifierId {
        let source = self.source.clone();
        let tok_chars = Source::get_span_content(&source, token.span);
        self.ident_id(tok_chars)
    }

    fn parse_literal(&mut self) -> ParseResult<Option<Literal>> {
        let (first, second) = self.tokens.peek_two();
        trace!("parse_literal {} {}", first.kind, second.kind);
        return match (first.kind, second.kind) {
            (OpenParen, CloseParen) => {
                trace!("parse_literal unit");
                let span = first.span.extended(second.span);
                self.tokens.advance();
                self.tokens.advance();
                Ok(Some(Literal::Unit(span)))
            }
            (DoubleQuote, Text) => {
                trace!("parse_literal string");
                self.tokens.advance();
                self.tokens.advance();
                let close = self.tokens.peek();
                if close.kind == DoubleQuote {
                    self.tokens.advance();
                    let text = self.tok_chars(second);
                    let span = first.span.extended(close.span);
                    Ok(Some(Literal::String(text.to_string(), span)))
                } else {
                    Err(Parser::error(DoubleQuote, close))
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
                        Some(c) if c.is_numeric() || c == '-' => {
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
        let name_token = self.expect_eat_token(Text)?;
        let ident_id = self.intern_ident_token(name_token);
        self.expect_eat_token(Colon)?;
        let typ_expr =
            Parser::expect("Type expression", self.peek(), self.parse_type_expression())?;
        Ok(Some(RecordTypeField { name: ident_id, ty: typ_expr }))
    }

    fn expect_type_expression(&mut self) -> ParseResult<TypeExpression> {
        Parser::expect("type_expression", self.peek(), self.parse_type_expression())
    }

    fn parse_type_expression(&mut self) -> ParseResult<Option<TypeExpression>> {
        let tok = self.peek();
        if tok.kind == Text {
            let source = self.source.clone();
            let text_str = Source::get_span_content(&source, tok.span);
            if text_str == "unit" {
                self.tokens.advance();
                Ok(Some(TypeExpression::Unit(tok.span)))
            } else if text_str == "int" {
                self.tokens.advance();
                Ok(Some(TypeExpression::Int(tok.span)))
            } else if text_str == "bool" {
                self.tokens.advance();
                Ok(Some(TypeExpression::Bool(tok.span)))
            } else {
                self.tokens.advance();
                let next = self.tokens.peek();
                if next.kind == OpenBracket {
                    // parameterized type: Dict[int, int]
                    self.tokens.advance();
                    let (type_parameters, params_span) =
                        self.eat_delimited(Comma, CloseBracket, |p| {
                            Parser::expect_type_expression(p)
                        })?;
                    let ident = self.intern_ident_token(tok);
                    Ok(Some(TypeExpression::TypeApplication(TypeApplication {
                        base: ident,
                        params: type_parameters,
                        span: tok.span.extended(params_span),
                    })))
                } else {
                    Ok(Some(TypeExpression::Name(self.ident_id(text_str), tok.span)))
                }
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
                let name = if named { Some(self.intern_ident_token(one)) } else { None };
                Ok(Some(FnArg { name, value: expr }))
            }
            Ok(None) => {
                if named {
                    Err(Parser::error("expression", self.peek()))
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
            let name = parser.expect_eat_token(Text)?;
            parser.expect_eat_token(Colon)?;
            let expr = Parser::expect("expression", parser.peek(), parser.parse_expression())?;
            Ok(RecordField { name: parser.intern_ident_token(name), expr })
        })?;
        let span = open_brace.span.extended(fields_span);
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
                    let target = self.expect_eat_token(Text)?;
                    let next = self.peek();
                    if next.kind == OpenParen {
                        // Method call
                        todo!("method call parsing")
                    } else {
                        let span = result.get_span().extended(next.span);
                        result = Expression::FieldAccess(FieldAccess {
                            base: Box::new(result),
                            target: self.intern_ident_token(target),
                            span,
                        })
                    }
                }
                if next.kind == OpenBracket {
                    self.tokens.advance();
                    let index_expr = Parser::expect(
                        "expression inside []",
                        self.peek(),
                        self.parse_expression(),
                    )?;
                    let close = self.expect_eat_token(CloseBracket)?;
                    let span = result.get_span().extended(close.span);
                    result = Expression::IndexOperation(IndexOperation {
                        target: Box::new(result),
                        index_expr: Box::new(index_expr),
                        span,
                    })
                }
            } else {
                return Ok(result);
            }
        }
    }

    fn parse_expression_binary_op(&mut self, expr: Expression) -> ParseResult<Expression> {
        let mut result = expr;
        loop {
            if self.peek().kind.is_binary_operator() {
                let op_token = self.tokens.next();
                let Some(op_kind) = BinaryOpKind::from_tokenkind(op_token.kind) else {
                    return Err(Parser::error("Binary Operator", op_token))
                };
                let next_expr =
                    Parser::expect("rhs of binary op", self.peek(), self.parse_expression())?;
                let span = result.get_span().extended(next_expr.get_span());
                result = Expression::BinaryOp(BinaryOp {
                    operation: op_kind,
                    operand1: Box::new(result),
                    operand2: Box::new(next_expr),
                    span,
                });
            } else {
                return Ok(result);
            }
        }
    }

    /// Base expression meaning no postfix or binary ops
    fn parse_base_expression(&mut self) -> ParseResult<Option<Expression>> {
        let (first, second, third) = self.tokens.peek_three();
        trace!("parse_expression {} {}", first.kind, second.kind);
        if let Some(lit) = self.parse_literal()? {
            return Ok(Some(Expression::Literal(lit)));
        }
        if first.kind == Text {
            // FnCall
            if second.kind == OpenBracket || second.kind == OpenParen {
                trace!("parse_expression FnCall");
                // Eat the name
                self.tokens.advance();
                let type_args: Option<Vec<FnTypeArg>> = if second.kind == OpenBracket {
                    // Eat the OpenBracket
                    self.tokens.advance();
                    let (type_expressions, type_args_span) =
                        self.eat_delimited(Comma, CloseBracket, Parser::expect_type_expression)?;
                    // TODO Support named type arguments later
                    let type_args: Vec<_> = type_expressions
                        .into_iter()
                        .map(|type_expr| FnTypeArg { name: None, value: type_expr })
                        .collect();
                    Some(type_args)
                } else {
                    None
                };
                self.expect_eat_token(OpenParen)?;
                match self.eat_delimited(Comma, CloseParen, Parser::expect_fn_arg) {
                    Ok((args, args_span)) => Ok(Some(Expression::FnCall(FnCall {
                        name: self.intern_ident_token(first),
                        type_args,
                        args,
                        span: first.span.extended(args_span),
                    }))),
                    Err(e) => Err(Parser::error_cause("function arguments", self.peek(), e)),
                }
            } else {
                // The last thing it can be is a simple variable reference expression
                self.tokens.advance();
                Ok(Some(Expression::Variable(Variable {
                    ident: self.intern_ident_token(first),
                    span: first.span,
                })))
            }
        } else if first.kind == OpenBrace {
            // The syntax {} means empty record, not empty block
            // If you want a void or empty block, the required syntax is { () }
            trace!("parse_expr {:?} {:?} {:?}", first, second, third);
            if second.kind == CloseBrace {
                let span = first.span.extended(second.span);
                Ok(Some(Expression::Record(Record { fields: vec![], span })))
            } else if second.kind == Text && third.kind == Colon {
                let record = Parser::expect("record", first, self.parse_record())?;
                Ok(Some(Expression::Record(record)))
            } else {
                match self.parse_block()? {
                    None => Err(Parser::error("block", self.peek())),
                    Some(block) => Ok(Some(Expression::Block(block))),
                }
            }
        } else if first.kind == KeywordIf {
            let if_expr = Parser::expect("If Expression", first, self.parse_if_expr())?;
            Ok(Some(Expression::If(if_expr)))
        } else if first.kind == OpenBracket {
            // Array
            let start = self.expect_eat_token(OpenBracket)?;
            let (elements, span) =
                self.eat_delimited(TokenKind::Comma, TokenKind::CloseBracket, |p| {
                    Parser::expect("expression", start, p.parse_expression())
                })?;
            let span = start.span.extended(span);
            Ok(Some(Expression::Array(ArrayExpr { elements, span })))
        } else {
            // More expression types
            Ok(None)
        }
    }

    fn parse_expression(&mut self) -> ParseResult<Option<Expression>> {
        let parsed_expression = self.parse_base_expression()?;
        // Forward search for binary and postfix ops
        if let Some(expr) = parsed_expression {
            let result = self.parse_expression_postfix_op(expr)?;
            let result = self.parse_expression_binary_op(result)?;
            let result = self.precedence_fixup(result);
            Ok(Some(result))
        } else {
            Ok(None)
        }
    }

    fn precedence_fixup(&self, expression: Expression) -> Expression {
        // TODO: Use the precedence as argument fix when building
        // PRECEDENCE FIXUP TIME
        if let Expression::BinaryOp(bin_op) = expression {
            if let Expression::BinaryOp(binop_rhs) = *bin_op.operand2 {
                // bin_op = * => 1
                // bin_op_rhs = + => 0
                println!("binop fixup at {:?} with {:?}", bin_op.operation, binop_rhs.operation);
                if binop_rhs.operation.precedence() < bin_op.operation.precedence() {
                    let inner_span =
                        bin_op.operand1.get_span().extended(binop_rhs.operand1.get_span());
                    let outer_span = bin_op.span;
                    Expression::BinaryOp(BinaryOp {
                        operation: binop_rhs.operation,
                        operand1: Box::new(Expression::BinaryOp(BinaryOp {
                            operation: bin_op.operation,
                            operand1: bin_op.operand1,
                            operand2: binop_rhs.operand1,
                            span: inner_span,
                        })),
                        operand2: binop_rhs.operand2,
                        span: outer_span,
                    })
                    // 2 * 1 + 3
                    //   *
                    //  2 +
                    //   1 3
                } else {
                    // We have to re-construct the original input because
                    // everything is partially moved
                    Expression::BinaryOp(BinaryOp {
                        operation: bin_op.operation,
                        operand1: bin_op.operand1,
                        operand2: Box::new(Expression::BinaryOp(binop_rhs)),
                        span: bin_op.span,
                    })
                }
            } else {
                Expression::BinaryOp(bin_op)
            }
        } else {
            expression
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
        let name_token = self.expect_eat_token(Text)?;
        let typ = match self.eat_token(Colon) {
            None => Ok(None),
            Some(_) => self.parse_type_expression(),
        }?;
        self.expect_eat_token(Equals)?;
        let initializer_expression =
            Parser::expect("expression", self.peek(), self.parse_expression())?;
        let span = eaten_keyword.span.extended(initializer_expression.get_span());
        Ok(Some(ValDef {
            name: self.intern_ident_token(name_token),
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
        let name_token = self.expect_eat_token(Text)?;
        let _colon = self.expect_eat_token(Colon);
        let typ = Parser::expect("type_expression", self.peek(), self.parse_type_expression())?;
        self.expect_eat_token(Equals)?;
        let value_expr = Parser::expect("expression", self.peek(), self.parse_expression())?;
        let span = keyword_val_token.span.extended(value_expr.get_span());
        ParseResult::Ok(Some(ConstVal {
            name: self.intern_ident_token(name_token),
            ty: typ,
            value_expr,
            span,
        }))
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
        let span = ident_token.span.extended(expr.get_span());
        let ident = self.tok_chars(ident_token).to_string();
        Ok(Some(Assignment { ident: self.ident_id(ident), expr, span }))
    }

    fn eat_fn_arg_def(&mut self) -> ParseResult<FnArgDef> {
        trace!("eat_fn_arg_def");
        let name_token = self.expect_eat_token(Text)?;
        self.expect_eat_token(Colon)?;
        let typ = Parser::expect("type_expression", self.peek(), self.parse_type_expression())?;
        Ok(FnArgDef { name: self.intern_ident_token(name_token), ty: typ })
    }

    fn eat_fndef_args(&mut self) -> ParseResult<(Vec<FnArgDef>, Span)> {
        self.eat_delimited(Comma, CloseParen, Parser::eat_fn_arg_def)
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
                        break Err(Parser::error(delim, self.peek()));
                    }
                }
                Err(e) => {
                    // trace!("eat_delimited got err from 'parse': {}", e);
                    break Err(Parser::error_cause(
                        format!("eat_delimited for delim={delim} term={terminator} encountered error parsing element"),
                        self.peek(),
                        e,
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
            let end_span = alt.as_ref().map(|a| a.get_span()).unwrap_or(consequent_expr.get_span());
            let span = if_keyword.span.extended(end_span);
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
                let span = return_token.span.extended(ret_val.get_span());
                let return_stmt = ReturnStmt { expr: ret_val, span };
                Ok(Some(BlockStmt::ReturnStmt(return_stmt)))
            } else {
                return Err(Parser::error("return statement", self.tokens.next()));
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
        let span = block_start.span.extended(statements_span);
        Ok(Some(Block { stmts: block_statements, span }))
    }

    fn expect_type_param(&mut self) -> ParseResult<TypeParamDef> {
        let s = self.expect_eat_token(Text)?;
        let ident_id = self.intern_ident_token(s);
        Ok(TypeParamDef { ident: ident_id, span: s.span })
    }

    fn parse_function(&mut self) -> ParseResult<Option<FnDef>> {
        trace!("parse_fndef");
        let Some(fn_keyword) = Parser::check(self.eat_token(KeywordFn))? else {
            return Ok(None);
        };
        let func_name = self.expect_eat_token(Text)?;
        let func_name_id = self.intern_ident_token(func_name);
        let type_arguments: Option<Vec<TypeParamDef>> =
            if let TokenKind::OpenBracket = self.peek().kind {
                self.tokens.advance();
                let (type_args, type_arg_span) = self.eat_delimited(
                    TokenKind::Comma,
                    TokenKind::CloseBracket,
                    Parser::expect_type_param,
                )?;
                Some(type_args)
            } else {
                None
            };
        self.expect_eat_token(OpenParen)?;
        let (args, args_span) = self.eat_fndef_args()?;
        self.expect_eat_token(Colon)?;
        let ret_type = self.parse_type_expression()?;
        let block = self.parse_block()?;
        let mut span = fn_keyword.span;
        span.end = block.as_ref().map(|b| b.span.end).unwrap_or(args_span.end);
        // FIXME: Eventually, we'll use an 'intern' keyword to mark intrinsic decls
        //        But for now, we are relying on the name
        let is_intrinsic =
            IntrinsicFunctionType::from_function_name(&self.get_ident_name(func_name_id)).is_some();
        Ok(Some(FnDef {
            name: func_name_id,
            type_args: type_arguments,
            args: args,
            ret_type,
            block,
            span,
            is_intrinsic,
        }))
    }

    fn parse_typedef(&mut self) -> ParseResult<Option<TypeDefn>> {
        let keyword_type = self.eat_token(KeywordType);
        if let Some(keyword_type) = keyword_type {
            let name = self.expect_eat_token(Text)?;
            let equals = self.expect_eat_token(Equals)?;
            let type_expr =
                Parser::expect("Type expression", equals, self.parse_type_expression())?;
            let span = keyword_type.span.extended(type_expr.get_span());
            Ok(Some(TypeDefn { name: self.intern_ident_token(name), value_expr: type_expr, span }))
        } else {
            Ok(None)
        }
    }

    fn parse_definition(&mut self) -> ParseResult<Option<Definition>> {
        if let Some(const_def) = self.parse_const()? {
            let sem = self.eat_token(Semicolon);
            if sem.is_none() {
                return Err(ParseError {
                    expected: Semicolon.to_string(),
                    token: self.peek(),
                    cause: None,
                });
            }
            Ok(Some(Definition::Const(const_def.into())))
        } else if let Some(fn_def) = self.parse_function()? {
            Ok(Some(Definition::FnDef(fn_def.into())))
        } else if let Some(type_def) = self.parse_typedef()? {
            Ok(Some(Definition::Type(type_def.into())))
        } else {
            Ok(None)
        }
    }

    fn parse_module(&mut self, filename: &str) -> ParseResult<AstModule> {
        let mut defs: Vec<Definition> = vec![];

        while let Some(def) = self.parse_definition()? {
            defs.push(def)
        }
        let ident = self.ident_id(filename);
        Ok(AstModule {
            name: filename.to_string(),
            name_id: ident,
            defs,
            source: self.source.clone(),
            identifiers: self.identifiers.clone(),
        })
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
pub fn parse_text(text: &str, module_name: &str, use_prelude: bool) -> ParseResult<AstModule> {
    let full_source: String = if use_prelude {
        let prelude = crate::prelude::PRELUDE_SOURCE;
        let mut modified_source = String::from(prelude);
        modified_source.push('\n');
        modified_source.push_str(text);
        modified_source
    } else {
        text.to_string()
    };
    log::info!("parser full source: {}", &full_source);

    let mut lexer = Lexer::make(&full_source);

    let token_vec = lexer.run();

    // TODO: TokenIter is dumb
    let tokens: TokenIter = TokenIter::make(token_vec);

    let mut parser = Parser::make(tokens, full_source, use_prelude);

    let result = parser.parse_module(module_name);
    if let Err(e) = &result {
        parser.print_error(e);
    }
    result
}

// pub fn print_ast(ast: &Module, identifiers: &Identifiers) -> Result<String, std::fmt::Error> {
//     let mut output = String::new();
//     output.write_str(&ast.name)?;
//     output.write_str("\n")?;
//     for def in &ast.defs {
//         match def {
//             Definition::Type(type_def) => output
//                 .write_fmt(format_args!("Type {} {:?}", type_def.name, type_def.value_expr))?,
//             Definition::FnDef(fn_def) => {
//                 output.write_fmt(format_args!("Function {} {:?}", fn_def.name, fn_def.))?
//             }
//         }
//     }
//     output
// }
