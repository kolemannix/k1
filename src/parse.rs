use std::cell::RefCell;
use std::fmt::{Display, Formatter, Write};
use std::rc::Rc;
use string_interner::Symbol;

use crate::lex::*;
use crate::typer::{self, BinaryOpKind, Linkage, UnaryOpKind};
use TokenKind as K;

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
    Char(u8, Span),
    Numeric(String, Span),
    Bool(bool, Span),
    /// TODO: Move these into the intern pool?
    String(String, Span),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Unit(_) => f.write_str("()"),
            Literal::Char(byte, _) => f.write_char(*byte as char),
            Literal::Numeric(n, _) => f.write_str(n),
            Literal::Bool(true, _) => f.write_str("true"),
            Literal::Bool(false, _) => f.write_str("false"),
            Literal::String(s, _) => f.write_str(s),
        }
    }
}

impl Literal {
    pub fn get_span(&self) -> Span {
        match self {
            Literal::Unit(span) => *span,
            Literal::Char(_, span) => *span,
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
pub struct FnCallArg {
    pub name: Option<IdentifierId>,
    pub value: Expression,
}

#[derive(Debug)]
pub struct FnCallTypeArg {
    pub name: Option<IdentifierId>,
    pub value: TypeExpression,
}

#[derive(Debug)]
pub struct FnCall {
    pub name: IdentifierId,
    pub type_args: Option<Vec<FnCallTypeArg>>,
    pub args: Vec<FnCallArg>,
    pub namespaces: Vec<IdentifierId>,
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

#[derive(Debug)]
pub struct BinaryOp {
    pub op_kind: BinaryOpKind,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub span: Span,
}

#[derive(Debug)]
pub struct UnaryOp {
    pub op_kind: UnaryOpKind,
    pub expr: Box<Expression>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Variable {
    pub ident: IdentifierId,
    pub span: Span,
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("var#{}", self.ident))
    }
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

impl Display for IndexOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.target.fmt(f)?;
        f.write_char('[')?;
        self.index_expr.fmt(f)?;
        f.write_char(']')?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct MethodCall {
    pub base: Box<Expression>,
    pub call: Box<FnCall>,
    pub span: Span,
}

#[derive(Debug)]
pub enum Expression {
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    Literal(Literal),
    FnCall(FnCall),
    Variable(Variable),
    FieldAccess(FieldAccess),
    MethodCall(MethodCall),
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
    #[inline]
    pub fn get_span(&self) -> Span {
        match self {
            Expression::BinaryOp(op) => op.span,
            Expression::UnaryOp(op) => op.span,
            Expression::Literal(lit) => lit.get_span(),
            Expression::FnCall(call) => call.span,
            Expression::Variable(var) => var.span,
            Expression::FieldAccess(acc) => acc.span,
            Expression::MethodCall(call) => call.span,
            Expression::Block(block) => block.span,
            Expression::If(if_expr) => if_expr.span,
            Expression::Record(record) => record.span,
            Expression::IndexOperation(op) => op.span,
            Expression::Array(array_expr) => array_expr.span,
        }
    }

    pub fn is_assignable(&self) -> bool {
        match self {
            Expression::Variable(_var) => true,
            Expression::IndexOperation(_op) => true,
            Expression::FieldAccess(_acc) => true,
            Expression::MethodCall(_call) => false,
            Expression::BinaryOp(_op) => false,
            Expression::UnaryOp(_op) => false,
            Expression::Literal(_lit) => false,
            Expression::FnCall(_call) => false,
            Expression::Block(_block) => false,
            Expression::If(_if_expr) => false,
            Expression::Record(_record) => false,
            Expression::Array(_array_expr) => false,
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::BinaryOp(op) => {
                f.write_fmt(format_args!("({} {} {})", op.lhs, op.op_kind, op.rhs))
            }
            Expression::UnaryOp(op) => {
                op.op_kind.fmt(f);
                op.expr.fmt(f)
            }
            Expression::Literal(lit) => lit.fmt(f),
            Expression::FnCall(call) => std::fmt::Debug::fmt(call, f),
            Expression::Variable(var) => var.fmt(f),
            Expression::FieldAccess(acc) => std::fmt::Debug::fmt(acc, f),
            Expression::MethodCall(call) => std::fmt::Debug::fmt(call, f),
            Expression::Block(block) => std::fmt::Debug::fmt(block, f),
            Expression::If(if_expr) => std::fmt::Debug::fmt(if_expr, f),
            Expression::Record(record) => std::fmt::Debug::fmt(record, f),
            Expression::IndexOperation(op) => op.fmt(f),
            Expression::Array(array_expr) => std::fmt::Debug::fmt(array_expr, f),
        }
    }
}

enum ExprStackMember {
    Operator(BinaryOpKind, Span),
    Expr(Expression),
}

impl ExprStackMember {
    fn expect_expr(self) -> Expression {
        match self {
            ExprStackMember::Expr(expr) => expr,
            _ => panic!("expected expr"),
        }
    }
    fn expect_operator(self) -> (BinaryOpKind, Span) {
        match self {
            ExprStackMember::Operator(kind, span) => (kind, span),
            _ => panic!("expected operator"),
        }
    }
}

#[derive(Debug)]
pub struct Assignment {
    pub lhs: Expression,
    pub rhs: Expression,
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
pub struct WhileStmt {
    pub cond: Expression,
    pub block: Block,
    /// Maybe its better not to store a span on nodes for which a span is trivially calculated
    pub span: Span,
}

#[derive(Debug)]
pub enum BlockStmt {
    ValDef(ValDef),
    Assignment(Assignment),
    LoneExpression(Expression),
    While(WhileStmt),
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
    Char(Span),
    Int(Span),
    Bool(Span),
    String(Span),
    Record(RecordType),
    Name(IdentifierId, Span),
    TypeApplication(TypeApplication),
}

impl TypeExpression {
    #[inline]
    pub fn is_int(&self) -> bool {
        match self {
            TypeExpression::Int(_) => true,
            _ => false,
        }
    }
    #[inline]
    pub fn get_span(&self) -> Span {
        match self {
            TypeExpression::Unit(span) => *span,
            TypeExpression::Char(span) => *span,
            TypeExpression::Int(span) => *span,
            TypeExpression::Bool(span) => *span,
            TypeExpression::String(span) => *span,
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
    pub linkage: Linkage,
    pub ast_id: AstId,
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
    pub ast_id: AstId,
}

#[derive(Debug)]
pub struct TypeDefn {
    pub name: IdentifierId,
    pub value_expr: TypeExpression,
    pub span: Span,
    pub ast_id: AstId,
}

#[derive(Debug)]
pub struct ParsedNamespace {
    pub name: IdentifierId,
    pub definitions: Vec<Definition>,
    pub ast_id: AstId,
}

#[derive(Debug)]
pub enum Definition {
    FnDef(Box<FnDef>),
    Const(Box<ConstVal>),
    TypeDef(Box<TypeDefn>),
    Namespace(Box<ParsedNamespace>),
}

impl Definition {
    pub fn get_name(&self) -> IdentifierId {
        match self {
            Definition::FnDef(def) => def.name,
            Definition::Const(def) => def.name,
            Definition::TypeDef(def) => def.name,
            Definition::Namespace(def) => def.name,
        }
    }
    pub fn get_ast_id(&self) -> AstId {
        match self {
            Definition::FnDef(def) => def.ast_id,
            Definition::Const(def) => def.ast_id,
            Definition::TypeDef(def) => def.ast_id,
            Definition::Namespace(def) => def.ast_id,
        }
    }
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
    pub fn get_ident_str(&self, id: IdentifierId) -> impl std::ops::Deref<Target = str> + '_ {
        std::cell::Ref::map(self.identifiers.borrow(), |idents| idents.get_name(id))
    }

    pub fn get_defn(&self, ast_id: AstId) -> &Definition {
        log::trace!("get_defn id {}", ast_id);
        for defn in &self.defs {
            log::trace!(
                "defn {:?} has ast_id {}",
                &*self.get_ident_str(defn.get_name()),
                defn.get_ast_id()
            );
            if defn.get_ast_id() == ast_id {
                return defn;
            } else if let Definition::Namespace(ns) = defn {
                for inner_def in &ns.definitions {
                    log::trace!(
                        "inner defn {:?} has ast_id {}",
                        &*self.get_ident_str(inner_def.get_name()),
                        inner_def.get_ast_id()
                    );
                    if inner_def.get_ast_id() == ast_id {
                        return inner_def;
                    }
                }
            }
        }
        panic!("failed to find defn with ast_id {}", ast_id);
    }

    pub fn defns_iter(&self) -> impl Iterator<Item = &Definition> {
        self.defs.iter()
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

struct Parser<'toks> {
    tokens: TokenIter<'toks>,
    source: Rc<Source>,
    identifiers: Rc<RefCell<Identifiers>>,
    ast_id_index: u32,
}

impl<'toks> Parser<'toks> {
    fn make(tokens: &'toks [Token], source: String) -> Parser {
        // TODO: parse the lines ourselves
        let lines: Vec<_> = source.lines().map(|l| l.to_owned()).collect();
        Parser {
            tokens: TokenIter::make(tokens),
            source: Rc::new(Source { content: source, lines }),
            identifiers: Rc::new(RefCell::new(Identifiers::default())),
            ast_id_index: 0,
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

impl<'toks> Parser<'toks> {
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
        let adjusted_line = span.line as i32 - crate::prelude::PRELUDE_LINES as i32 + 1;
        let line_no =
            if adjusted_line < 0 { "PRELUDE".to_string() } else { adjusted_line.to_string() };
        use colored::*;
        println!(
            "{} on line {}. Expected '{}', but got '{}'",
            "parse error".red(),
            line_no,
            parse_error.expected.blue(),
            parse_error.token.kind.as_ref().red()
        );
        println!();
        println!("{line_text}");
        println!("{span_text}");
    }

    fn peek(&self) -> Token {
        self.tokens.peek()
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

    fn eat_token(&mut self, target_token: TokenKind) -> Option<Token> {
        let tok = self.peek();
        if tok.kind == target_token {
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
                Err(Parser::error(target_token, actual))
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
            (K::OpenParen, K::CloseParen) => {
                trace!("parse_literal unit");
                let span = first.span.extended(second.span);
                self.tokens.advance();
                self.tokens.advance();
                Ok(Some(Literal::Unit(span)))
            }
            (K::Char, _) => {
                trace!("parse_literal char");
                self.tokens.advance();
                let text = self.tok_chars(first);
                assert_eq!(text.len(), 1);
                let byte = text.bytes().next().unwrap();
                Ok(Some(Literal::Char(byte, first.span)))
            }
            (K::String, _) => {
                trace!("parse_literal string");
                self.tokens.advance();
                let text = self.tok_chars(first);
                Ok(Some(Literal::String(text.to_string(), first.span)))
            }
            (K::Minus, K::Ident) if !second.is_whitespace_preceeded() => {
                let text = self.tok_chars(second);
                if text.chars().next().unwrap().is_numeric() {
                    let mut s = "-".to_string();
                    s.push_str(text);
                    self.tokens.advance();
                    self.tokens.advance();
                    Ok(Some(Literal::Numeric(s, first.span.extended(second.span))))
                } else {
                    Err(Parser::error("number following '-'", second))
                }
            }
            (K::Ident, _) => {
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
        let name_token = self.expect_eat_token(K::Ident)?;
        let ident_id = self.intern_ident_token(name_token);
        self.expect_eat_token(K::Colon)?;
        let typ_expr =
            Parser::expect("Type expression", self.peek(), self.parse_type_expression())?;
        Ok(Some(RecordTypeField { name: ident_id, ty: typ_expr }))
    }

    fn expect_type_expression(&mut self) -> ParseResult<TypeExpression> {
        Parser::expect("type_expression", self.peek(), self.parse_type_expression())
    }

    fn parse_type_expression(&mut self) -> ParseResult<Option<TypeExpression>> {
        let tok = self.peek();
        if tok.kind == K::Ident {
            let source = self.source.clone();
            let text_str = Source::get_span_content(&source, tok.span);
            if text_str == "unit" {
                self.tokens.advance();
                Ok(Some(TypeExpression::Unit(tok.span)))
            } else if text_str == "string" {
                self.tokens.advance();
                Ok(Some(TypeExpression::String(tok.span)))
            } else if text_str == "int" {
                self.tokens.advance();
                Ok(Some(TypeExpression::Int(tok.span)))
            } else if text_str == "bool" {
                self.tokens.advance();
                Ok(Some(TypeExpression::Bool(tok.span)))
            } else if text_str == "char" {
                self.tokens.advance();
                Ok(Some(TypeExpression::Char(tok.span)))
            } else {
                self.tokens.advance();
                let next = self.tokens.peek();
                if next.kind == K::OpenAngle {
                    // parameterized type: Dict[int, int]
                    self.tokens.advance();
                    let (type_parameters, params_span) =
                        self.eat_delimited(K::Comma, K::CloseAngle, |p| {
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
        } else if tok.kind == K::OpenBrace {
            let open_brace = self.expect_eat_token(K::OpenBrace)?;
            let (fields, fields_span) = self.eat_delimited(K::Comma, K::CloseBrace, |p| {
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

    fn parse_fn_arg(&mut self) -> ParseResult<Option<FnCallArg>> {
        let (one, two) = self.tokens.peek_two();
        let named = if one.kind == K::Ident && two.kind == K::Equals {
            self.tokens.advance();
            self.tokens.advance();
            true
        } else {
            false
        };
        match self.parse_expression() {
            Ok(Some(expr)) => {
                let name = if named { Some(self.intern_ident_token(one)) } else { None };
                Ok(Some(FnCallArg { name, value: expr }))
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

    fn expect_fn_arg(&mut self) -> ParseResult<FnCallArg> {
        let res = self.parse_fn_arg();
        Parser::expect("fn_arg", self.peek(), res)
    }

    fn parse_record(&mut self) -> ParseResult<Option<Record>> {
        let Some(open_brace) = self.eat_token(K::OpenBrace) else {
            return Ok(None);
        };
        let (fields, fields_span) = self.eat_delimited(K::Comma, K::CloseBrace, |parser| {
            let name = parser.expect_eat_token(K::Ident)?;
            parser.expect_eat_token(K::Colon)?;
            let expr = Parser::expect("expression", parser.peek(), parser.parse_expression())?;
            Ok(RecordField { name: parser.intern_ident_token(name), expr })
        })?;
        let span = open_brace.span.extended(fields_span);
        Ok(Some(Record { fields, span }))
    }

    fn parse_expression_with_postfix_ops(&mut self) -> ParseResult<Option<Expression>> {
        let Some(mut result) = self.parse_base_expression()? else { return Ok(None) };
        // Looping for postfix ops inspired by Jakt's parser
        loop {
            let next = self.peek();
            if next.kind.is_postfix_operator() {
                if next.kind == K::Bang {
                    unimplemented!("Postfix ! is unimplemented")
                }
                if next.kind == K::Dot {
                    // Field access syntax; a.b
                    self.tokens.advance();
                    let target = self.expect_eat_token(K::Ident)?;
                    let next = self.peek();
                    // method call access syntax; a.b() a.b<int>(c)
                    if next.kind == K::OpenParen
                        || (next.kind == K::OpenAngle && !next.is_whitespace_preceeded())
                    {
                        let type_args = self.parse_optional_type_args()?;
                        self.expect_eat_token(K::OpenParen)?;
                        let (args, args_span) =
                            self.eat_delimited(K::Comma, K::CloseParen, Parser::expect_fn_arg)?;
                        let span = result.get_span().extended(args_span);
                        result = Expression::MethodCall(MethodCall {
                            base: Box::new(result),
                            call: Box::new(FnCall {
                                name: self.intern_ident_token(target),
                                type_args,
                                args,
                                namespaces: Vec::new(),
                                span,
                            }),
                            span,
                        });
                    } else {
                        let span = result.get_span().extended(next.span);
                        result = Expression::FieldAccess(FieldAccess {
                            base: Box::new(result),
                            target: self.intern_ident_token(target),
                            span,
                        });
                    }
                }
                if next.kind == K::OpenBracket {
                    self.tokens.advance();
                    let index_expr = Parser::expect(
                        "expression inside []",
                        self.peek(),
                        self.parse_expression(),
                    )?;
                    let close = self.expect_eat_token(K::CloseBracket)?;
                    let span = result.get_span().extended(close.span);
                    result = Expression::IndexOperation(IndexOperation {
                        target: Box::new(result),
                        index_expr: Box::new(index_expr),
                        span,
                    });
                }
            } else {
                return Ok(Some(result));
            }
        }
    }

    fn expect_expression(&mut self) -> ParseResult<Expression> {
        Parser::expect("expression", self.peek(), self.parse_expression())
    }

    fn parse_expression(&mut self) -> ParseResult<Option<Expression>> {
        let Some(expr) = self.parse_expression_with_postfix_ops()? else {
            return Ok(None);
        };
        if !self.peek().kind.is_binary_operator() {
            return Ok(Some(expr));
        }
        let mut expr_stack: Vec<ExprStackMember> = vec![ExprStackMember::Expr(expr)];
        let mut last_precedence = 100_000;
        loop {
            let tok = self.peek();
            let Some(op_kind) = BinaryOpKind::from_tokenkind(tok.kind) else {
                break;
            };
            let precedence = op_kind.precedence();
            self.tokens.advance();
            let rhs = Parser::expect(
                "rhs of binary op",
                self.peek(),
                self.parse_expression_with_postfix_ops(),
            )?;
            while precedence <= last_precedence && expr_stack.len() > 1 {
                log::trace!(
                    "expr_stack at {:?}, precedence={}, last={}, stacklen={}",
                    op_kind,
                    precedence,
                    last_precedence,
                    expr_stack.len()
                );
                let rhs = expr_stack.pop().unwrap().expect_expr();
                let (op_kind, op_span) = expr_stack.pop().unwrap().expect_operator();
                last_precedence = op_kind.precedence();
                if last_precedence < precedence {
                    expr_stack.push(ExprStackMember::Operator(op_kind, op_span));
                    expr_stack.push(ExprStackMember::Expr(rhs));
                    break;
                }
                let ExprStackMember::Expr(lhs) = expr_stack.pop().unwrap() else {
                    panic!("expected expr on stack")
                };
                let new_span = lhs.get_span().extended(rhs.get_span());
                let bin_op = Expression::BinaryOp(BinaryOp {
                    op_kind,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span: new_span,
                });
                expr_stack.push(ExprStackMember::Expr(bin_op))
            }
            expr_stack.push(ExprStackMember::Operator(op_kind, tok.span));
            expr_stack.push(ExprStackMember::Expr(rhs));

            last_precedence = precedence;
        }

        // Pop and build now that everything is right
        while expr_stack.len() > 1 {
            let ExprStackMember::Expr(rhs) = expr_stack.pop().unwrap() else {
                panic!("expected expr")
            };
            let ExprStackMember::Operator(op_kind, _) = expr_stack.pop().unwrap() else {
                panic!("expected operator")
            };
            let ExprStackMember::Expr(lhs) = expr_stack.pop().unwrap() else {
                panic!("expected expr")
            };
            let new_span = lhs.get_span().extended(rhs.get_span());
            expr_stack.push(ExprStackMember::Expr(Expression::BinaryOp(BinaryOp {
                op_kind,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span: new_span,
            })));
        }
        let final_expr = expr_stack.pop().unwrap().expect_expr();
        Ok(Some(final_expr))
    }

    fn parse_optional_type_args(&mut self) -> ParseResult<Option<Vec<FnCallTypeArg>>> {
        let next = self.peek();
        if next.kind == K::OpenAngle {
            // Eat the OpenAngle
            self.tokens.advance();
            let (type_expressions, type_args_span) =
                self.eat_delimited(K::Comma, K::CloseAngle, Parser::expect_type_expression)?;
            // TODO named type arguments
            let type_args: Vec<_> = type_expressions
                .into_iter()
                .map(|type_expr| FnCallTypeArg { name: None, value: type_expr })
                .collect();
            Ok(Some(type_args))
        } else {
            Ok(None)
        }
    }

    /// Base expression meaning no postfix or binary ops
    fn parse_base_expression(&mut self) -> ParseResult<Option<Expression>> {
        let (first, second, third) = self.tokens.peek_three();
        trace!("parse_expression {} {}", first.kind, second.kind);
        if let Some(lit) = self.parse_literal()? {
            return Ok(Some(Expression::Literal(lit)));
        }
        if first.kind == K::OpenParen {
            self.tokens.advance();
            let expr = self.expect_expression()?;
            // TODO: If comma, parse a tuple
            self.expect_eat_token(K::CloseParen)?;
            Ok(Some(expr))
        } else if first.kind == K::Bang {
            self.tokens.advance();
            let expr = self.expect_expression()?;
            let span = first.span.extended(expr.get_span());
            Ok(Some(Expression::UnaryOp(UnaryOp {
                expr: Box::new(expr),
                op_kind: UnaryOpKind::BooleanNegation,
                span,
            })))
        } else if first.kind == K::Ident {
            // FnCall
            // Here we use is_whitespace_preceeded to distinguish between:
            // square<int>(42) -> FnCall
            // square < int > (42) -> square LESS THAN int GREATER THAN (42)
            let mut namespaces = Vec::new();
            if second.kind == K::Colon
                && third.kind == K::Colon
                && !second.is_whitespace_preceeded()
            {
                // Namespaced expression; foo::
                // Loop until we don't see a ::
                namespaces.push(self.intern_ident_token(first));
                self.tokens.advance(); // ident
                self.tokens.advance(); // colon
                self.tokens.advance(); // colon
                loop {
                    log::trace!("Parsing namespaces {:?}", namespaces);
                    let (a, b, c) = self.tokens.peek_three();
                    log::trace!("Parsing namespaces peeked 3 {} {} {}", a.kind, b.kind, c.kind);
                    if a.kind == K::Colon && b.kind == K::Colon && c.kind == K::Ident {
                        self.tokens.advance(); // ident
                        self.tokens.advance(); // colon
                        self.tokens.advance(); // colon
                        namespaces.push(self.intern_ident_token(c));
                    } else {
                        break;
                    }
                }
            }
            let (first, second) = self.tokens.peek_two();
            if (second.kind == K::OpenAngle && !second.is_whitespace_preceeded())
                || second.kind == K::OpenParen
            {
                trace!("parse_expression FnCall");
                // Eat the name
                self.tokens.advance();
                let type_args = self.parse_optional_type_args()?;
                self.expect_eat_token(K::OpenParen)?;
                let (args, args_span) =
                    self.eat_delimited(K::Comma, K::CloseParen, Parser::expect_fn_arg)?;
                Ok(Some(Expression::FnCall(FnCall {
                    name: self.intern_ident_token(first),
                    type_args,
                    args,
                    namespaces,
                    span: first.span.extended(args_span),
                })))
            } else {
                // The last thing it can be is a simple variable reference expression
                self.tokens.advance();
                Ok(Some(Expression::Variable(Variable {
                    ident: self.intern_ident_token(first),
                    // namespaces are ready when we need em here
                    span: first.span,
                })))
            }
        } else if first.kind == K::OpenBrace {
            // The syntax {} means empty record, not empty block
            // If you want a void or empty block, the required syntax is { () }
            trace!("parse_expr {:?} {:?} {:?}", first, second, third);
            if second.kind == K::CloseBrace {
                let span = first.span.extended(second.span);
                Ok(Some(Expression::Record(Record { fields: vec![], span })))
            } else if second.kind == K::Ident && third.kind == K::Colon {
                let record = Parser::expect("record", first, self.parse_record())?;
                Ok(Some(Expression::Record(record)))
            } else {
                match self.parse_block()? {
                    None => Err(Parser::error("block", self.peek())),
                    Some(block) => Ok(Some(Expression::Block(block))),
                }
            }
        } else if first.kind == K::KeywordIf {
            let if_expr = Parser::expect("If Expression", first, self.parse_if_expr())?;
            Ok(Some(Expression::If(if_expr)))
        } else if first.kind == K::OpenBracket {
            // Array
            let start = self.expect_eat_token(K::OpenBracket)?;
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

    fn parse_mut(&mut self) -> ParseResult<Option<ValDef>> {
        self.parse_val(true)
    }

    fn parse_val(&mut self, mutable: bool) -> ParseResult<Option<ValDef>> {
        trace!("parse_val");
        let keyword = if mutable { K::KeywordMut } else { K::KeywordVal };
        let Some(eaten_keyword) = self.eat_token(keyword) else {
            return Ok(None);
        };
        let name_token = self.expect_eat_token(K::Ident)?;
        let typ = match self.eat_token(K::Colon) {
            None => Ok(None),
            Some(_) => self.parse_type_expression(),
        }?;
        self.expect_eat_token(K::Equals)?;
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
        let Some(keyword_val_token) = self.eat_token(K::KeywordVal) else {
            return Ok(None);
        };
        let name_token = self.expect_eat_token(K::Ident)?;
        let _colon = self.expect_eat_token(K::Colon);
        let typ = Parser::expect("type_expression", self.peek(), self.parse_type_expression())?;
        self.expect_eat_token(K::Equals)?;
        let value_expr = Parser::expect("expression", self.peek(), self.parse_expression())?;
        let span = keyword_val_token.span.extended(value_expr.get_span());
        ParseResult::Ok(Some(ConstVal {
            name: self.intern_ident_token(name_token),
            ty: typ,
            value_expr,
            span,
            ast_id: self.next_ast_id(),
        }))
    }

    fn parse_assignment(&mut self, lhs: Expression) -> ParseResult<Assignment> {
        let valid_lhs = match &lhs {
            Expression::FieldAccess(_) => true,
            Expression::Variable(_) => true,
            Expression::IndexOperation(_) => true,
            _ => false,
        };
        self.expect_eat_token(K::Equals)?;
        let rhs = self.expect_expression()?;
        let span = lhs.get_span().extended(rhs.get_span());
        Ok(Assignment { lhs, rhs, span })
    }

    fn eat_fn_arg_def(&mut self) -> ParseResult<FnArgDef> {
        trace!("eat_fn_arg_def");
        let name_token = self.expect_eat_token(K::Ident)?;
        self.expect_eat_token(K::Colon)?;
        let typ = Parser::expect("type_expression", self.peek(), self.parse_type_expression())?;
        Ok(FnArgDef { name: self.intern_ident_token(name_token), ty: typ })
    }

    fn eat_fndef_args(&mut self) -> ParseResult<(Vec<FnArgDef>, Span)> {
        self.eat_delimited(K::Comma, K::CloseParen, Parser::eat_fn_arg_def)
    }

    fn eat_delimited<T, F>(
        &mut self,
        delim: TokenKind,
        terminator: TokenKind,
        parse: F,
    ) -> ParseResult<(Vec<T>, Span)>
    where
        F: Fn(&mut Parser<'toks>) -> ParseResult<T>,
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
            let alt = if else_peek.kind == K::KeywordElse {
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

    fn parse_while_loop(&mut self) -> ParseResult<Option<WhileStmt>> {
        let while_token = self.peek();
        if while_token.kind != K::KeywordWhile {
            return Ok(None);
        }
        self.tokens.advance();
        let cond = self.expect_expression()?;
        let block = Parser::expect("block for while loop", while_token, self.parse_block())?;
        let span = while_token.span.extended(block.span);
        Ok(Some(WhileStmt { cond, block, span }))
    }

    fn parse_statement(&mut self) -> ParseResult<Option<BlockStmt>> {
        trace!("eat_statement {:?}", self.peek());
        if let Some(while_loop) = self.parse_while_loop()? {
            Ok(Some(BlockStmt::While(while_loop)))
        } else if let Some(mut_def) = self.parse_mut()? {
            Ok(Some(BlockStmt::ValDef(mut_def)))
        } else if let Some(val_def) = self.parse_val(false)? {
            Ok(Some(BlockStmt::ValDef(val_def)))
        } else if let Some(expr) = self.parse_expression()? {
            let peeked = self.peek();
            // Assignment:
            // - Validate expr type, since only some exprs can be LHS of an assignment
            // - Build assignment
            if peeked.kind == K::Equals {
                let assgn = self.parse_assignment(expr)?;
                Ok(Some(BlockStmt::Assignment(assgn)))
            } else {
                Ok(Some(BlockStmt::LoneExpression(expr)))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_block(&mut self) -> ParseResult<Option<Block>> {
        let Some(block_start) = self.eat_token(K::OpenBrace) else {
            return Ok(None);
        };
        let closure =
            |p: &mut Parser| Parser::expect("statement", p.peek(), Parser::parse_statement(p));
        let (block_statements, statements_span) =
            self.eat_delimited(K::Semicolon, K::CloseBrace, closure)?;
        let span = block_start.span.extended(statements_span);
        Ok(Some(Block { stmts: block_statements, span }))
    }

    fn expect_type_param(&mut self) -> ParseResult<TypeParamDef> {
        let s = self.expect_eat_token(K::Ident)?;
        let ident_id = self.intern_ident_token(s);
        Ok(TypeParamDef { ident: ident_id, span: s.span })
    }

    fn parse_function(&mut self) -> ParseResult<Option<FnDef>> {
        trace!("parse_fndef");
        let is_intrinsic = if self.peek().kind == K::KeywordIntern {
            self.tokens.advance();
            true
        } else {
            false
        };
        let linkage = if is_intrinsic {
            typer::Linkage::Intrinsic
        } else if !is_intrinsic && self.peek().kind == K::KeywordExtern {
            self.tokens.advance();
            typer::Linkage::External
        } else {
            typer::Linkage::Standard
        };

        let Some(fn_keyword) = self.eat_token(K::KeywordFn) else {
            if is_intrinsic {
                return Err(ParseError {
                    expected: "fn".to_string(),
                    token: self.peek(),
                    cause: None,
                });
            } else {
                return Ok(None);
            }
        };
        let func_name = self.expect_eat_token(K::Ident)?;
        let func_name_id = self.intern_ident_token(func_name);
        let type_arguments: Option<Vec<TypeParamDef>> =
            if let TokenKind::OpenAngle = self.peek().kind {
                self.tokens.advance();
                let (type_args, type_arg_span) = self.eat_delimited(
                    TokenKind::Comma,
                    TokenKind::CloseAngle,
                    Parser::expect_type_param,
                )?;
                Some(type_args)
            } else {
                None
            };
        self.expect_eat_token(K::OpenParen)?;
        let (args, args_span) = self.eat_fndef_args()?;
        self.expect_eat_token(K::Colon)?;
        let ret_type = self.parse_type_expression()?;
        let block = self.parse_block()?;
        let mut span = fn_keyword.span;
        span.end = block.as_ref().map(|b| b.span.end).unwrap_or(args_span.end);
        Ok(Some(FnDef {
            name: func_name_id,
            type_args: type_arguments,
            args,
            ret_type,
            block,
            span,
            linkage,
            ast_id: self.next_ast_id(),
        }))
    }
    fn next_ast_id(&mut self) -> AstId {
        let id = self.ast_id_index;
        self.ast_id_index += 1;
        id
    }

    fn parse_typedef(&mut self) -> ParseResult<Option<TypeDefn>> {
        let keyword_type = self.eat_token(K::KeywordType);
        if let Some(keyword_type) = keyword_type {
            let name = self.expect_eat_token(K::Ident)?;
            let equals = self.expect_eat_token(K::Equals)?;
            let type_expr =
                Parser::expect("Type expression", equals, self.parse_type_expression())?;
            let span = keyword_type.span.extended(type_expr.get_span());
            Ok(Some(TypeDefn {
                name: self.intern_ident_token(name),
                value_expr: type_expr,
                span,
                ast_id: self.next_ast_id(),
            }))
        } else {
            Ok(None)
        }
    }

    fn parse_namespace(&mut self) -> ParseResult<Option<ParsedNamespace>> {
        let next = self.peek();
        if next.kind != K::KeywordNamespace {
            return Ok(None);
        };
        self.tokens.advance();
        let ident = self.expect_eat_token(K::Ident)?;
        self.expect_eat_token(K::OpenBrace)?;
        let mut functions = Vec::new();
        while let Some(fn_def) = self.parse_function()? {
            functions.push(Definition::FnDef(Box::new(fn_def)));
        }
        self.expect_eat_token(K::CloseBrace)?;
        Ok(Some(ParsedNamespace {
            name: self.intern_ident_token(ident),
            definitions: functions,
            ast_id: self.next_ast_id(),
        }))
    }

    fn parse_definition(&mut self) -> ParseResult<Option<Definition>> {
        if let Some(ns) = self.parse_namespace()? {
            Ok(Some(Definition::Namespace(ns.into())))
        } else if let Some(const_def) = self.parse_const()? {
            self.expect_eat_token(K::Semicolon)?;
            Ok(Some(Definition::Const(const_def.into())))
        } else if let Some(fn_def) = self.parse_function()? {
            Ok(Some(Definition::FnDef(fn_def.into())))
        } else if let Some(type_def) = self.parse_typedef()? {
            Ok(Some(Definition::TypeDef(type_def.into())))
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
        if tok.kind == K::Ident {
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
    log::info!("parser full source:\n{}", &full_source);

    let mut lexer = Lexer::make(&full_source);

    let token_vec: Vec<Token> =
        lexer.run().into_iter().filter(|token| token.kind != K::LineComment).collect();

    let mut parser = Parser::make(&token_vec, full_source);

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
