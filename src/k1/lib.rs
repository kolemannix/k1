pub mod codegen_llvm;
pub mod compiler;
pub mod lex;
pub mod parse;
mod pool;
mod strings;
pub mod typer;

static_assert_size!(parse::ParsedStmt, 24); // Get down below 100 // We did it!
static_assert_size!(parse::ParsedExpression, 120); // Get back down ideally below 50
static_assert_size!(typer::TypedExpr, 72);
static_assert_size!(typer::TypedStmt, 16);
