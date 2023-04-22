# nexlang

Just learning lang dev by implementing a toy language. The goal is to implement all the basics, but maybe
with a few syntactic insights or pet ideas that I like. I am trying not to have any aspirations for this project
except to explore the problem space of compiler development, and maybe to be able to do some of advent of code 2023
in this language

# Up next maybe
- [-] Records
  - [x] Syntax decision ({} vs .{} vs Point {})
  - [x] Parsing
  - [x] Typechecking
  - [x] codegen static as LLVM structs
  - [ ] Accessor syntax
  - [ ] Accessor ir
  - [ ] Accessor codegen
  - [ ] codegen dynamic as standard library Maps (long way off)
- [ ] Arrays
- [ ] Ditch semicolons? (this technically introduces significant whitespace)
- [x] Binary infix operations
- [ ] basic error reporting using spans
- [ ] oneofs (tagged unions)
- [ ] Loops
- [ ] Heap memory
- [ ] Tuples
- [ ] Control-Flow: proper codegen for early returns:
    // This needs to return either a basic value or an instruction value (in the case of early return)
    // Actually, early return is a big rabbit hole. We need to typecheck it in ir gen, and probably
    // store it on the block
    //
    // For now, I'm going to return an Option. If the block has an early return, we just return
    // None. We'll fix it when implementing early returns
    // Maybe we rename ReturnStmt to Early Return to separate it from tail returns, which have
    // pretty different semantics and implications for codegen, I am realizing
    fn codegen_block(&mut self, block: &IrBlock) -> Option<BasicValueEnum<'ctx>> {

# Hacks to fix
- [ ] main function / detect entrypoint properly. Typing of main? How to get main args?
- [ ] Replace all vectors with smallvec where appropriate
- [ ] Proper println implementation. Fine to use printf internally for now but we should define our own func around it
- [ ] Scoping is basically non-existent right now
- [ ] Implement Display instead of relying on Debug

Design for 
- optionality
- fallibility


# Dev ex
- Pretty-print AST
- Error Spans
- fancy output COLORS

# Maybe later
- [ ] Block-as-single-arg syntax (xs.map { y => } )
- [ ] Type Params
- [ ] Unicode source
- [ ] multiple parameter lists
- [ ] Symbols like lisp or Clojure keywords would be really great; or just have string literal types?
- [ ] Symbols are always interned and very fast. Also signals intent to use a symbol vs use strings

# Resurrect project (March '23)
- [x] Implement typed IR
- [x] Implement FnCall
- [x] Implement Add
- [x] Implement Multiply
- [x] Use basicvalue not pointervalue as main IR type
- [x] Add booleans
- [x] Add boolean AND and OR
- [x] Parse line comments 
- [x] implement "expected output" for test sources
- [x] Add spans to AST
- [x] Add spans to IR
- [x] Implement IF expressions

