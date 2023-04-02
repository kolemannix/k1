# Back to working
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
- [ ] Add spans to IR
- [ ] Ditch semicolons (this technically introduces significant whitespace)
- [ ] Implement IF expressions

# Up next maybe
- [x] Binary infix operations
- [ ] Structs
- [ ] Loops
- [ ] Type Params
- [ ] Heap memory
- [ ] Tuples

# Hacks to fix
- [ ] main function / detect entrypoint properly. Typing of main? How to get main args?
- [ ] Proper println implementation. Fine to use printf internally for now but we should define our own func around it
- [ ] Scoping is basically non-existent right now

Design for 
- optionality
- fallibility


# Dev ex
- Pretty-print AST
- Error Spans
- fancy output COLORS

# Maybe later
- [ ] Block-as-single-arg syntax (xs.map { y => } )
- [ ] Unicode source
- [ ] multiple parameter lists
- [ ] Symbols like lisp or Clojure keywords would be really great; or just have string literal types?
- [ ] Symbols are always interned and very fast. Also signals intent to use a symbol vs use strings

