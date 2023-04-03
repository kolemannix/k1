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
- [x] Add spans to IR
- [ ] Implement IF expressions

# Up next maybe
- [ ] Ditch semicolons? (this technically introduces significant whitespace)
- [x] Binary infix operations
- [ ] basic error reporting using spans
- [ ] Structs
- [ ] Loops
- [ ] Type Params
- [ ] Heap memory
- [ ] Tuples
- [ ] REPL would be cool; can I just swap a function out for another one for a Clojure-like cider-repl experience? Interactive live sessions

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

