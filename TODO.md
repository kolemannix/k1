# nexlang

Just learning lang dev by implementing a toy language. The goal is to implement all the basics, but maybe
with a few syntactic insights or pet ideas that I like. I am trying not to have any aspirations for this project
except to explore the problem space of compiler development, and maybe to be able to do some of advent of code 2023
in this language

# What? Why?

The project isn't mature enough yet to make this list but the spirit of the effort is this: https://justforfunnoreally.dev/

I was living by this philosophy, which I had never actually consciously formulated, that there was no point in coding outside of work, because
I might as well just be working at that point. Nothing could be further from the truth; the difference between recreational programming and the 
daily grind of a high-stress software engineering job is night and day.

I really just started programming recreationally again about 5 months ago, but I feel so inspired and have been reminded of why
I do this. I share this chapter of The Mythical Man-Month with my programming students at the start of every semester:

> The programmer, like the poet, works only slightly removed from pure thought-stuff. He builds his castles in the air, from air, creating by exertion of the imagination. Few media of creation are so flexible, so easy to polish and rework, so readily capable of realizing grand conceptual structures.... Yet the program construct, unlike the poet's words, is real in the sense that it moves and works, producing visible outputs separate from the construct itself. 

I'm realizing that my interest has never really been in applications at all, but in foundations, in the systems and libraries that make our software tick.
I want to build my own castles, not just learn my way around other people's castles.

There are only like 15 industrially viable programming languages in the world. If your goal is adoption and recognition, building one when you have a full-time job and a family
is an absolute waste of time. You can't possibly hope to compete. For awhile I thought about what side project I would throw my entire being into once
I started programming outside of work again. I saw myself as biding my time, growing in maturity as an engineer, lying in wait to spring that world-class side
project on the world. I bought domain names like naturaldb.dev (I have a great idea for a database, let me tell you!) and thought that if I ever tried to make anything, 
it had to be world-class and industrially viable, make the top page of hackernews, otherwise I'd been wasting my free time and embarassing myself.

But over the last year, through projects like Andreas Kling's SerenityOS and Jakt, Tsoding's 'recreational programming', the Helix editor, and little things like Advent of Code,
I've realized that computers can be for fun again. I can grind on pull requests and large stable systems, worry about backwards-compatible SQL migrations, and trawl through 
production logs and plan sprints by day, and still work on interesting software at night. Even if I only find 1 free hour this week, maybe I'll clean up something in the parser, 
maybe I'll just fix warnings, or think about syntax (for real: syntax is so important its how our brains work its literally the purpose of programming languages and not so easy
to separate from 'semantics' so don't get me started) for Arrays in my little language. Once it becomes recreational, it's all fun. It's not a side hustle, 
it's just an activity. It just took me 10 years to discover it again, but this time at a deeper level.

# Up next maybe
- [x] Records
  - [x] Syntax decision ({} vs .{} vs Point {})
  - [x] Parsing
  - [x] Typechecking
  - [x] codegen static as LLVM structs
  - [x] Accessor syntax
  - [x] Accessor ir
  - [x] Accessor codegen
- [x] Identifier cleanup and interning
- [x] Actual scoping
- [x] Real prelude so we can more easily add runtime/stl functions (for array)
- [x] Heap memory (just using malloc)
- [x] Arrays (Fixed size but heap allocated)
- [ ] Remove / fix TokenIter type
- [ ] Optionals
- [ ] Function syntax change (foo = fn (a, b, c): int { }
- [ ] Ditch semicolons? (this technically introduces significant whitespace; need newline token)
- [ ] Syntax changes; from qualifier-focused to name -focused
- [ ] Introduce uint type
- [x] Binary infix operations
- [x] basic error reporting using spans
- [ ] oneofs (variants?) (tagged unions)
- [ ] Loops
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
- [ ] Tuples

# Fibonacci todos (Aug 20)
- [x] Equality binop
- [x] Precedence of binops; parens?
- [x] Recursion fix: process module decls first, then impls
- [x] Negative integer literals
- [x] PHI nested branch fix

# Hacks to fix
- [ ] Fix unnecessary load of function args
- [ ] main function / detect entrypoint properly. Typing of main? How to get main args?
- [ ] Replace vectors with smallvec where appropriate
- [x] Proper println implementation. Fine to use printf internally for now but we should define our own func around it
- [ ] Implement Display instead of relying on Debug

Design for 
- optionality
- fallibility


# Dev ex
- Pretty-print AST
- [x] Error Spans
- [x] fancy output COLORS

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

