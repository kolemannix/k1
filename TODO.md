# Project Completion Checklist

- Fixing enums big time
  - [x] Rename Optional -> Opt
  - [x] Remove tag literals, make enum tags per-enum
  - [x] Parse type expr `Opt[T].Some`
  - [x] expr `Opt.None`
  - [x] expr `_root::Opt.None`
  - [x] expr `_root::Opt.None[i32]`
  - [x] expr `Opt.Some(5)`
  - [x] expr `Opt.Some[i32](5)`
  - [x] expr `_root::Opt.Some[i32](5)`
  - [x] deal with fncall syntax collision by checking for the enum first
  - [x] kill ParsedTypeExpression::AnonEnumVariant?
  - [x] CastType `EnumVariant`: checks should not happen in codegen
  - [x] Convert TypedIf.consequent and TypedIf.alternative to TypedExpr from TypedBlock

- [x] QoL before rest of pattern matching
  - [x] Codegen fail instead of panic
  - [x] Rename record to struct
  - [x] Start gui
  - [x] Remove ! for negation; switch to 'not'
- [x] Get LLVM interpreter working (for test suite)
- [x] 'crash' w/ line no for option unwrap
- [x] 'crash' w/ line no for bad enum cast
- [x] Match expected errors from test programs
- [x] 'never' type
  - [x] typechecking / codegen
  - [x] if/else
  - [x] match
- [x] Pattern Matching
  - [x] Single-arm match evals to boolean
  - [x] Literals
  - [x] Variables
  - [x] Optional None
  - [x] Optional Some
  - [x] Make sure binary AND doesn't evaluate its rhs if lhs is false!
  - [x] Records
  - [x] Enums
  - [x] Multi-case
- [x] Enum types (enums / tagged unions)
  - [x] Syntax
  - [x] Construction/repr - [x] Later, pattern match on variants
  - [x] Make first-class types for each variant
  - [x] First, hard cast to variant (`as<.Tag>`)
  - [x] Built-in .as<Tag> or panic
  - [x] syntax: optional pipe separator
  - [x] Enum methods
- [x] Type cleanup
  - [x] Order of elet (topology through defer/skip)
  - [x] named struct and enum fix
  - [x] Newtypes (opaque)
  - [x] Aliases
- [x] Generic structs and enums
- [x] Recursive types
- [x] TypedIf allow exprs instead of requiring blocks (did not do; instead improved handling of unit blocks)
- [x] Remove binding 'if' since we have pattern matching
- [x] Abilities
- [x] Ability impl decl order bugfix (ability impls need to be seen in the decl phase)
- [x] Type Ascriptions
- [x] Enforce unique function name in namespace!
- [x] Optional coalescing binary operator '?'
- [x] Raw Pointer
  - [x] from int64
  - [x] to int64
  - [x] from reference
  - [x] to reference
- [x] sizeOf
- [x] Rest of the int sizes
  - [x] 8
  - [x] 16
  - [x] 32
  - [x] 64
  - [x] reject invalid values
  - [x] implement infallible coercions
  - [x] implement cast
  - [x] hex literals?
  - [x] binary literals?
- [x] Reject too many function args
- [x] Use abilities to implement Bits namespace
- [x] return statement (control flow of blocks / statements: CanReturn, AlwaysReturns, NeverReturns (which is 'never'))
      ^ mixed feelings here as it breaks the fact that 'expr as T' always returns a T...
- [x] Use `as` casting syntax for rawpointer.asUnsafe
- [x] Allow `as` casting syntax for enums to supply only the tag (`result as .Ok` instead of result as `Result<T,E>.Ok`)
- [x] Ability constraints on functions
- [x] Remove custom size/align code and use LLVM's
- [x] More type expressions! (this was a big part of my original point)
  - [x] Intersect structs
  - [x] Union structs
  - [x] inner type of optional
  - [x] return type of function (fn.return)
  - [x] param types of function (fn.arg, fn.arg2)
- [x] Exhaustive pattern matching
- [x] Rework RawPointer to be a builtin and support 'multipointer' operations
- [x] Pipe operator (copy Elixir)
- [x] Rework builtin array to use new Pointer, Remove all array intrinsics and builtin type
  - [x] Add array bounds checking
  - [x] Fix array literal syntax
- [x] Rework builtin string to use new Pointer
- [x] Rework builtin optionals to be a generic enum
- [x] floating point (f32 and f64)
- [x] 'Context' system; implicit stack arguments
- [x] Pass caller source location for assert
- [x] Function types (functions have types but there's no syntax for describing a function type yet)
- [x] Optional coalescing field accessor (x?.y)
- [x] function pointers (By taking the static address of a function as Pointer)
- [x] Finish/fix simple generic inference
- [x] Prevent function overloading in same namespace
- [x] Typecheck the binary ops
- [x] Bitwise ops
- [x] Bitwise ops using abilities
- [x] Closures
  - [x] Direct calls, no captures
  - [x] Support captures, but explicit only
  - [x] Support implicit captures
  - [x] Allow 0 args
  - [x] Allow specifying return type (without typing the closure itself)
  - [x] Return works
- [x] Change reference and dereference syntax to x.& and x.*
- [x] Fix context params when combined with generics
- [x] string interpolation
- [x] typeOf(<anyexpr>)
- [x] Remove coercion from the language (only dereference is left)
- [x] Rework Opaques
  - [x] Just use structs with private fields
- [x] Working with references push, specifically struct and either references
  - [x] A nice syntax for `referenceSet`
  - [x] New assignment operator, not `=` (`<-`)
  - [x] Make EnumGetPayload a valid lhs?
  - [x] Kill .& because so unsound, use let* for those cases
    - [x] Not the worst thing to use let* for the c-interop case of needing a pointer, that's what .& was doing anyway
  - [x] EnumGetPayload on a reference doesn't give a reference (and shouldn't always, what syntax to specify)
  - [x] FieldAccess on a reference doesn't give a reference (and shouldn't always, what syntax to specify)
- [x] Array/Slice/string rework to Buffer/List, and eventually Array (fixed size at compile time)
- [x] Do away with k1lib.c?
- [x] Test Struct shorthand syntax
- [x] Imports
  - [x] types
  - [x] functions
  - [x] constants
  - [x] namespaces
- [x] Ability call resolution rework
  - [x] Remove restriction that first arg is Self by
  - [x] Reworking resolution to find a function by name first, then Solve for 'self' by unifying call and signature, then find the impl
- [x] Fully generic abilities with separate 'input' vs 'output' type parameters
- [x] nocompile(<expr>) intrinsic! yields an actual runtime constant string for assertions?!
       `let result1 = compiler/nocompile(1 + "asdf"); assert(result1.startsWith("Type mismatch"))`
- [ ] Improve LLVM opt pipeline https://www.reddit.com/r/Compilers/comments/1hqmd7x/recommended_llvm_passes/
      https://llvm.org/docs/NewPassManager.html#just-tell-me-how-to-run-the-default-optimization-pipeline-with-the-new-pass-manager
- [ ] More explicit companion ns via injecting `for` keyword `ns (for) type {`
- [ ] BTs via Runtime_Support_Crash_Handler?
- [ ] Namespace stuff
  - [x] Imports via `use`
  - [x] Change keyword to `ns`
  - [x] `namespace <ident>;` to namespace whole file
  - [x] Allow namespace extension via simple multiple blocks of same name in same scope
- [ ] 'call' method syntax (Scala's 'apply' feature)
- [ ] Operator 'overloading' story, +other special function names that work like ==?), make == less special
- [ ] LLVM cleanup
-  [ ] avoid uses of aggregate *values* where we can: so routine uses of 'struct's and 'enum's
-  [ ] Move allocas to entry block. "Doing this is actually quite easy as LLVM provides functions you can use to retrieve the entry block for a function and insert instructions into it."
- [ ] Replace 'unit' with an empty struct, encoded as `{}` at the type level and `{}` at the value level. This would remove a whole base type
- [x] Re-write signature specialization to be simpler.
- [ ] Re-write body specialization to not re-typecheck but instead transform the typed tree
- [ ] Typecheck 'main'
- [ ] Matching on references
  - [ ] Match to get reference to each struct field, for example, use * for a dereferencing match
  - [ ] Match to get reference to enum payload
- [ ] ? operator is really the 'else' operator, can implement using an 'Else' ability or 'Unwrap' ability
- [ ] Context location params are not being propagated
- [ ] Test and fix named arguments
- [x] return from while
- [x] break from while
- [ ] 'never' needs to work in every expression position
- [x] Move tests into fewer files
- [ ] accumulate test errors and support inline test comment assertions when a line should produce a compiler error.
      - Probably one test for failing compilation and one passing one for each major language area
- [ ] b"" strings that are of type Buffer[u8]
- [x] Finish hashmap implementation
- [ ] Introduce Warnings
  - [ ] Unused var
  - [ ] Unused type bound
  - [ ] Disallow naked variable patterns in 'is' OR Disallow capital variables, require capital enum variants...
      - `if self.slots.get(probe_index) is None {`
- [x] Handle escaped chars in string literals
- [x] Friendliness pass
  - [x] Replace 'enum' keyword with 'either', ensure the ambiguous cases have good errors (inside struct, inside param list)
  - [x] 'when' keyword is bad; `switch` maybe or `case`, or resolve the ambiguity with `when <x> is {}`
  - [x] Replace `type` with `deftype` - It would be really nice _not_ to take the keyword 'type'. Just a thought from using Rust/Scala
- [x] Remove tag literals, make enum tags per-enum
- [ ] Add ranges in stdlib
      `sealed abstract class Range(let start: Int, let end: Int, let step: Int)`
- [ ] Make demo readme / site
- [ ] Allow scoped namespace defns; `namespace <ident>/<ident>/<ident> {}`
- [ ] Define clear 'platform layer' (crash, alloc/free, other?). Then we could do an LLVM interp platform and a rust interpreter platform
- [ ] Runtime type info story, typeOf, typeInfo, and 'any' type
- [ ] Ability-based iteration
- [ ] Conditional compile directive
- [ ] Mark types as trivially copyable or not
^ The builtin array would be NOT copyable so that you don't accidentally alias the data ptr
- [ ] Ability constraints on generics

## Non-goals at least for now
- [ ] Memory safety / solving the 'aliasing' problem, not because its unimportant but because I have other interests
- [ ] Tuples. I don't think you need them if you have anonymous structs. The lack of names always makes them
      easy to start using and very hard to maintain / read consumer code.
      "In the beginning all you want is an anonymous tuple and in the end all you want are named fields"
- [ ] Ability derivation (prefer a metaprogram solution)

## Maybe
- [ ] 'join' types to form new enums/structs, statically, like Roc
- [ ] Require unsafe marker when unsafe stuff is used
- [ ] Might be very cool to have builtin syntax for anything implementing a 'Monad' ability
  - (Monad ability would require closures and generic abilities)
- [ ] Require named fncall args by default; and allow anonymous w/ declaration like Jakt?
- [ ] as! for fallible casting and as? for optional casting
- [ ] Test handling of NaN and Infinity, other float edge cases
- [ ] German/Umbra strings

## Compiler
- [ ] LLVM: avoid loading aggregate values directly
- [ ] Convert NamedType to a trait
- [ ] Use smallvec
- [ ] UTF8
- [ ] Intern ParsedBlock and ParsedStatement

## Error story
- [ ] As values, of course.
- [ ] A simple stdlib enum?
- [ ] A '?' operator for early return? We could do an ability for it!

## Memory Management story
- [ ] Figure out the pointer/reference story
  - I want to do a minimal runtime with a user-visible heap, semi-auto memory management. Write the heap and main fn in C, call main from the 'runtime' main, which sets up a basic heap based on settings or something user-visible.
  This will let us intern strings, etc. Kinda like the JVM, except its not
  a VM, just a native runtime.
  - If we introduce simple implicits for context passing, we can use this to pass heaps around
  - Generational References combined w/ arena-style memory mgmt

# Major fix
- [ ] Unmatched closing delim in namespace causes silent failure to parse rest of sources
- [ ] Replace IdentifierId with global 'Symbol' where its a bug not to
- [x] Parsing bug where first expr of block is namespaced with ::
- [x] Parsing bug where `if rest.startsWith("one") .Some(1: u64)` parses as `if rest.startsWith("one").Some(1: u64)`
- [x] ICE when assigning to struct member when struct is not a reference (self.module.types.get(field_access.base.get_type()).as_reference().is_some())
- [ ] Require indirection for recursive types; and make them actually really work

# Minor Fix (possible good bite-sized videos)
- [x] Lexer cleanup > Am I crazy or is this just always tok_buf.len()?!?!?!
- [ ] type suffixes on int literals 123u32, 4u8, etc
- [ ] Inference improvements
  - assert(sizeOf[Text]() == 16 + 32); rhs should infer to u64
- [x] Precedence of dereference (and i guess unary ops in general) should be higher
      Kinda fixed by removing all unary ops except 'not'

# GUI checklist

- [x] Get render loop working with access to module, ability to trigger compile, run
- [ ] Render namespaces, use recursion over namespace for all functionality?
- [ ] Search for a type?
- [ ] One day allow updating or adding a single definition


# Old todo list

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
- [x] Real core so we can more easily add runtime/stl functions (for array)
- [x] Heap memory (just using malloc)
- [x] Arrays (Fixed size but heap allocated)
- [x] Generic functions (no inference)
- [x] Remove / fix TokenIter type
- [x] parens around expressions
- [x] Binary infix operations
- [x] basic error reporting using spans
- [x] Array and Struct member assignment
- [x] While Loop
- [x] Assert (syscall exit? 'panic'?)
- [x] Replace .length on string and array with a MethodCall node (adding empty parens ())
- [x] Change Array repr to a struct w/ length so we have length
- [x] Strings
  - [x] String literals cant have spaces lol
  - [x] print(string)
  - [x] hardcoded via codegen
  - [x] string.length
  - [x] string[i] (int)
  - [x] char
  - [x] string[i] (char)
  - [x] add string.length function
  - [x] char.to_string()
- [x] Concatenate strings (in userland; slow)
- [x] Infer let types
- [x] Extern keyword, then
- [x] Link at build time not via LLVM module
- [x] Use ctx.const_string for printf format strings
- [x] Optionals
- [x] Improve printing output by fixing display functions
- [x] Implement builtin growable array
- [x] Use aggregates in codegen, not pointers, for optionals
- [x] Basic generic type inference in function calls
- [x] Pretty print scopes
- [x] Support multiple files
- [x] Fix namespaces to require full paths (currently busted must be unique names globally)
- [x] Allow namespaces inside namespaces
- [x] Some debug info in LLVM IR (source snippets or line numbers?)
- [x] uint type
- [x] Generic type inference
- [ ] Type literals would be fun
- [ ] Syntax Shed
  - [ ] Function syntax change? (foo = fn (a, b, c): int { }
  - [ ] from qualifier-focused to name -focused
  - [ ] Parse trailing commas

- [-] For iteration (we will just hardcode the iterable types)

  - [x] 'do' version
  - [x] 'yield' version on array
  - [x] 'yield' version on string
  - [x] Provide index var (it_index)

- [x] DEBUG info
  - [x] on or off
  - [x] flag types
  - [x] correct line nums
  - [x] correct spans (depends on multifile)
  - [x] correct file path
  - [ ] Add lexical scopes for if and while

# Optionals

- [x] Optional types
- [x] None types
- [x] None value
- [x] Runtime repr for boolean
- [x] Runtime repr for unit
- [x] Runtime repr for int
- [x] Runtime repr for char
- [x] Runtime repr for string
- [x] struct
- [x] array of optional structs

# Optionals Round 2

- [x] move some-wrapping into a function
- [x] has value from userland (currently only happens by desugaring))
- [x] unwrap from userland (I think this can just be a core function not intrinsic)
- [x] test with optional array elements
- [x] test with optional struct fields
- [x] test with optional function args
- [x] Explicit Some() expression

# Dogfood wishlist Oct23

- [x] I really need a way to write code in Zig or C and use it in my stl to move things along
- [x] Zig binding of my Array
- [ ] Early return
- [x] Implicit return of unit if no return statement
- [x] string.indexOf
  - charToString implemented in zig and linked
- [ ] Array.distinct in zig
- [x] Not equal != operator
- [x] Unary negation operator

# Fibonacci todos (Aug 23)

- [x] Equality binop
- [x] Precedence of binops; parens?
- [x] Recursion fix: process module decls first, then impls
- [x] Negative integer literals
- [x] PHI nested branch fix

# Hacks to fix

- [ ] main entrypoint properly. Typing of main? How to get main args?
- [ ] Replace vectors with smallvec where appropriate
- [x] Fix line comments
- [x] Maybe eventually actually free some memory? lol
- [x] Rename IR to typed-ast, since it's a tree not instruction set. TAST?
- [x] Make intrinsics like arrayIndex a real function in the LLVM IR?
- [x] Fix line numbers to account for core
- [x] Fix unnecessary load of function args
- [x] Proper println implementation. Fine to use printf internally for now but we should define our own func around it
- [x] Implement Display instead of relying on Debug

# Dev ex

- [x] Pretty-print AST
- [x] Error Spans
- [x] fancy output COLORS

# Maybe later

- [x] Type Params

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
