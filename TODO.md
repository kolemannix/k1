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
  - [x] Order of eval (topology through defer/skip)
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
- [ ] Context location params are not being propagated
- [ ] Fix named arguments
- [x] return from while
- [x] break from while
- [ ] 'never' needs to work in every expression position
- [ ] Move tests into single file, accumulate errors and allow inline assertions when a line should error.
      Probably one tests for failing compilation and one passing one for each major language area
- [ ] Remove lvalue-based assignment from the language, use references to assign instead, introduce a desugaring for `referenceSet` (`set a := b`)
      - lvalue-based assignment stuff is just an abomination from an algebraic perspective, and probably inhibits learning
- [ ] b"" strings that are of type... Slice[u8]
- [ ] Builtin syntax for maps w/ Map[K, V]
- [ ] Warnings/Errors
  - [ ] Unused var
  - [ ] Unused type bound
  - [ ] Disallow naked variable patterns in 'is' OR Disallow capital variables, require capital enum variants...
      - `if self.slots.get(probe_index) is None {`
- [x] Handle escaped chars in string literals
- [ ] Friendliness pass
  - [ ] Move to single '=' for equality
  - [ ] Move to single ':=' for assignment / definition? Or just space; `let x 42`, `set x 52`
  - [ ] Replace 'enum' keyword with 'either', ensure the ambiguous cases have good errors (inside struct, inside param list)
  - [x] 'when' keyword is bad; `switch` maybe or `case`, or resolve the ambiguity with `when <x> is {}`
  - [x] Replace `type` with `deftype` - It would be really nice _not_ to take the keyword 'type'. Just a thought from using Rust/Scala
- [x] Remove tag literals, make enum tags per-enum
- [ ] Imports (need design around modules)
- [ ] Add ranges, as stdlib thing
      `sealed abstract class Range(val start: Int, val end: Int, val step: Int)`
- [ ] Make demo readme / site
- [ ] Generic abilities (ability From<T> { ... })
- [ ] Define clear 'platform layer' (crash, alloc/free, other?). Then we could do an LLVM interp platform and a rust interpreter platform
- [ ] RTTI story and 'any' type
- [ ] Ability-based iteration
- [ ] Mark types as trivially copyable or not
- [ ] Conditional compile directive
^ The builtin array would be NOT copyable so that you don't accidentally alias the data ptr
- [ ] German/Umbra strings
- [ ] Ability constraints on generics (not strictly needed since we have them on functions)
- [ ] Do away with k1lib.c?
- [ ] Test handling of NaN and Infinity, other float edge cases

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

## Compiler
- [ ] LLVM: avoid loading aggregate values directly
- [ ] Use smallvec
- [ ] Invoke LLVM from in-memory repr?
- [ ] UTF8
- [ ] Intern ParsedBlock and ParsedStatement
- [ ] Think about introducing type 'kind' and check_kind next time we need to typecheck on shape but not _really_ typecheck such as when inferring

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
- [ ] Parsing bug where `if rest.startsWith("one") .Some(1: u64)` parses as `if rest.startsWith("one").Some(1: u64)`
- [x] ICE when assigning to struct member when struct is not a reference (self.module.types.get(field_access.base.get_type()).as_reference().is_some())

# Minor Fix (possible good bite-sized videos)
- [x] // FIXME: Am I crazy or is this just always tok_buf.len()?!?!?!
- [ ] type suffixes on int literals 123u32, 4u8, etc
- [ ] Inference improvements
  - assert(sizeOf[Text]() == 16 + 32); rhs should infer to u64
  - expected type not passed to this else branch (involves generics + some lifting though)
        if rest.startsWith("one") Opt.Some(1: u64) else
        if rest.startsWith("two")   2          else
        > else branch type did not match then branch type: Invalid generic type param: Expected u64 but got i64
- [ ] Require indirection for recursive types
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
- [x] Infer val types
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
