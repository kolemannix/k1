"C with typeclasses and tagged unions"

IDEAS 11/13
A let* that goes to the heap, mark/reset on function return. (function arenas)
Similarly, put lambda environments in the arena
Inline all small functions in typer or bc
AbilitySignature as context variable kind in addition to Type (enables context Writer, context Mem)
- let context(impl Alloc) temp = mem/AllocMode.Arena;
- let context(impl Iterator[string]) temp = mem/AllocMode.Arena;
- language level hot reload support. TWEAK_FLOAT(f) thing. Explore this and find out if language support really helps or if it can just be solved by library
Specialization solution (when types are known by the function)
- A kind of *pattern* that checks the type and binds a variable of that type! What other thing
  for a feature that needs to _check_ and _bind_ than a pattern?!
fn write[T](t: View[T]) {
    if t is type[View[u8]](binding) {
      // `binding` is of byte View[u8]
    } else {

    }
}

More optimal final programs
- [ ] Represent payload-less `either` types as ints not structs (Actually might just add enum as separate thing from eithers)
- [ ] Add 'switch' to bytecode; compile switches with no patterns or guards to LLVM switch
- [ ] Unit syntax of '()' makes no sense when we don't have tuples. What about `{}`

Non-major Ideas
- [ ] c"" string literals that are of type ptr (what about interpolation?)
- [ ] userland: CCompatString which is a valid c string with prefixed length
- [ ] Implicit conversions: based on a special ability that integrates with 
      type inference? (like Mojo's ImplicitlyIntable, etc)
- [ ] "Flags in Tags"
- [ ] Support "base-2-shifted" enum tags, allowing for set-like logic on variants:
      if tags go 1,2,4,8, then we can make a mask for, 1 and 4, instead of matching or writing predicate functions (See Andrew Reece; BSC 2025; Assuming as much as possible)
      either(u32, set|tagset|bitfield)? Gives you a few 'free (jumpless)' predicates per enum!
  - [ ] First-class data-oriented design features for struct/enum setups?
      base2 tags, ["encoding approach"](https://www.youtube.com/watch?v=IroPQ150F6c),
      
      'shared' payload type, AoS or AoSoA by default...
- [ ] Dogfood idea: 'niched' integer abstraction (-1 as 'not found' but safely, vs using option and wasting space + adding more code)
      `impl Unwrap<Inner = u32> for { hidden: i64 }`

Syntax/elegance
- [x] Get rid of the name 'Unwrap'; twitter is right about that one. Good opportunity
      to produce a very strong name for this concept
- [ ] Default type args for abilities, or partially applied abilities (alias Unwrap[T] = Try[T, unit])
- [ ] Replace the builtin for ... yield with a userspace function taking a lambda
- [ ] Need a syntax that takes an interpolated string but writes it to a Writer that you already have
 - [ ] Also need positional format args as well (probably just our userland printf finished out)
- [x] Consider a rename of 'uword/iword'; they do not feel good to use. What about `u` and `i`.
  - [x] Ok now I'm really thinking about `size` and it being signed.
  - [x] Also: do safe integer coercions automatically

Simple but missing
- [x] Support ability constraints on generics
- [ ] Support explicit type args in AnonEnumConstructor syntax 
- [ ] implement iterator for Array
- [ ] Allow scoped namespace defns; `namespace <ident>/<ident>/<ident> {}`

- [x] META test: Can we build ArrayOfStructs using current metaprogramming?!
- [ ] Bindings generator; `rust-bindgen` equivalent

# From dogfood round
- [x] Introduce an "uninitialized" specifier, similar to `zeroed()`
- [x] Add 'zeroed()' static value special case for efficiency?
- [x] provide a way to specify if globals are comptime available or just runtime globals?
      I accidentally wrapped a global defn in #static...

# Bugs
- [ ] Defect: Generic (co)recursive types do not work
- [ ] We should require that a blanket impl's params appear in the Self type
- [-] Limitation (ordering): ability impls have to be provided in dependency order, since their constraints can depend on each other. I think I have to do a
                             'skip and progress' style of pass for them to prevent that. It possibly not worth the complexity

## Project: Actual modules, library vs binary compile, allow linker options
- [ ] Separate modules
  - [x] Introduce 'module' w/ kind (lib/bin/core), deps, and namespace+scope
  - [x] Add entire modules from TypedProgram
  - [x] Module manifests somewhere
  - [x] Library vs Binary
  - [ ] **Prevent modules using definitions from modules they dont depend on (implicit transitive dependency problem)**
  - [x] Dependencies: local module
  - [x] Specify linked libraries in manifest (Eventually this will need to be more customizable)
  - [ ] serialize typedprogram at each module completion (for incremental compilation)
  - [ ] Support ".c" sources; compiles your c and adds it to the main compilation unit
- [x] clang passthrough options, when do we 'link', in IR or as object files, ...
  - [x] we 'link' with k1 in the typer's modules system
  - [x] we link with other deps w/ the linker

## Project: Instruction-level IR ('bytecode')
Primarily an execution target for the VM, but also would DRY up the significant duplication between the two current backends, LLVM and k1::vm.
- [x] Try to compile a function to bytecode
- [x] Draw the rest of the owl (done)

## Project: Optimize StaticValue representation for aggregates to be the same as the VM representation
- [-] Real layouts, in a mem pool, to save roundtripping and increase locality
        -> Kinda done, we now re-use the values in a global 'static stack'

## Project: Arena-based core, builtins, stdlib 
- [x] Thread-local globals
- [ ] transmute function (for struct ABI workarounds right now)

## Project: Ability objects; dyn[<ability expr>]

## Project: Static Improvements
- [ ] Collapse long runs of zero-only data into a single one in LLVM IR (e.g., mem/allocStack)
- [-] static #for, special-case like IF. Can unroll the loop at comptime but the body is runtime
- [ ] functions taking only a single type could be invoked with a nice syntax like `type.sizeOf`
- [x] VM "PermSpace" for caching converted static values in their VM representation
- [x] Add StaticValue::Zero as an efficient special-case (generalization of the existing NullPointer, actually)
- [ ] 'Type predicate' functions as type bounds

## Project: aarch64 struct passing ABI
## Project: x86-64 struct passing ABI

## Introduce Warnings
- [x] Unused var
- [ ] Unused type bound
- [ ] Disallow naked variable patterns in 'is' OR Disallow capital variables, require capital enum variants...
    - `if self.slots.get(probe_index) is None {`

## Project: Zero-Sized Types
- [ ] Treat Unit and empty Struct as ZSTs
- [ ] Treat statics as ZSTs
- [ ] Make Result[T, never] == T ?

## Project: system interface, 'Write' ability and intrinsic fix.

## Project: More LSP features
- [x] Hover first pass
- [ ] Hover much better
- [ ] Go-to
- [ ] Completion

## Project: VM for `static` execution
- [x] vm: static execution
  - [x] Order-independence for globals used in static code
  - [x] Static Buffers (slices)
    - [x] vm -> static
    - [x] static -> vm
    - [x] LLVM gen
  - [x] reference to reference cast
  - [x] Introduce uword/iword types
  - [x] Switch to a single stack
  - [x] Move to intrinsic: system core memory functions
  - [x] Move to intrinsic: memcpy/memmove
  - [x] Allow 'write's from static code
  - [x] Move to intrinsic: `exit`
  - [x] Run global initializers before most bodies but after all other phases, treat it
        like 'body' code SINCE it'll end up using the user's types, and even impls!
  - [x] Define clear 'platform layer' (crash, assert, mem, other?).
      Then we could do an LLVM interp platform and a rust interpreter platform
  - [x] All tests passing in #static mode
  - [x] Allow upgrading static buffers to fixed-length Arrays (so cool actually)

## Project: Mutable and non-mutable reference types
- [x] Change pointer syntax: `*<ty>`, `*<ty>`, `*mut <ty>`
- [x] Rename 'Buffer' to ... View?
- [x] Convert reference type syntax to prefix for better chaining
- [x] Convert option to prefix syntax
- [x] Add mutable/const bool to ReferenceType
- [x] Update stdlib

## Project: Metaprogramming system built on 'static': both string #insert and parsed code #insert, like Jai
- [x] #meta First working version
- [x] Multiline string literals,
- [x] #code directive
- [x] static type universe: `static T`
- [x] A syntax for talking about a certain impl of an ability: `Show::bool/show(b: bool)` or `(Allocator for T)/supportsFree()`

## Project: Metaprogramming round 2
- [x] Finish StructOfArrays builder
- [x] Provide in-file 'meta' module for convenience
      (it gets pre-compiled as its own module so that you can write and use functions for metaprogramming)
- [x] Provide a specialized StringBuilder and suite of helpers, CodeBuilder?

## Project: Array types
- [x] Add fixed length array types: `Array[<type expr> x <int literal>]`

## Project: Defer
- [x] Defer

## Project: static reflection
- [x] Runtime type info story
- [x] typeOf, typeId
- [x] TypeSchema for all types
- [x] Test 'Any' type

## Project: Operator overloading
- [x] Operator 'overloading' story. I think the story is just abilities.
        This will actually fix the really poor inference that binary ops currently have
- [x] Start with Equals
- [x] Do add
- [x] Move all the binary operations to intrinsic calls; and remove BinaryOp from the Typed AST

## Working list (early 2025)
- [x] String pool for string values, not just identifiers (will dedupe in LLVM too)
- [x] Adopt `ecow`'s EcoVec
- [x] reproduce shadow bug: only when statically run?!
      buffer.slice: `let end = if end > self.len self.len else end;`
- [x] Fix referencing match not 'eliminating' patterns on `struct*` giving unhandled pattern `.CustomHeap({ zalloc }*) -> {`
- [x] Optimize lambdas that don't capture to become function pointers instead
- [x] comptime #if needs to be a real node not a directive (can't parse if/else). More like `#const if` than `#if`
- [x] string interp at end puts unnecessary empty string part: `putString(__sb_1001, "");`
- [x] Backend codegen cleanup
-  [x] avoid uses of aggregate *values* where we can: so routine uses of 'struct's and 'enum's
-  [x] Move allocas to entry block. "Doing this is actually quite easy as LLVM provides functions you can use to retrieve the entry block for a function and insert instructions into it."
    (handled by optimization passes for now)
-  [x] Upgrade to LLVM 18
- [x] Context location params are not being propagated
- [x] Test and fix named arguments
- [x] 'never' needs to work in every expression position (got close enough, might add more if one comes up)
       -> Originally did the wrong way, plumbing special cases. Instead now we just only generate the crashy expr
- [x] accumulate test errors and support inline test comment assertions when a line should produce a compiler error.
      - Probably one test for failing compilation and one passing one for each major language area
- [x] Add simple int range in stdlib
- [x] Fix enum codegen, read Inko llvm backend (its inkwell + rust and does ABI compatible stuff https://yorickpeterse.com/articles/the-mess-that-is-handling-structure-arguments-and-returns-in-llvm/)
- [x] Conditional compile directive
- [x] Support boolean operators in compile time expressions
- [x] Change FieldAccess semantics to work on struct references, and copy only the field out
      This saves copying the entire aggregate first with a Dereference instruction
- [x] ThreadLocal globals
- [x] LLVM Codegen callstack is too deep due to codegen_function_or_get
- [x] Switch VM stack to a single virtual allocation https://crates.io/crates/memmap2
- [x] Improve LLVM opt pipeline https://www.reddit.com/r/Compilers/comments/1hqmd7x/recommended_llvm_passes/
      https://llvm.org/docs/NewPassManager.html#just-tell-me-how-to-run-the-default-optimization-pipeline-with-the-new-pass-manager
- [x] Stacktraces on crash (using libunwind and a little C program to call it: `rt/unwind.c`)
- [-] Write a 'validateTypedModule' procedure. This need is lessened by the VM which in a way typechecks the TAST
      This is basically an interpreter; what we have now with the vm solves this problem a bit
      But not entirely because it only checks the code that runs!

# General (late 2024)
- [x] Matching push
  - [x] boolean chains w/ binding ifs
  - [x] Don't codegen conditions for arms that don't run
  - [x] Remove 'statement conditions'
  - [x] Prevent shadowing
  - [x] Rewrite codegen for match to allow for better control flow
  - [x] 'if' guards on regular match
  - [x] Move pattern bindings for field access and enum payload back to variables to fully remove duplication (we can do this now that we have a place to put them that's per-arm)
  - [x] Look into converting 'matching if' to also compile to a TypedMatch
  - [x] Binding `while`
  - [x] Matching on references
    - [x] Match to get reference to each struct field, for example, use * for a dereferencing match
    - [x] Match to get reference to enum payload
  - [x] ASSERT FAILED: true != true at core.k1:23
- [x] Real type inference
  - [x] True inference variables, instantiate function types, unification and consistency checks, aka make it work
  - [x] Make 'crash' work in no-std
  - [x] Make it pretty (de-coupled inference hole from type variable)
  - [x] Move enum constructor onto new inference infra
  - [x] Move ability resolution onto new inference infra
  - [x] Make it fast (Added better 'pool' to prepare for avoiding lots of allocations)
- [x] Fix closure types / get static dispatch for functions taking a closure directly
- [x] *Specializing functions on their provided closures to allow inlining and static dispatch*
- [x] Run in Linux x86-64
- [x] `require` statements with matching and binding
- [x] Runtime-branching Allocator system (v2 is comptime branching)
  - [x] comptime enhancement to support this global initializer: `let* a: Arena* = { .. };`
  - [x] Global pointers, to enable
  - [x] The problem with passing an allocator around is all code becomes generic, or casts a pointer.
  - [x] Comptime structs!
  - [x] alloca in loops fix
  - [x] Parameterize stdlib over the current allocator ()
- [x] Proper basic comptime
  - [x] Rename to 'static'
  - [x] Move to stack-based VM
  - [x] literals
  - [x] if/else
  - [x] Arith
  - [x] Struct construction
  - [x] Struct field access
  - [x] Enum construction

## Non-goals at least for now
- Memory safety / solving the 'aliasing' problem, not because its unimportant but because I have other interests
- Tuples. I don't think you need them if you have anonymous structs. The lack of names always makes them
      easy to start using and very hard to maintain / read consumer code.
      "In the beginning all you want is an anonymous tuple and in the end all you want are named fields"
- Ability derivation (prefer a metaprogram solution)

## Won't do
- [ ] Replace 'unit' with an empty struct, encoded as `{}` at the type level and `{}` at the value level. This would remove a whole base type
      This simplification comes at the cost of making empty struct quite special, so I think it's a sidegrade. Won't do
- [ ] 'call' method syntax (Scala's 'apply' feature)

## Ideas
- [ ] 'join' types to form new enums/structs, statically. `switch {strict|dynamic} ...`? If dynamic, I'll build a sum or product based on the branches' types
- [ ] Might be very cool to have builtin syntax for anything implementing a 'Monad' ability
  - (Monad ability would require closures and generic abilities, which we now have. Just need higher order type params `F[_]`
- [ ] Require named fncall args by default; and allow anonymous w/ declaration like Jakt?
- [ ] as! for fallible casting and as? for optional casting
- [ ] Test handling of NaN and Infinity, other float edge cases
- [ ] Try to encode prefix strings, aka German/Umbra strings

## Compiler
- [x] LLVM: avoid loading aggregate values directly
- [x] Convert NamedType to a trait
- [x] Use smallvec
- [x] UTF8
- [ ] Test multi-byte characters (emoji, other)
- [x] Intern ParsedBlock and ParsedStatement

## Error story
- [x] As values, of course.
- [x] A simple stdlib enum?
- [x] A '?' operator for early return? We could do an ability for it!

## Memory Management story
- [ ] Semi-auto, mostly arenas, perhaps 2 global ones, 'perm' and 'tmp' via thread-locals, standard library built around them

# Major fix
- [x] Unmatched closing delim in namespace causes silent failure to parse rest of sources
- [x] Parsing bug where first expr of block is namespaced with ::
- [x] Parsing bug where `if rest.startsWith("one") .Some(1: u64)` parses as `if rest.startsWith("one").Some(1: u64)`
- [x] ICE when assigning to struct member when struct is not a reference (self.module.types.get(field_access.base.get_type()).as_reference().is_some())
- [ ] Require indirection for recursive types; and make them actually really work

# Minor Fix (possible good bite-sized videos)
- [x] Lexer cleanup > Am I crazy or is this just always tok_buf.len()?!?!?!
- [ ] Binary op inference improvements
  - assert(sizeOf[Text]() == 16 + 32); rhs should infer to u64
- [x] Precedence of dereference (and i guess unary ops in general) should be higher
      Kinda fixed by removing all unary ops except 'not'

# GUI checklist

- [x] Get render loop working with access to module, ability to trigger compile, run
- [ ] Render namespaces, use recursion over namespace for all functionality?
- [ ] Search for a type?
- [ ] One day allow updating or adding a single definition

### 2024 

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
- [x] Blanket ability implementations
- [x] nocompile(<expr>) intrinsic! yields an actual runtime constant string for assertions?!
       `let result1 = compiler/nocompile(1 + "asdf"); assert(result1.startsWith("Type mismatch"))`
- [x] Migrate ? to use an ability
- [x] Add "or return error" operator based on an ability
- [x] Convert try to postfix: hello.try (hearkening to .await)
- [x] ! operator should now just call Unwrap.unwrap()
- [x] Bug: if a blanket impl fails to typecheck, we should not use it. Currently we ice trying to instantiate it 
- [x] Migrate `for` loops to use a core Iterator ability
- [x] Namespace stuff
  - [x] Imports via `use`
  - [x] Change keyword to `ns`
  - [x] `namespace <ident>;` to namespace whole file
  - [x] Allow namespace extension via simple multiple blocks of same name in same scope
- [x] Re-write signature specialization to be simpler.
- [x] return from while
- [x] break from while
- [x] Move tests into fewer files
- [x] Finish hashmap implementation
- [x] Handle escaped chars in string literals
- [x] Friendliness pass
  - [x] Replace 'enum' keyword with 'either', ensure the ambiguous cases have good errors (inside struct, inside param list)
  - [x] 'when' keyword is bad; `switch` maybe or `case`, or resolve the ambiguity with `when <x> is {}`
  - [x] Replace `type` with `deftype` - It would be really nice _not_ to take the keyword 'type'. Just a thought from using Rust/Scala
- [x] Remove tag literals, make enum tags per-enum
- [x] Ability-based iteration

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
