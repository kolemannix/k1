`k1` is like C with typeclasses, full compile-time execution, ADTs\*, capturing lambdas, pattern matching, next-generation metaprogramming, and a modern generic typesystem

Core ideas
- The compiler should be uncompromisingly fast
- The generated code should be as optimal as possible (No always-on costs like GC, dynamic dispatch, or any sort of runtime)
- Compile-time execution and reflection enables powerful metaprogramming that is just regular programming.

Check out the [TODO](TODO.md) for a glimpse into the development flow, or the [test_src/](test_src/) dir to see what the language can currently do!

\*structs for product types and first-class tagged unions for sum types via the `either` keyword

# About the project

Just exploring the PL dev world by designing and implementing a toy language. The goal is to implement all the basics,
and also explore some interesting ideas. I am trying not to have any aspirations for this project
except to explore the problem space of compiler development, and maybe to be able to do some of advent of code 2023 in this language.

Inspiration


> The programmer, like the poet, works only slightly removed from pure thought-stuff. He builds his castles in the air, from air, creating by exertion of the imagination. Few media of creation are so flexible, so easy to polish and rework, so readily capable of realizing grand conceptual structures.... Yet the program construct, unlike the poet's words, is real in the sense that it moves and works, producing visible outputs separate from the construct itself.
- Brooks

> The way to fall asleep is by pretending to be someone who is asleep. And that's how everything works
- Someone on twitter

> You can just do things
- Popular memes archive, 2024

https://justforfunnoreally.dev/

Heroes
- Andreas Kling
- Jonathan Blow
- Ken Thompson
- Chris Lattner
- Bjarne Stroustrup
- [Rob Pike](https://docs.google.com/presentation/d/e/2PACX-1vSmIbSwh1_DXKEMU5YKgYpt5_b4yfOfpfEOKS5_cvtLdiHsX6zt-gNeisamRuCtDtCb2SbTafTI8V47/pub?start=false&loop=false&delayms=3000#slide=id.p)

## Example

Let's add a feature to `k1` using metaprogramming to support bitfields. I'd like to be able to define a bitfield by providing a name, a 'base' integer type
big enough to house all the fields I provide, and a series of fields as a collection of (name, bit width) pairs.
We can encode this in K1 easily enough as a function:

```k1
fn bitfield[Base](typeName: string, members: View[{ name: string, bits: size}]): ???
```
We use a type parameter, denoted in square brackets, to track our 'base type', which
should be an unsigned integer type, which we can enforce later. `k1` provides u{8|16|32|64}.

We also accept a name to attach to whatever constructs we end up generating for our flags.
Perhaps functions to get and set them, maybe some constants? Not sure yet.

That leaves the return type, which we've left as `???`. Ultimately we'd like to produce
some code; most text-based programming languages are very well-represented and maintained in
a particularly powerful format: source code text. So we'll return a `string`,
(not a `org.sys.meta.macros.whitebox.TTreeSyn.Quoted`) containing the K1 code we'd otherwise have hand-written for our bitfield.

```k1
fn bitfield[Base](typeName: string, members: View[{ name: string, bits: size}]): string
```

Let's just implement this function as we would any other program.

```rust
fn intWidthForBits(bits: size): size {
  if bits <= 8 8
  else if bits <= 16 16
  else if bits <= 32 32
  else if bits <= 64 64
  else crash("Too many bits: {bits}")
}
fn bitfield[Base](typeName: string, members: View[{ name: string, bits: size }]): string {
  use core/StringBuilder;
  use meta/CodeWriter;
  let baseTypeId: u64 = types/typeId[Base]();
  let base = types/typeSchema(baseTypeId);

  // We can exit the program, which will ultimately become a compilation error
  require base is .Int(intKind) else { crash("Base should be an int") };
  let totalBits = intKind.bitWidth();
  let baseName = types/typeName(baseTypeId);

  let* code = StringBuilder/new();

  // We could also use the builtin typeFromId(...); but I like the short name and we're restricted it u{n} here
  code.line("deftype {typeName} = {{ bits: {baseName} }");

  code.line("ns {typeName} {{");

  // Let Masks (I can also store inverse mask strings here: 0b111...)
  let* bitIndex = 0;
  bitIndex;
  let memberInfo: List[
  { mask: usize, invMask: usize, typeWidth: size, rawValue: usize, offset: size, nameCap: string }] = List/fromMap(members, \member. {
    let bits = member.bits;
    if bitIndex.* + bits > totalBits crash("Too big: {bitIndex.* + bits}");
    let rawValue = u64/bitmaskLow(bits);
    let typeWidth = intWidthForBits(bits);
    let nameCap = member.name.capitalizeAscii();
    let mask = rawValue.shiftLeft(bitIndex.* as u32);
    let offset = bitIndex.*;
    let sizeMask = u64/bitmaskLow(typeWidth);
    let invMask = mask.bitNot().bitAnd(sizeMask);

    // println("{member.name}: typeWidth: {typeWidth}");
    // println("{member.name}: raw value: {rawValue}");
    // println("{member.name}: mask: {mask}");
    // println("sizeMask: {sizeMask}");
    // println("invMask: {invMask}, bits: {invMaskBits}");
    bitIndex <- bitIndex.* + bits;

    // mask: the positioned mask; 0b100
    // rawValue: the mask shifted to the start, the 'magnitude' of the used bits
    // offset: the bit pos that the mask starts at; where the value lives
    { mask, invMask, typeWidth, rawValue, offset, nameCap }
  });

  for members {
    let info = memberInfo.get(itIndex);
    code.line("  let {it.name}Mask: {baseName} = {info.mask};");
  };

  code.line("  let zero: {typeName} = {{ bits: 0 };");

  for members {
    let info = memberInfo.get(itIndex);
    let nameCap = info.nameCap;
    let isBool = it.bits == 1;
    let fieldType = if isBool "bool" else "u{info.typeWidth}";

    // Getter
    code.line("  fn get{nameCap}(self: {typeName}): {fieldType} {{");

    code.line("    let bits: {baseName} = self.bits;");
    code.line("    let shifted: {baseName} = bits.shiftRight({info.offset});");
    code.line("    let cleared: {baseName} = shifted.bitAnd({info.rawValue});");
    if isBool {
      code.line("    cleared == 1")
    } else {
      code.line("    cleared as {fieldType}");
    };

    code.line("  }"); // End Getter

    // Setter
    code.line("  fn set{nameCap}(self: {typeName}, value: {fieldType}): {typeName} {{");
    if isBool {
      code.line("    let maskedValue = (value.as_u8());")
    } else {
      code.line("    let maskedValue: {baseName} = value.bitAnd({info.rawValue});")
    };
    let clearMask = info.invMask;
    code.line("    let clearedBits: {baseName} = self.bits.bitAnd({clearMask});");
    code.line("    let insertedBits: {baseName} = clearedBits.bitOr(maskedValue.shiftLeft({info.offset}));");
    code.line("    {{ bits: insertedBits }");
    code.line("  }"); // End setter
  };

  code.line("}"); // End ns

  let* printBody = StringBuilder/new();
  for members {
    let info = memberInfo.get(itIndex);
    let getName = "get{info.nameCap}";
    let last = itIndex + 1 == members.len();
    let sep = if last "" else ",";
    printBody.line(`    w.writeString("{it.name}={{self.{getName}()}{sep}");`);
  };
  code.implPrint(typeName, printBody.build(), indent = 0);

  let s = code.build();
  println(s);
  s
}
```


### Type System
- Strong Static Typing with limited, fast, and predictable type inference
- Algebraic Data Types via `either` for tagged unions and structs for product types
- Generics with support for type constraints through abilities (traits)
- Deeply expression-oriented 
  - `loop` with `break(<expr>)`, 
  - `if` expressions that support a single pattern
  - A `never` type that allows simplifying types while handling cases
- First-class Optional Types with ergonomic `?` and `?.` operators and pattern matching
- Reference Types with distinct pointer and reference semantics
- Anonymous structs and enums allow for lightweight, low-boilerplate, zero-cost data modeling
- Zero-cost Abstraction through opaque type aliases, and zero-overhead structs

### Modern Features
- Pattern Matching with exhaustiveness checking and useless pattern detection
- Closures with automatic capture analysis and environment generation
- Type-safe String Interpolation using `\{...}` syntax
- Iterator Protocol with `for` expressions supporting `yield` and `do` blocks
- Method Syntax with namespaced scoping
- Pipeline Operator (`|`) for functional composition
- Abilities (traits/interfaces) for type-class like abstraction

### Systems Programming facilities
- Direct Memory Management with `Pointer` operations and introspectable type layouts
- Foreign Function Interface through external function declarations
- Bit Manipulation operations
- Fixed-Size Integer Types (u8/i8 through u64/i64)
- Platform Integration through libc bindings

### Limited WIP Standard Library
- Generic Collections including Array and HashMap implementations
- String Utilities with builders, interpolation, and efficient operations
- Option Type with ergonomic methods and pattern matching
- Numeric Tower supporting integers, floats with standard operations
- Runtime Error Handling with basic crash reporting

### LLVM Backend
- Efficient Machine Code through LLVM's optimization pipeline
- Zero-Cost Abstractions with aggressive inlining and dead code elimination
- Flat Memory Layout for structs and tagged unions
- Debug Information generation using DWARF format
