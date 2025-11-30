For the joy of programming

`k1` is like C with 
- typeclasses
- full compile-time execution
- algebraic data types
- capturing lambdas
- pattern matching
- semi-automatic memory management
- next-generation metaprogramming
- modern generics

Tenets
- Programming be joyful
- Tools should empower not restrict (unless elected)
- A compiler should be uncompromisingly fast; feedback loop is tantamount to quality
- The generated code should be as fast as your computer is, not as fast as the runtime is. This is another way of saying its native, but more than that: No always-on costs like GC, dynamic dispatch, or any sort of runtime)
- Compile-time execution and reflection enables powerful metaprogramming that is just regular programming.

Check out the [TODO](TODO.md) for a glimpse into the development flow, or the [test_src/](test_src/) dir to see what the language can currently do!

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
fn bn(name: string, bits: size): { name: string, bits: size } { { name, bits } }
fn b1(name: string): { name: string, bits: size } { bn(name, 1) }
fn b8(name: string): { name: string, bits: size } { bn(name, 8) }
fn define[Base](typeName: string, members: View[{ name: string, bits: size }]): string {
  use core/StringBuilder; use meta/CodeWriter;

  let baseTypeId: u64 = types/typeId[Base]();
  let baseSchema = types/typeSchema(baseTypeId);
  let baseName = types/typeName(baseTypeId);
  require baseSchema is .Int(intKind) else { crash("Base should be an int; got {baseName}") };
  let totalBits = intKind.bitWidth();

  let* code = StringBuilder/new();
  // Could also use typeFromId; but I like the short name and we're restricted it u{n} here
  code.line("deftype {typeName} = {{ bits: {baseName} }");

  code.line("ns {typeName} {{");

  let* bitIndex = 0;
  let memberInfo: List[
  { mask: usize, typeWidth: size, invMask: usize, rawValue: usize, offset: size, nameCap: string }] = List/fromMap(members, \member. {
    let bits = member.bits;
    if bitIndex.* + bits > totalBits crash("Too big: {bitIndex.* + bits}");
    let rawValue = u64/bitmaskLow(bits);
    let typeWidth = intWidthForBits(bits);
    let nameCap = member.name.capitalizeAscii();
    let mask = rawValue.shiftLeft(bitIndex.* as u32);
    let offset = bitIndex.*;
    let sizeMask = u64/bitmaskLow(typeWidth);
    let invMask = mask.bitNot().bitAnd(sizeMask);

    bitIndex <- bitIndex.* + bits;

    // mask: the positioned mask; 0b100
    // typeWidth: the smallest type that can hold these bits; see `intWidthForBits`
    // invMask: the negation of the mask, but also masked down to typeWidth size (to generate constants that fit)
    // rawValue: the mask shifted to the start, the 'magnitude' of the used bits
    // offset: the bit pos that the mask starts at; where the value lives; how much to shift
    // nameCap: the name of the field, capitalized
    { mask, typeWidth, invMask, rawValue, offset, nameCap }
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

  code.build()
}
```

Our bitfield function returns k1 code that defines a struct wrapping our type, in this case a `{ bits: u16 }`, which
is physically just a u16, and various functions for setting and retrieving the bitfields we requested. If we specify too
many fields, exceeding the 16 bits we've alotted, we get a compilation error. In fact, anytime the metaprogram fails
we simply see that as a compiler error.

We also defined some simple struct constructor functions like `b1` to simplify usage.

We also generate an implementation of the `Print` ability for our type that knows about our encoding. We use a helper
`implPrint` for that, which is nothing more than a string building utility defined in core:
```
fn implPrint(self: Self, typeName: string, body: string, indent: size): unit {
  self.indent(indent);
  self.line("impl Print for {typeName} {{");

  self.indent(indent + 1);
  self.line("fn printTo[W: Writer](self: Self, w: W): unit {{");
  self.writeString(body);
  self.indent(indent + 1);
  self.line("}");

  self.indent(indent);
  self.line("}");
}
```


Let's ask k1 to execute our function at compile-time and insert the code:
```
#meta bitfield/define[u16]("FlagsAuto", [b1("shielded"), b1("cloaked"), b1("damaged"), { name: "sectorId", bits: 5 }, bn("foo", 8)])
```

And that's it! We can now use the type `FlagsAuto`:
```
fn testBitfield(): unit {
  let y: FlagsAuto = { bits: 0b1111_0000_1000_1101 };
  let x: FlagsAuto = FlagsAuto/zero.setShielded(true).setDamaged(true).setSectorId(17).setFoo(0b1111_0000);
  assertEquals(y.getShielded(), x.getShielded());
  assertEquals(y.getCloaked(), x.getCloaked());
  assertEquals(y.getDamaged(), x.getDamaged());
  assertEquals(y.getSectorId(), x.getSectorId());
}
```
