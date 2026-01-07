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

## Tenets
### Programming should be joyful
I believe programming is more like writing than manufacturing, and I believe the literature, research, and countless body of testimonials
support this claim. As such, we can be relatively confident that we'll do our best work when we are willing, inspired, and 'feeling it'.
### Compile fast
Shortening the feedback cycle preserves flow and greatly increases joy, as well as literally temporally enabling more iterations.
This results in better software that is fun to work on. There are a number of language design decisions in `k1` that were
explicitly made to enable and preserve very fast compilation, like requiring function signatures to fully specify their types
### Your programs should be as fast as your computer is
A career spent programming for the JVM has made me loathe inescapable costs. There are many fantastic features
and idioms that can be free, such as newtypes, lambdas, even something as simple as the almighty `struct`, which
many languages lack in favor of a boxed class as the primary unit of computation. This is a huge mistake; it forces
the programmer to choose between something typesafe and idiomatic with overhead, or something fast. You end up [doing
metaprogramming heroics](https://getkyo.io/#/?id=maybe-allocation-free-optional-values) to implement a zero-cost option in Scala,
or [managing your own memory in Java](https://github.com/netty/netty/blob/4.2/buffer/src/main/java/io/netty/buffer/PooledByteBufAllocator.java).
### Metaprogramming should be like programming
Compile-time execution and reflection enables powerful metaprogramming that is just regular programming.

As programmers, we've become exceedingly good at expressing and solving our problems using programming. Why can't
we also solve our programming, and build, problems this way?
### Typeclasses + structs and enums. No inheritance
[Just so good](https://dl.acm.org/doi/10.1145/75277.75283)
### No forced abstractions or costs
Pay for what you use, and use what you want. And what you use costs as little as possible. This is
exactly Rust's philosophy and definition of 'zero-cost' functionality. I think it holds up and is a fantastic
north star.
### Tools should empower not restrict
While I don't take safety and stability lightly, I'm more concerned with making awesome things possible than making bad things impossible.

As a case study I like to pick on Rust a bit. Fantastic achievement that it is, Rust undeniably feels bad to write.

By a stark contrast, C is widely regarded as super fun to write. You feel empowered, and in control.

I'm most interested in adding safety, guardrails, and guarantees in a way that does **not** strip away this core feeling of being in control.
Even C has some frustrating gotchas surrounding the "C virtual machine", TBAA, and certain "as if" semantics where we don't truly know the
code will be exactly as we wrote it without writing assembly. However, I dub these 'gotchas' rather than limitations for a reason; they
can certainly be navigated around. But the limitations Rust imposes on our programs cannot. You can't really escape the "I know better than
you" vibe that Rust gives off, and it is offputting in a way that standard typechecking or other checks and controls really aren't.

## Status
Long todo list still!
[Check it out](TODO.md) for a glimpse into the development flow, or the [test_src/](test_src/) dir to see what the language can currently do!

Still exploring and heavily experimenting, but I really like using this language these days. 

**Disclaimer**: `k1` is not ready for use; it needs to evolve much more and will require a lot of elbow grease if things ever
get to that point. I'd like to push the language further, as well as correct a number of design mistakes, before allowing it to begin to crystallize

Please reach out to me on X @kolemannix or here if you're interested in the project!

## Reflections
I simply started because I was curious what LLVM was, and had a lot of ideas about how programming could be better (20,000 hours will do that to you), and
I specifically wanted to understand performance from the inside. Why were my tools and programs so slow?
Mainly I wanted to see if a compiler could be fast and also do nice things for you. There are some core decisions (functions must declare
their return types) designed to protect the performance of the compiler. (I think they are good decisions for software engineering anyway)
I had no expectation that I could do better, having never designed a language or made a compiler, but I just wanted to explore. So I just started studying and
practicing using this project.

More later I'm sure... everything evolves...

Inspiration

> The programmer, like the poet, works only slightly removed from pure thought-stuff. He builds his castles in the air, from air, creating by exertion of the imagination. Few media of creation are so flexible, so easy to polish and rework, so readily capable of realizing grand conceptual structures.... Yet the program construct, unlike the poet's words, is real in the sense that it moves and works, producing visible outputs separate from the construct itself.
- Brooks

> The way to fall asleep is by pretending to be someone who is asleep. And that's how everything works
- Someone on twitter

> You can just do things
- Popular memes archive, 2024

https://justforfunnoreally.dev/

Some Heroes: Casey Muratori, Linus Torvalds, Andreas Kling, Jonathan Blow, Ken Thompson, Chris Lattner, Bjarne Stroustrup, [Rob Pike](https://docs.google.com/presentation/d/e/2PACX-1vSmIbSwh1_DXKEMU5YKgYpt5_b4yfOfpfEOKS5_cvtLdiHsX6zt-gNeisamRuCtDtCb2SbTafTI8V47/pub?start=false&loop=false&delayms=3000#slide=id.p)

## Case Study: implementing bitfields

Let's add a feature to `k1` using metaprogramming to support bitfields.
[Video Version](https://youtu.be/bSxzW6lGWlc)

I'd like to be able to define a bitfield by providing a name, a 'base' integer type
big enough to house all the fields I provide, and a series of fields as a collection of (name, bit width) pairs.

We can encode this in K1 easily enough as a function:

```rust
fn bitfield[Base](typeName: string, members: View[{ name: string, bits: size}]): ???
```
We use a type parameter, `Base`, denoted in square brackets, to track our 'base type', which
should be an unsigned integer type, which we can enforce later. `k1` provides the usual friends: u8, u16, u32, and u64.

We also accept a name to attach to whatever constructs we end up generating for our flags.
Perhaps functions to encode and decode each bit-field, and maybe some constants? Not sure yet.

Most importantly we accept a View of structs, with `name` and `bits`, telling us which 'bit' 'fields' we'd
like to encode or pack into the value.

That leaves the return type, which we've left as `???`. Ultimately we'd like to produce
some code; most text-based programming languages are very well-represented and maintained in
a particularly powerful format: source code text. So we'll return a `string`,
(not a `org.sys.meta.macros.whitebox.TTreeSyn.Quoted`) containing the K1 code we'd otherwise have hand-written for our bitfield.

```rust
fn intWidthForBits(bits: size): size {
  if bits <= 8 8
  else if bits <= 16 16
  else if bits <= 32 32
  else if bits <= 64 64
  // We'll just exit on an error here rather than returning a `Result[T, E]`, since we intend to run this code at compile-time.
  else crash("Too many bits: {bits}")
}
// Some simple constructors to make our metaprogram nice to invoke later
fn bn(name: string, bits: size): { name: string, bits: size } { { name, bits } }
fn b1(name: string): { name: string, bits: size } { bn(name, 1) }
fn b8(name: string): { name: string, bits: size } { bn(name, 8) }
```

Here comes a big wall of advanced metaprogramming code; but it may look strikingly like dumb string-building code to you.
```rust
fn define[Base](typeName: string, members: View[{ name: string, bits: size }]): string {
  use core/StringBuilder; use meta/CodeWriter;

  let baseTypeId: u64 = types/typeId[Base]();
  let baseSchema = types/typeSchema(baseTypeId);
  let baseName = types/typeName(baseTypeId);
  require baseSchema is .Int(intKind) else { crash("Base should be an int; got {baseName}") };
  let totalBits = intKind.bitWidth();

  // This is just a regular StringBuilder that gets some extra methods since we've
  // brought the meta/CodeWriter ability into scope
  let* code = StringBuilder/new(); // The asterisk means we want a reference (a stack address)

  // This defines a named struct type with one field, which is a common newtype pattern
  code.line("deftype {typeName} = {{ bits: {baseName} }");

  // We crack open a namespace to put some goodies inside
  code.line("ns {typeName} {{");

  // This actually gets captured by the lambda below
  let* bitIndex = 0;

  // Let's analyze each member we were passed, and perform the actual bitfield layouting work once.
  // We'll make a list of an unnamed (anonymous) little struct type for our info we'll need later
  let memberInfo: List[{ 
    // mask: the positioned mask
    mask: usize,
    // typeWidth: the smallest type that can hold these bits; see `intWidthForBits`
    typeWidth: size,
    // invMask: the negation of the mask, but also masked down to typeWidth size (to generate constants that fit)
    invMask: usize,
    // rawValue: the mask shifted to the start, the 'magnitude' of the used bits
    rawValue: usize,
    // offset: the bit pos that the mask starts at; where the value lives; how much to shift
    offset: size,
    // nameCap: the name of the field, capitalized
    nameCap: string
  }] = List/fromMap(members, \member. {
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

    { mask, typeWidth, invMask, rawValue, offset, nameCap }
  });

  // The for loop works on any type that implements Iterable or Iterator
  // The item being iterated over is named `it` if no binding is supplied
  // `itIndex` is also given to us
  for members {
    let info = memberInfo.get(itIndex);
    // Emit globals, or constants, for each field's mask
    code.line("  let {it.name}Mask: {baseName} = {info.mask};");
  };

  code.line("  let zero: {typeName} = {{ bits: 0 };");

  // Now let's do a getter and setter for each bitfield. If the width is 1,
  // we'll use bool since that's what the people likely want. Note that we could configure
  // any of this behavior because this is just a regular function
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
For an example input, let's encode some data about a starship into 16 bits. 
- bit 1: shielded
- bit 2: cloaked
- bit 3: damaged
- bits 4-8: a sector id
- bits 9-16: a byte named `foo` because what example is complete without a `foo`

We could test this function by just running it and inspecting the string, since
it is a normal function, or we could ask `k1` to run it for us and insert the result into our program:
```rust
#meta std/bitfield/define[u16]("StarshipFlags",
  [b1("shielded"), b1("cloaked"), b1("damaged"), { name: "sectorId", bits: 5 }, bn("foo", 8)]
)
```

And now we can use the type and namespace `StarshipFlags` to pack some bits!
```rust
fn testBitfield(): unit {
  let y: StarshipFlags = { bits: 0b1111_0000_1000_1101 };
  //                           foo......|secto||||
  //                                           |||- shielded
  //                                           ||- cloaked
  //                                           |- damaged
  let x: StarshipFlags = StarshipFlags/zero.setShielded(true).setDamaged(true).setSectorId(17).setFoo(0b1111_0000);
  assertEquals(y.getShielded(), x.getShielded());
  assertEquals(y.getCloaked(), x.getCloaked());
  assertEquals(y.getDamaged(), x.getDamaged());
  assertEquals(y.getSectorId(), x.getSectorId());
}
```
