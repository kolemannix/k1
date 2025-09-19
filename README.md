# k1 Programming Language

`k1` is like C with typeclasses, ADTs\*, capturing lambdas, pattern matching, and a powerful generic typesystem.

`k1` is what resulted from me wanting to write similar code to what I write in Scala but have it compile and run 1000x faster.


Core ideas
- The compiler should be uncompromisingly fast
- The generated code should be as optimal as possible
- Compile-time reflection should enable powerful metaprogramming

Check out the [TODO](TODO.md) for a glimpse into the development flow, or the [test_src/](test_src/) dir to see what the language can currently do!

\*structs for product types and first-class tagged unions for sum types via the `either` keyword

# About the project

Just exploring the PL dev world by designing and implementing a toy language. The goal is to implement all the basics,
and also explore some interesting ideas. I am trying not to have any aspirations for this project
except to explore the problem space of compiler development, and maybe to be able to do some of advent of code 2023 in this language.

Inspiration

https://justforfunnoreally.dev/

> The programmer, like the poet, works only slightly removed from pure thought-stuff. He builds his castles in the air, from air, creating by exertion of the imagination. Few media of creation are so flexible, so easy to polish and rework, so readily capable of realizing grand conceptual structures.... Yet the program construct, unlike the poet's words, is real in the sense that it moves and works, producing visible outputs separate from the construct itself.
- Brooks

> The way to fall asleep is by pretending to be someone who is asleep. And that's how everything works
- Someone on twitter

> You can just do things
- Popular memes archive, 2024

## Example

Here's an example showing several language features:

```rust
type Color = either Red, Green, Blue
impl Show for Color {
  fn show(self: Color): string {
    switch self {
      .Red -> "red",
      .Green -> "green",
      .Blue -> "blue"
    }
  }
}

fn main(): i32 {

  // File IO
  let input: string = Files::readToString("k1/dogfood/aoc/2023/2.txt");

  println("Hello, day 2!");

  // Anonymous struct, strongly typed and statically laid-out
  let supply = {
    red: 12,
    green: 13,
    blue: 14
  };
  let mut goodGamesSum: u64 = 0;

  // TODO: use `List.filter`
  for line in input.splitByChar('\n') do {
    let id = it_index + 1;
    let line = line.splitByChar(':').get(1);
    let mut isGoodGame = true;
    for round in line.splitByChar(';') do {

      // Parse the puzzle input
      let values = round.splitByChar(',');
      let values: List[{ amount: int, color: Color }] = for value in values yield {
        let amountColor = value.splitByChar(' ');
        let amount = amountColor.get(0).toInt() ? crash("Bad amount: \{value}");
        let color: Color = switch amountColor.get(1) {
          "red" -> .Red,
          "green" -> .Green,
          "blue" -> .Blue,
          c -> crash("Unexpected color: \{c}")
        };
        { amount: amount, color: color }
      };

      // Solve the puzzle; detect 'bad games'
      for value in values do {
        let colorSupply = switch value.color {
          .Red -> supply.red,
          .Green -> supply.green,
          .Blue -> supply.blue
        };
        if value.amount > colorSupply {
          isGoodGame = false;
          let amt = value.amount;
          let color = value.color;
          println("Bad Game: amount=\{amt} color=\{color}")
        };
      };
    };
    if isGoodGame { goodGamesSum = goodGamesSum + id };
    // println(line);
  };
  println("{goodGamesSum}");
  0
}
```


### Type System
- **Strong Static Typing** with limited, fast, and predictable type inference
- **Algebraic Data Types** via `either` for tagged unions and structs for product types
- **Generics** with support for type constraints through abilities (traits)
- **Deeply expression-oriented** 
  - `loop` with `break(<expr>)`, 
  - `if` expressions that support a single pattern
  - A `never` type that allows simplifying types while handling cases
- **First-class Optional Types** with ergonomic `?` and `?.` operators and pattern matching
- **Reference Types** with distinct pointer and reference semantics
- **Anonymous structs and enums** allow for lightweight, low-boilerplate, zero-cost data modeling
- **Zero-cost Abstraction** through opaque type aliases, and zero-overhead structs

### Modern Features
- **Pattern Matching** with exhaustiveness checking and useless pattern detection
- **Closures** with automatic capture analysis and environment generation
- **Type-safe String Interpolation** using `\{...}` syntax
- **Iterator Protocol** with `for` expressions supporting `yield` and `do` blocks
- **Method Syntax** with namespaced scoping
- **Pipeline Operator** (`|`) for functional composition
- **Abilities** (traits/interfaces) for type-class like abstraction

### Systems Programming facilities
- **Direct Memory Management** with `Pointer` operations and introspectable type layouts
- **Foreign Function Interface** through external function declarations
- **Bit Manipulation** operations
- **Fixed-Size Integer Types** (u8/i8 through u64/i64)
- **Platform Integration** through libc bindings

### Limited WIP Standard Library
- **Generic Collections** including Array and HashMap implementations
- **String Utilities** with builders, interpolation, and efficient operations
- **Option Type** with ergonomic methods and pattern matching
- **Numeric Tower** supporting integers, floats with standard operations
- **Runtime Error Handling** with basic crash reporting

### LLVM Backend
- **Efficient Machine Code** through LLVM's optimization pipeline
- **Zero-Cost Abstractions** with aggressive inlining and dead code elimination
- **Flat Memory Layout** for structs and tagged unions
- **Debug Information** generation using DWARF format
