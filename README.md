# k1

Design principles wip
- Expressiveness of a dynamic language, native static performance
- Introspectable types
- Data-first
- Exclude features that would preclude fast compilation
- Fully expression-oriented (in the value context _and_ the type context)
- Expose primitives (`Pointer`, `u8`, etc)
- Aim for friendliness and teachability, avoid jargon (I'd like to kill `enum`)

Notable features wip
- Rich type-level expressions and operations (express a function's return type, or a struct member's type, etc)
- Abilities enable abstractions and polymorphism
- Anonymous structs and enums allow for lightweight, low-boilerplate, zero-cost data modeling
- Pattern matching using both `switch` and `is`
- Limited and predictable type inference: function return types are never inferred, allowing for modular and parallel compilation


## Language tour

### Functions


## About the project

Just exploring the PL dev world by designing and implementing a toy language. The goal is to implement all the basics,
and also explore some interesting ideas. I am trying not to have any aspirations for this project
except to explore the problem space of compiler development, and maybe to be able to do some of advent of code 2023 in this language.

## 
The project isn't mature enough yet to make this list but the spirit of the effort is this: https://justforfunnoreally.dev/

> The programmer, like the poet, works only slightly removed from pure thought-stuff. He builds his castles in the air, from air, creating by exertion of the imagination. Few media of creation are so flexible, so easy to polish and rework, so readily capable of realizing grand conceptual structures.... Yet the program construct, unlike the poet's words, is real in the sense that it moves and works, producing visible outputs separate from the construct itself.
- Brooks
