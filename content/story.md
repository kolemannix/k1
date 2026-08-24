`k1` is what resulted from me wanting to write similar code to what I write in Scala but have it compile and run 1000x faster.

But as the design developed, I came to believe that a language's job is not to shield the programmer from the
realities of computing, but to provide tools for dealing with them. Thus, it became quite low-level. The baked-in
growable List was removed in favor of more powerful primitives.

So now we have this design that attempts to provide as much expressive power, as well as control over the final
executable, as possible without making any compromises on performance ceiling. Memory management is
