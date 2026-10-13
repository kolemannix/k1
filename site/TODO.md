# Site to-do

- dbx case study: write it up under examples (how dbx pulls SQLite into the k1
  fold at compile time: the dependency, the bindings, the schema, tables as k1
  types), then link it from the early execution section on the homepage, which
  alludes to it today without a link.
- One-line install: `curl -fsSL <url> | sh` on the install page. Needs its own
  session to design: where the script and the bundles are fetched from, what
  the script does on macOS and Linux (bundle download, `~/.k1`, PATH), how it is
  versioned and verified, and how `builds/bundle.sh` and `install.sh` feed it.
  Once it works, put the one-liner on the install page.
- Manifesto tenet 01 is a placeholder: write the opening tenet about the human
  wonder of computing and programming as an art, more specific than "the joy of
  programming" (overused; Odin has it). Candidates so far: programming is
  wonderful, programming is art, protecting the art of programming.
- OKLCH palette as a second live wasm demo: render the swatches the compile-time
  code produces, on the page, the way refchess runs perft.
