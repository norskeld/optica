## Todo

- Re-structure, add abstractions, and split `compiler` crate into several independent crates.
- Avoid so much `clone`ing, both explicit and implicit; use `Rc`/`Arc` for cheap clones, if it's possible or makes sense.
- Prefer iterators over `for` loops where possible.
- Get rid of unnecessary derives.
