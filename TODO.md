## Todo

- Avoid so much `clone`ing, both explicit and implicit; use `Rc`/`Arc` for cheap clones, if it's possible or makes sense.
- Prefer iterators over `for` loops where possible.
- Get rid of unnecessary derives.
- Get rid of `unwrap`s.
- Make `Bool` internal type and value?
- Add more tests.
- Set up CI.
- Probably use `Range` for spans.
