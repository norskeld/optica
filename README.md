# Optica

[![Tests](https://img.shields.io/github/workflow/status/norskeld/optica/checks?style=flat-square&colorA=22272d&colorB=22272d&label=checks)](https://github.com/norskeld/optica/actions/workflows/test.yml)

Minimal, interpreted, statically typed functional programming language.

## Status & Purpose

> MVP.

I have no real aspirations for the language as this is primarily a personal hobby project for learning Rust and language design.

## Planned features

Core language stuff:

- [x] Haskell/Elm-like syntax.
- [x] Hindley–Milner type inference.
- [x] Type aliases and ADTs.
- [ ] Records (using [this paper by Daan Leijen][records-paper] as a reference).
- [ ] Pattern matching, currently works with:
  - [x] Literals.
  - [x] Tuples and lists.
  - [x] Non-parameterized ADTs.
  - [ ] Parameterized ADTs.
- [x] Module system.
- [ ] Standard library (Prelude).
- [ ] Diagnostic reporting via [codespan] or [ariadne].
- [x] Basic stack-based interpreter.

Other goodies I would like to implement as well, mostly related to pattern matching capabilities:

- [ ] Overload-like pattern matching.
- [ ] Function guards.
- [ ] `where` clauses.
- [ ] As-patterns.
- [ ] Compile patterns to efficient decision trees.

These are not as important the ones above, I will do research first and implement them later on:

- [ ] Effect system.
- [ ] REPL with autocomplete and syntax highlighting.
- [ ] VM with garbage collection.
- [ ] LSP-server.
- [ ] VS Code extension.
- [ ] Infrastructure: package management, tests.
- [ ] Interop with Rust.

## License

[MIT](LICENSE).

<!-- Links. -->

[records-paper]: https://www.microsoft.com/en-us/research/publication/extensible-records-with-scoped-labels/
[codespan]: https://github.com/brendanzab/codespan
[ariadne]: https://github.com/zesterer/ariadne
