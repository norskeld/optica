## Todo

- [Avoid so much `clone`ing](#fck-clones).
- Prefer iterators over `for` loops where possible.
- Get rid of `unwrap`s in favor of proper error handling. Document cases, where it's **safe** to `unwrap`.
- Use `Range` for spans.
- Make use of string interning.
- Add more tests and set up coverage reports.

## F*ck clones

- **Borrowing**. Sometimes, a reference is all we need.
- **Moving**. Cheaper than clone for some types, and will also clarify the semantics of the code.
- **Boxing**. Makes moves cheap for all types, at the cost of some dynamic memory allocation and pointer indirection.
- **Refcounting** (`Rc` and `Arc`). Useful when we want an immutable borrow, but design does not allow us to do it at compile time. Cyclic references may lead to headaches.
- **Copy-on-write** (`Cow`). Can help in situations where simple immutable borrows do not apply, because we want to mutate the data from time to time. `std::borrow::Cow` is only useful for single-threaded code.
- **Read-copy-update** (**RCU**). Basically copy-on-write, but for multiple threads.
