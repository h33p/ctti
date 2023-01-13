# Compile-Time Type Information

This crate is an experimental standard library side implementation of potential ctti language feature.

The idea is to provide a function `const fn type_info<T: ?Sized>() -> &'static Type` which
encodes the underlying type. Other crates can then build on top of this to provide advanced
features, such as:

1. Stable `TypeId`.
2. Safe dynamic linking by comparing 2 `Type`s across libraries.
3. Reflection.

None of the above need to be implemented by the standard library. Everything can be built on
top of compile-time type information.

**Why not just this crate?**

To be useful, the type information should be available to all types. Implementing it on
primitives is simple, however, user types (structs, enums, unions) are impossible without
either:

1. Wrapping every type with a macro.
2. Compiler support.

1 is the current state of many crates, such as `abi_stable`, `safer_ffi`. See
  [`impls`](crate::impls) module with example of how we do it. It works, however, it is
  incredibly invasive - production code requires heavy modification to the crate, and
  downstream crates cannot be used by them for the purposes of type layout checks.

2 becomes the only feasible solution. Depending on how advanced it is, it can also nicely
  handle things like self-referential structures without requiring runtime processing.

License: MIT
