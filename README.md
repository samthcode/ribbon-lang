NOTE: I'm currently starting again on this, so checkout the branch "starting-anew" for an up-to-date look at what the language might look like and the new implementation.

# The Ribbon Programming Language (WIP)

A language made for fun and designed to be very easy to write. It is made in Rust, designed to be easy to write, and is inspired by languages such as [Nim](https://nim-lang.org/) and other functional languages.

Currently, this language is incomplete.

## Current Stage

At current, work on the parser is just about to begin.

## Code Examples

### Hello World (not final)

```ribbon
"Hello World".print
```

### Factorial (not final)

```ribbon
factorial := (n:u32) => {
    n <= 1 ? return n : n * factorial n-1
}
```

## Aspirations

**This language aims to be:**

- Functional
- Immutable by default
- Declarative
- Strongly and statically typed

This will be a 'toy' language really, not anything made for production code, but I hope it will be very enjoyable to write in and fun to use!
