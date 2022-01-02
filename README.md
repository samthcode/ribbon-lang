# The Ribbon Programming Language (WIP)

This language is designed to be quick to write and is heavily inspired by [Rust](https://www.rust-lang.org/), which is also the language it was created in.

Currently, this language is very incomplete.

## Current Stage

At current, the lexer has not yet been completed, but the groundworks are there.

## Code Examples

### Hello World (not final)

```ribbon
fn main {
    "Hello World".print
}
```

### Factorial (not final)

```ribbon
fn n:usize.factorial {
    if n < 2 {
        return n
    } else {
        return n * (n-1).factorial
    }
}
```