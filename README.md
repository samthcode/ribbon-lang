NOTE: I'm currently starting again on this, so checkout the branch "starting-anew" for an up-to-date look at what the language might look like and the new implementation.

# The Ribbon Programming Language (WIP)

The Ribbon Programming Language is designed to be pleasing to develop in, including modern features and sugar which elevate the developer experience.

Ribbon is just in its infancy at this stage and is not even fully concepted, let alone anywhere near completion.

## Unfinalised Code Examples

```rust
const add := (i32, i32) -> i32 {
    (10, b) => 100 + b,
    (a, 20) => a.pow(2) + 20,
    (a, b) => a + b
};
const main := () -> {
    a := 10;
    b := 20;
    add(a, b).assert_eq(120)
};
```