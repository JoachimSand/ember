# Ember 

Ember is a work-in-progress C90 compiler written in Rust. 

### Features
- Zero dependencies.

- Hand-written lexer and parser.
Ember uses a hand-written recursive descent parser for parsing most of the C90 grammar. However, for efficiency expressions are parsed with precedence climbing instead.

- Runtime allocations with a custom-built memory arena module.
In order to minimize allocation overhead, Ember uses a small memory arena for allocating the generated AST. By tying the lifetime of every AST node to the arena, this also avoids the usual pitfalls of dealing with trees in Rust.

- Extensive visualisation of generated ASTs with a canonical representation for all types (even the really gnarly looking function pointers!).

### Building

Since Ember is written in Rust, you will need an installation of Rust to compile it. Ember is known to compile with Rust 1.59 (Stable) or newer. Compatibility with older versions of Rust will be check in the near future.

To build:

```
$ git clone https://github.com/JoachimSand/Ember
$ cd ember
$ cargo build --release
$ ./target/release/ember -h
```

### Tests

