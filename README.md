# Ember 

Ember is a work-in-progress C90 compiler written in Rust. 

### Features
- Zero dependencies.
- Hand-written lexer and parser.
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

