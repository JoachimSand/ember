# Ember 

Ember is a work-in-progress C90 compiler written in Rust. 

### Features
- Zero dependencies.

- Hand-written lexer and parser.  
Ember uses a hand-written recursive descent parser for parsing most of the C90 grammar. However, for efficiency expressions are parsed with precedence climbing instead.

- Runtime allocations with a custom-built memory arena module.  
In order to minimize allocation overhead, Ember uses a small memory arena for allocating the generated AST. By tying the lifetime of every AST node to the arena, this also avoids the usual pitfalls of dealing with trees in Rust.

- Extensive visualisation of generated ASTs with a canonical representation for all types (even the really gnarly looking function pointers!).  

Take the following example:
```C
int main(){
    int (*(*fp_arr[3][9])[15])();
    int a = 1;
    int b = 4 + 2/a++;
    if(b)
        a = 5;
     else 
        a = 8;
}
```
Ember can visualize this as:


    ![print_example](https://user-images.githubusercontent.com/37040245/192108603-6ff136b4-09e3-4125-89e5-786898459033.png)

### Building

Since Ember is written in Rust, you will need an installation of Rust to compile it. Ember is known to compile with Rust 1.59 (Stable) or newer. Compatibility with older versions of Rust will be check in the near future.

To build:

```
$ git clone https://github.com/JoachimSand/Ember
$ cd ember
$ cargo build --release
$ ./target/release/ember -h
```

### Usage
```
 Usage: ember [ -p ][ -c ] <file>
         -p       Parse specified file and print an AST
         -c       Compile specified file. WIP
         -h       Print this help menu
         <file> can optionally be left empty, in which case input can be input directly with a REPL.
```

### Tests
Work-in-progress.
