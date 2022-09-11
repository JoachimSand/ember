use std::io;
use std::env;
use std::fs;
use std::error::*;

mod lexer;
mod parser;
mod typechecker;
mod arena;
mod compile;
mod codegen;

use compile::*;

fn main() {
    let args : Vec<String> = env::args().collect();
    println!("Recieved args: {:?}", args);
    
    if args.len() == 1 {
        // No arguments recieved, try parsing command line input.
        let stdin = io::stdin();
        let input = &mut String::new();

        loop {
            input.clear();
            
            match stdin.read_line(input) {
                Ok(_) => {
                    compile_start(input);
                },
                Err(e) => {
                    println!("Error: {e}");
                    return;
                }
            };
        }     
    } else if args.len() == 2 {
        let filename = &args[1];

        match fs::read_to_string(filename) {
            Ok(contents) => {
                compile_start(&contents);
            }

            Err(e) => {
                println!("Could not open specified file: {e}");
            }
        }
    }
}



