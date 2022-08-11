use std::io;
use std::env;
use std::fs;

mod lexer;
mod parser;

use lexer::*;

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
                Ok(_n) => {
                    let lexer = Lexer::new(&input);

                    for cur_token in Lexer::new(&input) { 
                        println!("{:?} ", cur_token);
                    }

                    match parser::parse_expression(&mut lexer.peekable(), 0) {
                        Ok(node) => {
                            println!("Parsed expr.");
                            parser::print_ast(node, String::new(), true);
                        }
                        Err(e) => println!("Error {e:?}"),
                    }
                },
                Err(e) => println!("Error: {e}"),
            };
        }     
    } else if args.len() == 2 {
        let filename = &args[0];

        match fs::read_to_string(filename) {
            Ok(contents) => {

                //let lexer = lexer::Lexer{char_stream : contents.chars().peekable()};

                let lexer = Lexer::new(&contents);

                parser::parse_expression(&mut lexer.peekable(), 0);
                /*
                for token in lexer {
                    println!("{:?}", token);
                }
                */
            }

            Err(e) => {
                println!("Could not open specified file: {e}");
            }
        }
    }
}

