use std::io;
use std::env;
use std::fs;

mod lexer;
mod parser;
mod typechecker;
mod arena;

use lexer::*;

use crate::typechecker::type_check_start;

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

                    //for cur_token in Lexer::new(&input) { 
                    //    println!("{:?} ", cur_token);
                    //}
                    let parser_result = parser::parse_translational_unit(&mut lexer.peekable());
                    match parser_result {
                        Ok(node) => {
                            println!("Parsed translation_unit.");
                            println!("{:?}", node);
                            parser::print_ast(node.clone(), String::new(), true);

                            match typechecker::type_check_start(&mut typechecker::Scopes::new(), node) {
                                Ok(_) => (),
                                Err(e) => println!("Error {e:?}"),
                            }
                        }
                        Err(e) => println!("Error {e:?}"),
                    }
                    
                },
                Err(e) => println!("Error: {e}"),
            };
        }     
    } else if args.len() == 2 {
        let filename = &args[1];

        match fs::read_to_string(filename) {
            Ok(contents) => {

                for cur_token in Lexer::new(&contents) { 
                    println!("{:?} ", cur_token);
                }
                
                let lexer = Lexer::new(&contents);

                match parser::parse_translational_unit(&mut lexer.peekable()) {
                    Ok(node) => {
                        println!("Parsed translation_unit from file.");
                        println!("{:?}\n", node);
                        parser::print_ast(node, String::new(), true);
                    }
                    Err(e) => println!("Error {e:?}"),
                }
            }

            Err(e) => {
                println!("Could not open specified file: {e}");
            }
        }
    }
}

