use std::io;
use std::env;
use std::fs;
use std::error::*;
use std::fmt;

mod lexer;
mod parser;
mod pretty_print;
mod typechecker;
mod arena;
mod compile;
mod codegen;

use compile::*;

// Global Error type for the whole compiler.
#[derive(Debug)]
pub enum MainError {
    InvalidArgCount(usize),
    UnableToOpenFile(io::Error),
    UnrecognisedArg(String),
}

impl Error for MainError {}

impl fmt::Display for MainError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        match self {
            MainError::InvalidArgCount(n) => {
                write!(f, "Invalid number of arguments provided: {}", n)?; 
            } 

            MainError::UnableToOpenFile(io_err) => {
                write!(f, "{io_err}")?; 
            }

            MainError::UnrecognisedArg(arg) => {
                write!(f, "Unrecognised argument {arg}")?;
            }

        }

        writeln!(f, "See --help for documentation")
    }
}


fn main() -> Result<(), MainError>{
    let args : Vec<String> = env::args().collect();
    //println!("Recieved args: {:?}", args);
    
    if args.len() == 1 {
        // No arguments provided, default to repl compiling
        repl_input(compile_start);
    } else if args.len() > 3 {
        return Err(MainError::InvalidArgCount(args.len() - 1));
    }

    let mut n = 1;
    while n < args.len() {
        let arg = &args[n];
        match arg.as_str() {
            "-parse" | "-p" => {
                if let Some(file_name) = args.get(n + 1){
                    process_file(file_name, parse_only)?;
                    n += 1;
                } else {
                    repl_input(parse_only);
                    return Ok(())
                }
            }

            "-compile" | "-c" => {
                if let Some(file_name) = args.get(n + 1){
                    process_file(file_name, compile_start)?;
                    n += 1;
                } else {
                    repl_input(compile_start);
                    return Ok(())
                }
            }

            "-help" | "-h" => {
                println!("\n \
                Usage: ember [ -p ][ -c ] <file>\n \
                    \t -p       Parse specified file and print an AST\n \
                    \t -c       Compile specified file. WIP\n \
                    \t -h       Print this help menu\n \
                    \t <file> can optionally be left empty, in which case input can be input directly with a REPL. \n \
                    ")

            }
            _ => return Err(MainError::UnrecognisedArg(arg.clone())),
        }

        n += 1;
    }
    return Ok(())
}

fn process_file(file_name : &str, func : fn(&str)) -> Result<(), MainError>{
    let contents = fs::read_to_string(file_name).map_err(|e| MainError::UnableToOpenFile(e))?;
    func(&contents);
    Ok(())
}

fn repl_input(func : fn(&str)) {
    let stdin = io::stdin();
    let input = &mut String::new();

    loop {
        input.clear();
        
        match stdin.read_line(input) {
            Ok(_) => {
                func(input);
            },
            Err(e) => {
                println!("Error: {e}");
                return;
            }
        };
    }     
}
