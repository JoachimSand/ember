use std::{fmt, error::Error};
use crate::lexer::*;
use crate::parser::*;
use crate::pretty_print::*;
use crate::arena::*;
//use crate::typechecker::*;
use crate::codegen::*;

// Global Error type for the whole compiler.
#[derive(Debug)]
pub enum CompilationError<'e> {
    // Memory Arena Errors
    InsufficientSpace,

    // Parser Errors
    UnimplVerbose(String),
    NotImplemented,
    UnexpectedToken(Token<'e>),
    EarlyLexerTermination,
    UnknownPrecedence(Token<'e>),
    UnableToDecomposeDeclaration,
    InvalidConstExpression,

    // Type Check Errors
    Redefinition(&'e str),
    InvalidASTStructure,
    NoScopes,
}

impl <'e> Error for CompilationError<'e> {}

impl <'e> fmt::Display for CompilationError<'e> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        use CompilationError::*;
        let err_str = match self {
            InsufficientSpace       => "Memory Arena ran out of space.".to_string(),
            NotImplemented          => "Error caused by feature not yet implemented.".to_string(),
            UnimplVerbose(msg)      => format!("Error caused by unimplemented feature {msg}"),
            EarlyLexerTermination   => format!("Lexer terminated early."),
            UnknownPrecedence(t)    => format!("Token {t:?} does not have any precedence associated with it"),
            _ => return write!(f, "Compilation Error: {:?}", self)  
        };

        write!(f, "Compilation Error: {err_str}")
    }
}

// Don't compile - only parse the input
// and produce a legible AST.
pub fn parse_only(input : &str){
    println!("{input}");
    let arena = &mut Arena::new(20000);

    let lexer = &mut Lexer::new(&input, &arena).peekable();
    match parse_translational_unit(lexer, arena) {
        Ok(node) => {
            //println!("{:?}", node);
            print_ast(node, "".to_string(), true);
        }
        Err(e) => println!("{e}"),
    }
}

pub fn compile_start(input : &str){
    let arena = &mut Arena::new(20000);
    match compile(input, &arena) {
        Err(e) => println!("{e}"),
        Ok(()) => (),
    }
}

fn compile<'i : 'a, 'a>(input : &'i str, arena : &'a Arena) -> Result<(), CompilationError<'a>>{
    let lexer = &mut Lexer::new(&input, &arena).peekable();

    let translation_unit = parse_translational_unit(lexer, arena)?;
    print_ast(translation_unit, "".to_string(), true);
    //type_check_start(translation_unit)?;
    codegen_start(translation_unit)?;
    Ok(())
}
