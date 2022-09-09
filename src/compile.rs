use std::{iter::Peekable, fmt, error::Error};

use crate::lexer::*;
use crate::parser::*;
use crate::arena::*;
use crate::typechecker::*;

// Global Error type for the whole compiler.
#[derive(Debug)]
pub enum CompilationError<'e> {
    // Memory Arena Errors
    InsufficientSpace,

    // Parser Errors
    NotImplemented,
    UnexpectedToken(Token<'e>),
    EarlyLexerTermination,
    UnknownPrecedence(Token<'e>),
    UnableToDecomposeDeclaration,
    InvalidConstExpression,

    // Type Check Errors
    RedefOfVariable,
    InvalidTreeStructure,
}

impl <'e> Error for CompilationError<'e> {}

impl <'e> fmt::Display for CompilationError<'e> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        write!(f, "Compilation Error Error: {}", self.to_string())
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
    type_check_start(translation_unit)?;
    Ok(())
}
