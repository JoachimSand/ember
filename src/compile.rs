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
    ExpectedToken(TokenType<'e>),
    ExpectedInput/*(&'e str)*/,
    UnknownOperator(Token<'e>),
    UnableToDecomposeDeclaration,
    InvalidConstExpression,

    // Type Check Errors
    Redefinition(&'e str),
    InvalidASTStructure,
    NoScopes,
}


fn display_token_error(token : Token, lexer : &mut Lexer, msg : String){
    println!("ERROR: {msg}");
    println!("On line {}:", token.pos.line_num);
    while token.pos.line_num >= lexer.lines.len() {
        lexer.next_char();
    }

    let err_line = lexer.lines[token.pos.line_num];
    println!("{err_line}")
}

fn display_compilation_error<'i>(err : CompilationError<'i>, lexer : &mut Lexer) {
    use CompilationError::*;
    let err_str = match err {
        InsufficientSpace       => "Memory Arena ran out of space.".to_string(),
        NotImplemented          => "Error caused by feature not yet implemented.".to_string(),
        UnimplVerbose(msg)      => format!("Error caused by unimplemented feature {msg}"),

        // TODO: This error is generally unhelpful without explain _what_ the parser expected to come next.
        ExpectedInput => format!("Lexer terminated early."),
        UnknownOperator(t)    => {
            display_token_error(t, lexer , format!("Expected an operator, got {:#?}", t.token_type));
            return;
        }
        UnexpectedToken(t)    => {
            display_token_error(t, lexer, format!("Unexpected token {:#?}.", t.token_type));
            return;
        }
        _ => format!("Compilation Error: {err:?}"), 

    };

    println!("Compilation Error: {err_str}")
}

// Don't compile - only parse the input
// and produce a legible AST.
pub fn parse_only(input : &str){
    println!("{input}");
    let arena = &mut Arena::new(20000);

    let lexer = &mut Lexer::new(&input, &arena);
    match parse_translational_unit(lexer, arena) {
        Ok(node) => {
            //println!("{:?}", node);
            print_ast(node, "".to_string(), true);
        }
        Err(e) => display_compilation_error(e, lexer),
    }
}

pub fn compile_start(input : &str){
    let arena = &mut Arena::new(20000);
    match compile(input, &arena) {
        Err(e) => println!("{e:?}"),
        Ok(()) => (),
    }
}

fn compile<'i : 'a, 'a>(input : &'i str, arena : &'a Arena) -> Result<(), CompilationError<'a>>{
    let lexer = &mut Lexer::new(&input, &arena);

    let translation_unit = parse_translational_unit(lexer, arena)?;
    print_ast(translation_unit, "".to_string(), true);
    //type_check_start(translation_unit)?;
    codegen_start(translation_unit)?;
    Ok(())
}
