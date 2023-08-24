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
    UnclosedDelimeter(Token<'e>),
    UnknownOperator(Token<'e>),
    UnableToDecomposeDeclaration,
    InvalidConstExpression,
    CannotCombineDeclarationSpecifiers{
        prev_specifier : Token<'e>,
        specifier : Token<'e>, 
    },
    IllegalInitializer {
        identifier_name : &'e str,
    },

    // Type Check Errors
    Redefinition(&'e str),
    InvalidASTStructure,
    NoScopes,
}


fn display_token_error(token : Token, lexer : &mut Lexer, msg : String){
    println!("{}: {msg}", red!("error"));
    println!("On line {}:", token.pos.line_num);
    while token.pos.line_num >= lexer.lines.len() {
        lexer.next_char();
    }

    let err_line = lexer.lines[token.pos.line_num];
    let mut start_col = token.pos.start_col;
    let mut end_col = token.pos.end_col;

    for (p, char) in err_line.chars().enumerate() {
        if char == '\t' {
            if p < start_col {
                start_col += 3;
            }

            if p < end_col {
                end_col += 3;
            }
        }
    }

    let err_line = err_line.replace("\t", "    ");
    println!("{err_line}");
    println!("{0: <1$}{0:^<2$}", "", start_col, end_col - start_col);
}

fn display_compilation_error<'i>(err : CompilationError<'i>, lexer : &mut Lexer) {
    use CompilationError::*;
    let err_str = match err {
        InsufficientSpace       => "Memory Arena ran out of space.".to_string(),
        NotImplemented          => "Error caused by feature not yet implemented.".to_string(),
        UnimplVerbose(msg)      => format!("Error caused by unimplemented feature {msg}"),

        // TODO: This error is generally unhelpful without explaining _what_ the parser expected to come next.
        ExpectedInput => format!("Lexer terminated early."),
        UnknownOperator(t)    => {
            display_token_error(t, lexer , format!("Expected an operator, got {:#?}", t.token_type));
            return;
        }
        UnexpectedToken(t)    => {
            display_token_error(t, lexer, format!("Unexpected token {:#?}.", t.token_type));
            return;
        }

        UnclosedDelimeter(t) => {
            display_token_error(t, lexer, format!("This delimiter is unclosed {:#?}.", t.token_type));
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
    let lexer = &mut Lexer::new(&input, &arena);

    match compile(&arena, lexer) {
        Err(e) => display_compilation_error(e, lexer),
        Ok(()) => (),
    }
}

fn compile<'a>(arena : &'a Arena, lexer : &mut Lexer<'a>) -> Result<(), CompilationError<'a>>{
    let translation_unit = parse_translational_unit(lexer, arena)?;
    print_ast(translation_unit, "".to_string(), true);
    //type_check_start(translation_unit)?;
    codegen_start(translation_unit)?;
    Ok(())
}
