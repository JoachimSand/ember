use std::{iter::Peekable, fmt, error::Error, ops::{Deref, DerefMut}};
use crate::lexer::*;

#[derive(Debug)]
pub enum ParserError {
    NotImplemented,
    EarlyLexerTermination,
    UnknownPrecedence,
    UnexpectedOperator,
    UnexpectedOperand,
}

impl Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        write!(f, "Parser Error: {}", self.to_string())
    }
}

#[derive(Clone, Debug)]
pub enum TerminalType {
    Identifier(String),
    IntegerLiteral(i64),
    DoubleLiteral(f64),
}

type NodePtr = Box<Node>;

#[derive(Clone, Debug)]
pub struct BinaryNode {
    operator : Token,
    left : NodePtr,
    right : NodePtr,
}

#[derive(Clone, Debug)]
pub struct UnaryNode {
    operator : Token,
    operand : NodePtr,
}


#[derive(Clone, Debug)]
pub enum Node { 
    Binary(BinaryNode),
    Unary(UnaryNode),
    Terminal(TerminalType),
    Empty,
}


struct OperatorInfo {
    precedence : u8,
    left_associative : bool,
}

pub fn print_ast(start : NodePtr) {
    print!("Node: {:?}", start.deref());

    match *start {
        Node::Binary(node) => {
            print_ast(node.left);
            print_ast(node.right);
        }
        Node::Unary(node) => {
            print_ast(node.operand);
        }
        _ => return,
    }
}

fn operator_lookup(token : &Token) -> Result<OperatorInfo, ParserError> {
    match token {
        Token::Asterisk | Token::Div | Token::Mod => {
            return Result::Ok(OperatorInfo{precedence : 90, left_associative : true});
        }

        Token::Plus | Token::Minus => {
            return Result::Ok(OperatorInfo{precedence : 80, left_associative : true});
        }
        
        _ => return Result::Err(ParserError::UnknownPrecedence),
    }
}



fn parse_postfix(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    
}



// unary_expression
// 	: postfix_expression
// 	| INC_OP unary_expression
// 	| DEC_OP unary_expression
// 	| unary_operator cast_expression
// 	| SIZEOF unary_expression
// 	| SIZEOF '(' type_name ')'
fn parse_unary(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError>{
   
    let token = lexer.next().ok_or(ParserError::EarlyLexerTermination)?;
    match token {
        Token::IntegerLiteral(n) => {
            let node = Node::Terminal(TerminalType::IntegerLiteral(n));
            return Ok(Box::new(node));
        }

        Token::Increment | Token::Decrement | Token::Ampersand | Token::Asterisk | 
        Token::Plus | Token::Minus | Token::Negation => {
            //TODO: casting
            let operand = parse_unary(lexer)?;

            let node = Node::Unary(
                UnaryNode { 
                    operator : token,
                    operand,
                });
            
            return Ok(Box::new(node));
        }

        Token::Sizeof => {
            let peek = lexer.next().ok_or(ParserError::EarlyLexerTermination)?;
            if peek == Token::LParen {

                //TODO: Parse type name
                return Err(ParserError::NotImplemented); 
            } else {
                
                let node = Node::Unary(
                    UnaryNode { 
                        operator : token,
                        operand,
                    });
                
                return Ok(Box::new(node));
            }

        }

        _ => Err(ParserError::UnexpectedOperand),
    }
}

pub fn parse_expression(lexer : &mut Peekable<Lexer>, min_precedence : u8) -> Result<NodePtr, ParserError>{
    // Find next atom
    let mut result : NodePtr = parse_unary(lexer)?;
    
    loop {
        let peek_token = lexer.peek().ok_or(ParserError::EarlyLexerTermination)?;

        if let Ok(op_info) = operator_lookup(peek_token) {
            
            if op_info.precedence < min_precedence {
                return Ok(result);
            }

            let op_token = lexer.next().ok_or(ParserError::EarlyLexerTermination)?;

            let rhs : NodePtr;
            if op_info.left_associative {
                rhs = parse_expression(lexer, min_precedence + 1)?;
            } else { 
                rhs = parse_expression(lexer, min_precedence)?;
            }

            // Need to use Jones' trick here it seems
            match op_token {
                Token::Plus => result = Box::new(Node::Binary(BinaryNode{operator : Token::Plus, left : result.to_owned(), right : rhs})),
                _ => return Err(ParserError::UnexpectedOperand),
            } 
        } else {
            // Peek token not recognised as operator - this is not an error,
            // as we may simply have run into the end of the expression.
            return Ok(result);
        }
    }
}



