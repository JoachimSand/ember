use std::{iter::Peekable, fmt, error::Error, ops::{Deref, DerefMut}};
use crate::lexer::*;

#[derive(Debug)]
pub enum ParserError {
    NotImplemented,
    UnexpectedToken(Token),
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
pub struct ArrayIndexNode {
    lvalue : NodePtr,
    index : NodePtr,
}

#[derive(Clone, Debug)]
pub enum Node { 
    Binary(BinaryNode),
    Prefix(UnaryNode),
    Postfix(UnaryNode),

    ArrayIndex(ArrayIndexNode),
    Terminal(Token),
    Empty,
}


struct OperatorInfo {
    precedence : u8,
    left_associative : bool,
}

pub fn print_ast(start : NodePtr) {
    println!("Node: {:?}", start.deref());

    match *start {
        Node::Binary(node) => {
            print_ast(node.left);
            print_ast(node.right);
        }
        Node::Prefix(node) => {
            print_ast(node.operand);
        }
        Node::Postfix(node) => {
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


fn expect_token(lexer : &mut Peekable<Lexer>, expect : Token) -> Result<(), ParserError> {
    let cur_token = lexer.next().ok_or(ParserError::EarlyLexerTermination)?;

    if cur_token != expect {
        return Ok(());
    } else {
        return Err(ParserError::UnexpectedToken(cur_token));
    }
}

fn parse_type(lexer : &mut Peekable<Lexer>) -> Result<Token, ParserError> {
    let cur_token = lexer.next().ok_or(ParserError::EarlyLexerTermination)?;
    match cur_token {
        Token::Float | Token::Int | Token::Double => {
            return Ok(cur_token);
        }
        _ => {
            // Handle user defined types
            return Err(ParserError::NotImplemented);
        }
    }
}

// primary_expression
// 	: IDENTIFIER
// 	| CONSTANT
// 	| STRING_LITERAL
// 	| '(' expression ')'
// 	;
fn parse_primary(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError>{
    let cur_token = lexer.next().ok_or(ParserError::EarlyLexerTermination)?;
    match cur_token {
        Token::Identifier(_) | Token::IntegerLiteral(_) => {
            return Ok(Box::new(Node::Terminal(cur_token)));
        }

        Token::LParen => {
            let expr = parse_expression(lexer, 0)?;
            expect_token(lexer, Token::RParen)?; 
            return Ok(expr);
        }

        //TODO: String literal
        _ => return Err(ParserError::UnexpectedToken(cur_token)),
    }  
}

// postfix_expression
// 	: primary_expression
// 	| postfix_expression '[' expression ']'
// 	| postfix_expression '(' ')'
// 	| postfix_expression '(' argument_expression_list ')'
// 	| postfix_expression '.' IDENTIFIER
// 	| postfix_expression PTR_OP IDENTIFIER
// 	| postfix_expression INC_OP
// 	| postfix_expression DEC_OP
fn parse_postfix(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    // The common left-recursion issue can be circumvented by realizing that
    // a postfix expression must begin with a primary expression
    let primary = parse_primary(lexer)?;
    let cur_token = lexer.next().ok_or(ParserError::EarlyLexerTermination)?;
    match cur_token {
        Token::LBracket => {
            let index = parse_expression(lexer, 0)?;
            expect_token(lexer, Token::RBracket)?;
            let node = Node::ArrayIndex(ArrayIndexNode { lvalue : primary, index });
            return Ok(NodePtr::new(node));
        }
        
        _ => return Err(ParserError::UnexpectedToken(cur_token)),
    }
}

// unary_expression
// 	: postfix_expression
// 	| INC_OP unary_expression
// 	| DEC_OP unary_expression
// 	| unary_operator cast_expression
// 	| SIZEOF unary_expression
// 	| SIZEOF '(' type_name ')'
fn parse_unary(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError>{
   
    //TODO: This needs to be a peek.
    let token = lexer.peek().ok_or(ParserError::EarlyLexerTermination)?;
    match token {
        Token::IntegerLiteral(n) => {
            lexer.next();
            let node = Node::Terminal(Token::IntegerLiteral(*n));
            return Ok(Box::new(node));
        }

        Token::Increment | Token::Decrement | Token::Ampersand | Token::Asterisk | 
        Token::Plus | Token::Minus | Token::Negation => {
            //TODO: casting. Currently this calls a unary expression
            // but a unary expression does not allow for casting.
            let operand = parse_unary(lexer)?;

            let node = Node::Prefix(
                UnaryNode { 
                    operator : *token,
                    operand,
                });
            
            return Ok(Box::new(node));
        }

        Token::Sizeof => {
            // TODO: Evaluate sizeof as a constant immediately!
            let cur_token = lexer.next().ok_or(ParserError::EarlyLexerTermination)?;
            if cur_token == Token::LParen {
                // Sizeof on type
                return Err(ParserError::NotImplemented);
                /*
                let type_token = parse_type(lexer)?;
                expect_token(lexer, Token::RParen);

                let prefix_node = Node::Prefix(
                        UnaryNode {
                            operator : Token::Sizeof,
                            operand : type_token
                        }
                    )

                return Ok( NodePtr::new( Node::Prefix(Node::Terminal(type_token))));
                */

            } else {
                // Sizeof also works on expressions! e.g. sizeof 123
                let operand = parse_unary(lexer)?;

                let node = Node::Prefix(
                    UnaryNode { 
                        operator : *token,
                        operand,
                    });
                
                return Ok(Box::new(node));
            }
        }

        _ => {
            return parse_postfix(lexer);
        }
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



