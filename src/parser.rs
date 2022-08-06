use std::iter::Peekable;
use crate::lexer::*;

#[derive(Clone)]
pub enum BinaryOperator { 
    // Arithmetic Operators
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
    
    // Bitwise Operators
    AND,
    OR,
    XOR,
    LeftShift,
    RightShift,

    // Logical Operators
    ANDLogical,
    ORLogical,
    Equals,
    LessThan,
    GreaterThan,
    LessThanOrEq,
    GreaterThanOrEq,

    // Assignments
    Assign,
    PlusAssign,
    MinusAssign,
    MultAssign,
    DivAssign,
    ModAssign,
    ANDAssign,
    ORAssign,
    XORAssign,
    NegationAssign,
}

#[derive(Clone)]
pub enum UnaryOperator {
    PrefixIncrement,
    Decrement,
}

#[derive(Clone)]
pub enum TerminalType {
    Identifier(String),
    IntegerLiteral(i64),
    DoubleLiteral(f64),
}

type NodePtr = Box<Node>;

#[derive(Clone)]
pub enum Node { 
    BinaryNode { operator : BinaryOperator, left : NodePtr, right : NodePtr},
    UnaryNode { operator : UnaryOperator, operand : NodePtr},
    TerminalNode { val : TerminalType},
}

pub enum ParserError {
    ParseError,
    EarlyLexerTermination,
    UnknownPrecedence,
    UnexpectedOperator,
    UnexpectedOperand,
}

struct OperatorInfo {
    precedence : u8,
    left_associative : bool,
}

fn operator_lookup(token : &Token) -> Result<OperatorInfo, ParserError> {
    match token {
        Token::Mult | Token::Div | Token::Mod => {
            return Result::Ok(OperatorInfo{precedence : 90, left_associative : true});
        }

        Token::Plus | Token::Minus => {
            return Result::Ok(OperatorInfo{precedence : 80, left_associative : true});
        }
        
        _ => return Result::Err(ParserError::UnknownPrecedence),
    }
}

fn parse_atom<'a>(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError>{
   
    match lexer.next().ok_or(ParserError::EarlyLexerTermination)? {
        Token::IntegerLiteral(n) => {
            let node = Node::TerminalNode{ val : TerminalType::IntegerLiteral(n)};
            return Ok(Box::new(node));
        }
        _ => Err(ParserError::UnexpectedOperand),
    }
}


fn parse_expression(lexer : &mut Peekable<Lexer>, min_precedence : u8) -> Result<NodePtr, ParserError>{
    // Find next atom
    let mut result : NodePtr = parse_atom(lexer)?;
    


    // Stages:
    //

    loop {
        let mut peek_token : &Token = lexer.peek().ok_or(ParserError::EarlyLexerTermination)?;
        let op_info = operator_lookup(peek_token)?;
        
        if(op_info.precedence < min_precedence){
            return Ok(result);
        }

        let cur_token = &lexer.next().ok_or(ParserError::EarlyLexerTermination)?;

        let rhs : NodePtr;
        if op_info.left_associative {
            rhs = parse_expression(lexer, result, min_precedence + 1)?;
        } else { 
            rhs = parse_expression(lexer, result, min_precedence)?;
        }

        // Need to use Jones' trick here it seems
        let prev_result = std::mem::replace(&mut result, rhs);
        match cur_token {
            Token::Plus => result = Box::new(Node::BinaryNode{ operator : BinaryOperator::Plus, left : prev_result, right : rhs}),
            _ => return Err(ParserError::UnexpectedOperand),
        } 
    }
}



