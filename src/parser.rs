use std::iter::Peekable;
use crate::lexer::*;

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

pub enum UnaryOperator {
    PrefixIncrement,
    Decrement,
}

pub enum TerminalType {
    Identifier(String),
    IntegerLiteral(i64),
    DoubleLiteral(f64),
}

type NodePtr = Box<Node>;

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


fn parse_expression(lexer : &mut Peekable<Lexer>, left : NodePtr, min_precedence : u8) -> Result<NodePtr, ParserError>{
    // Find next atom
    let mut result : NodePtr = parse_atom(lexer)?;

    let mut op_token : &Token = lexer.peek().ok_or(ParserError::EarlyLexerTermination)?;
    let mut op_info = operator_lookup(op_token)?;

    while op_info.precedence >= min_precedence{
        // If the current operator has a higher precedence than the callee (min) precedence
        // then consume the operator and parse its right hand operand.
        let op_token = &lexer.next().ok_or(ParserError::EarlyLexerTermination)?;
        
        let rhs : NodePtr;
        if op_info.left_associative {
            rhs = parse_expression(lexer, result, min_precedence + 1)?;
        } else { 
            rhs = parse_expression(lexer, result, min_precedence)?;
        }

        // Once the right hand has been parsed, construct an appropriate node.
        match op_token {
            Token::Plus => result = Box::new(Node::BinaryNode { operator: BinaryOperator::Plus, left: result, right: rhs }),
            _ => return Err(ParserError::UnexpectedOperand),
        }

        op_token = lexer.peek().ok_or(ParserError::EarlyLexerTermination)?;
        op_info = operator_lookup(op_token)?;
    
    }

    return Ok(result);
}



