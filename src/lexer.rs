use std::iter::Peekable;
use std::str::Chars;

use crate::arena::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token<'t> {
    Identifier(&'t str),

    IntegerLiteral(i64),
    FloatLiteral(f32),
    DoubleLiteral(f64),
    StringLiteral(&'t str),

    // Arithmetic Operators
    Plus,
    Minus,
    Div,
    Mod,
    Increment,
    Decrement,
    
    // Binary Operators
    OR,
    XOR,
    Negation,
    LeftShift,
    RightShift,

    // Logical Operators
    ANDLogical,
    ORLogical,
    NegationLogical,
    Equals,
    NotEquals,
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
    LeftShiftAssign,
    RightShiftAssign,

    // Keywords
    Auto, 
    Break, 
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Int,
    Long,
    Register,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,

    // Misc.
    Asterisk,
    Ampersand,
    Ternary,
    PtrOperand,
    Dot,
    Comma,
    Colon,
    Semicolon,
    LParen,
    RParen,
    LCurlyBracket,
    RCurlyBracket,
    LBracket,
    RBracket,
    End,
    Unknown,
}

#[macro_export]
macro_rules! lex_operand {
    (
        chars = $c:expr,
        fallback => $f:expr,
        $(
            $s:pat => $t:expr  
        ),*
    ) => {{  
        match $c.peek() {
            Some(c) => {
                match *c {
                    $(
                        $s => {
                            $c.next();
                            $t;
                        }    
                    )*
                    _ => {
                        $f;
                    }
                }
            },
            None => $f,
        }
    }};
}

pub struct Lexer<'input> {
    char_stream : Peekable<Chars<'input>>,
    arena : &'input Arena
}

impl <'input> Iterator for Lexer <'input> { 
   
    type Item = Token<'input>; 

    fn next(&mut self) -> Option<Self::Item> {    

        let token = self.advance_tokens();
        println!("{:?}", token);
        return token;
    }
}

impl <'input> Lexer<'input> {
    pub fn new(contents : &'input str, arena : &'input Arena) -> Self {
        Lexer { 
            char_stream : contents.chars().peekable(), 
            arena
        }
    }

    fn advance_tokens(&mut self) -> Option<Token<'input>> {    
        let mut first_char : char;
        
        loop {
            first_char = self.char_stream.next()?;

            if first_char.is_whitespace() == false {
                break;
            }
        }

        match first_char {
            'a'..='z' | 'A'..='Z' | '_' => {
                
                let identifier_string = &mut String::new();

                // Attempt to parse identifier
                identifier_string.push(first_char);

                loop {

                    // Only consume iterator if next char can be added to current token
                    match self.char_stream.peek() {
                        Some(c) => {
                            if c.is_ascii_alphanumeric() || *c == '_' {
                                identifier_string.push(*c);
                                self.char_stream.next();
                            } else {
                                break;
                            }
                        },
                        None => break, 
                    }
                }
                
                if identifier_string.chars().count() >= 10 {
                    // No keyword is 10 characters or longer

                    // TODO: Remove this unwrap when capability of mem arena has been improved
                    let identifier = self.arena.push_str(identifier_string.as_str()).unwrap();
                    return Some(Token::Identifier(identifier));
                }

                match identifier_string.as_str() {
                    "auto" => return Some(Token::Auto),
                    "break" => return Some(Token::Break),
                    "case" => return Some(Token::Case),
                    "char" => return Some(Token::Char),
                    "const" => return Some(Token::Const),
                    "continue" => return Some(Token::Continue),
                    "default" => return Some(Token::Default),
                    "do" => return Some(Token::Do),
                    "double" => return Some(Token::Double),
                    "else" => return Some(Token::Else),
                    "enum" => return Some(Token::Enum),
                    "extern" => return Some(Token::Extern),
                    "float" => return Some(Token::Float),
                    "for" => return Some(Token::For),
                    "goto" => return Some(Token::Goto),
                    "if" => return Some(Token::If),
                    "int" => return Some(Token::Int),
                    "long" => return Some(Token::Long),
                    "register" => return Some(Token::Register),
                    "return" => return Some(Token::Return),
                    "short" => return Some(Token::Short),
                    "signed" => return Some(Token::Signed),
                    "sizeof" => return Some(Token::Sizeof),
                    "static" => return Some(Token::Static),
                    "struct" => return Some(Token::Struct),
                    "switch" => return Some(Token::Switch),
                    "typedef" => return Some(Token::Typedef),
                    "union" => return Some(Token::Union),
                    "unsigned" => return Some(Token::Unsigned),
                    "void" => return Some(Token::Void),
                    "volatile" => return Some(Token::Volatile),
                    "while" => return Some(Token::While),

                    _ => {
                        let identifier = self.arena.push_str(identifier_string.as_str()).unwrap();
                        return Some(Token::Identifier(identifier));
                    }
                }
            }

            '"' => {

                let string_literal = &mut String::new();

                while let Some(c) = self.char_stream.next() {
                    if c == '\"' {
                        break;
                    }

                    if c == '\\' {
                        let escaped_char = self.char_stream.next()?;
                        string_literal.push(escaped_char);
                        
                    } else if c.is_ascii() {
                        string_literal.push(c);
                    }
                }

                let literal = self.arena.push_str(string_literal.as_str()).unwrap();
                return Some(Token::StringLiteral(literal));
            }

            

            '0'..='9' => { 
                let num_literal_str = &mut String::new();
                num_literal_str.push(first_char);

                enum LiteralType {
                    Int,
                    Float,
                    Double,
                }

                let mut l_type = LiteralType::Int;

                while let Some(c) = self.char_stream.peek() {
                    if c.is_ascii_digit() || matches!(c, '.') {
                        num_literal_str.push(*c); 
                        
                        if *c == '.' {
                            l_type = LiteralType::Double;
                        }
                        self.char_stream.next();
                    } else {
                        if *c == 'f' {
                            self.char_stream.next();
                            l_type = LiteralType::Float;
                        }
                        break;
                    }
                }

                match l_type {
                    LiteralType::Int => return Some(Token::IntegerLiteral(num_literal_str.parse().ok()?)),
                    LiteralType::Float => return Some(Token::FloatLiteral(num_literal_str.parse::<f32>().ok()?)),
                    LiteralType::Double => return Some(Token::DoubleLiteral(num_literal_str.parse::<f64>().ok()?))
                }

            }

            '+' => {
                lex_operand!(chars = self.char_stream, 
                    fallback => return Some(Token::Plus), 
                    '+' => return Some(Token::Increment),  
                    '=' => return Some(Token::PlusAssign) 
                );
            }

            '-' => {
                lex_operand!(chars = self.char_stream, 
                    fallback => return Some(Token::Minus), 
                    '-' => return Some(Token::Decrement),  
                    '=' => return Some(Token::MinusAssign),  
                    '>' => return Some(Token::PtrOperand)
                );
            }

            '*' => {
                lex_operand!(chars = self.char_stream, 
                    fallback => return Some(Token::Asterisk), 
                    '=' => return Some(Token::MultAssign)  
                );
            }

            '/' => {
                lex_operand!(chars = self.char_stream, 
                    fallback => return Some(Token::Div), 
                    '=' => return Some(Token::DivAssign), 
                    '/' => {
                        // Handle one-line comments!
                        loop {
                            let c = self.char_stream.next()?;

                            if c == '\n' {
                                return self.next();
                            }
                        }
                    },
                    '*' => {
                        // Handle multi-line comments!
                        loop {
                            let c = self.char_stream.next()?;

                            if c == '*' {
                                if *self.char_stream.peek()? == '/' { 
                                    self.char_stream.next();
                                    return self.next();
                                }
                            }
                        }
                    }
                );
            }

            '%' => {
                lex_operand!(chars = self.char_stream, 
                    fallback => return Some(Token::Mod), 
                    '=' => return Some(Token::ModAssign)  
                )
            }

            '&' => {
                lex_operand!(chars = self.char_stream, 
                    fallback => return Some(Token::Ampersand), 
                    '&' => return Some(Token::ANDLogical),  
                    '=' => return Some(Token::ANDAssign)  
                );
            }

            '|' => {
                lex_operand!(chars = self.char_stream, 
                    fallback => return Some(Token::OR), 
                    '|' => return Some(Token::ORLogical),  
                    '=' => return Some(Token::ORAssign)
                );
            }

            '^' => {
                lex_operand!(chars = self.char_stream, 
                    fallback => return Some(Token::XOR), 
                    '=' => return Some(Token::XORAssign)
                );
            }

            '<' => {
                lex_operand!(chars = self.char_stream, 
                    fallback => return Some(Token::LessThan), 
                    '<' => {
                        lex_operand!(chars = self.char_stream, 
                            fallback => return Some(Token::LeftShift),
                            '=' => return Some(Token::LeftShiftAssign)
                        );
                    },  
                    '=' => return Some(Token::LessThanOrEq)  
                );
            }

            '>' => {
                lex_operand!(chars = self.char_stream, 
                    fallback => return Some(Token::GreaterThan), 
                    '>' => {
                        lex_operand!(chars = self.char_stream, 
                            fallback => return Some(Token::RightShift),
                            '=' => return Some(Token::RightShiftAssign)
                        );
                    },  
                    '=' => return Some(Token::GreaterThanOrEq)
                );
            } 


            '!' => {
                lex_operand!(chars = self.char_stream, 
                    fallback => return Some(Token::NegationLogical), 
                    '=' => return Some(Token::NotEquals)
                );
            }

            '=' => {
                lex_operand!(chars = self.char_stream, 
                    fallback => return Some(Token::Assign), 
                    '=' => return Some(Token::Equals)
                );
            }
            '~' => return Some(Token::Negation),
            '?' => return Some(Token::Ternary),
            '.' => return Some(Token::Dot),
            ',' => return Some(Token::Comma),
            ';' => return Some(Token::Semicolon),
            ':' => return Some(Token::Colon),
            '(' => return Some(Token::LParen), 
            ')' => return Some(Token::RParen),    
            '{' => return Some(Token::LCurlyBracket),  
            '}' => return Some(Token::RCurlyBracket),  
            '[' => return Some(Token::LBracket),  
            ']' => return Some(Token::RBracket),  

            _ => {
                //return Some(Token::Unknown);
                
                // This is more idiomatic rust - however, it blurs
                // the line between whether 
                // 1. No more chars -> therefore no more tokens
                // and 2. current token not recognised.
                return None;
            }
        }    
    }
}

