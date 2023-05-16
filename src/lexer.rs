use std::iter::Peekable;
use std::str::Chars;

use crate::arena::*;


#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'t> {
    pos : Pos,
    pub token_type : TokenType<'t>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct Pos {
    start_col : usize,
    end_col : usize,
    line_num : usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType<'t> {
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
        lexer = $c:expr,
        fallback => $f:expr,
        $(
            $s:pat => $t:expr  
        ),*
    ) => {{  
        match $c.peek_char() {
            Some(c) => {
                match *c {
                    $(
                        $s => {
                            $c.next_char();
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
    input : &'input str,
    char_stream : Peekable<Chars<'input>>,
    arena : &'input Arena,

    // references to lines in input
    lines : Vec<&'input str>,
    // absolute position of last character lexed
    char_pos : usize,           
    // absolute pos of where the line currently being lexed starts 
    cur_line_start : usize,     
}


impl <'input> Iterator for Lexer <'input> { 
   
    type Item = Token<'input>; 

    fn next(&mut self) -> Option<Self::Item> {    

        // get rid of white space and comments first
        loop {
            let mut c = *self.peek_char()?;

            if c.is_whitespace() {
                c = self.next_char()?;
            } else if c == '/' {
                self.next_char();
                match self.peek_char()? {
                    '/' => {
                        // Handle one-line comments!
                        while self.next_char()? != '\n' {}
                    },
                    '*' => {
                        // Handle multi-line comments!
                        loop {
                            let c = self.char_stream.next()?;

                            if c == '*' && *self.char_stream.peek()? == '/' {
                                self.char_stream.next();
                                break;
                            }
                        }
                    }
                    _ => break,
                }
            } else {
                break;
            }
        }

        
        // track position before and after lexing token.
        // This can be used to track the size and pos of the token
        let line_num = self.lines.len();
        let start_col = self.char_pos - self.cur_line_start;

        let token_type = self.next_token_type()?;
        let pos = Pos { start_col, line_num, end_col : self.char_pos};

        let token = Token { pos, token_type};
        println!("{token:?}");
        Some(token)
    }
}



impl <'input> Lexer<'input> {
    pub fn new(input : &'input str, arena : &'input Arena) -> Self {
        Lexer {
            input, char_stream : input.chars().peekable(), arena,
            lines : Vec::new(), cur_line_start : 0, char_pos : 0
        }
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.char_stream.peek()
    }

    fn next_char(&mut self) -> Option<char>{
        let c = self.char_stream.next()?;
        self.char_pos += 1;

        if c == '\n' {
            self.lines.push(&self.input[self.cur_line_start..self.char_pos - 1]);
            println!("Line {}", self.lines.last().unwrap());
            self.cur_line_start = self.char_pos;
        }

        Some(c)
    }

    fn generate_error(pos : Pos, msg : &str) {

    }


    fn next_token_type(&mut self) -> Option<TokenType<'input>> {    
        let mut first_char = self.next_char()?;

        match first_char {
            'a'..='z' | 'A'..='Z' | '_' => {
                
                let identifier_string = &mut String::new();

                // Attempt to parse identifier
                identifier_string.push(first_char);

                while let Some(c) = self.peek_char() {
                    // Only consume iterator if next char can be added to current token
                    if c.is_ascii_alphanumeric() || *c == '_' {
                        identifier_string.push(*c);
                        self.next_char();
                    } else {
                        break;
                    }
                }
                
                if identifier_string.chars().count() >= 10 {
                    // No keyword is 10 characters or longer

                    // TODO: Remove this unwrap when capability of mem arena has been improved
                    let identifier = self.arena.push_str(identifier_string.as_str()).unwrap();
                    return Some(TokenType::Identifier(identifier));
                }

                match identifier_string.as_str() {
                    "auto" => return Some(TokenType::Auto),
                    "break" => return Some(TokenType::Break),
                    "case" => return Some(TokenType::Case),
                    "char" => return Some(TokenType::Char),
                    "const" => return Some(TokenType::Const),
                    "continue" => return Some(TokenType::Continue),
                    "default" => return Some(TokenType::Default),
                    "do" => return Some(TokenType::Do),
                    "double" => return Some(TokenType::Double),
                    "else" => return Some(TokenType::Else),
                    "enum" => return Some(TokenType::Enum),
                    "extern" => return Some(TokenType::Extern),
                    "float" => return Some(TokenType::Float),
                    "for" => return Some(TokenType::For),
                    "goto" => return Some(TokenType::Goto),
                    "if" => return Some(TokenType::If),
                    "int" => return Some(TokenType::Int),
                    "long" => return Some(TokenType::Long),
                    "register" => return Some(TokenType::Register),
                    "return" => return Some(TokenType::Return),
                    "short" => return Some(TokenType::Short),
                    "signed" => return Some(TokenType::Signed),
                    "sizeof" => return Some(TokenType::Sizeof),
                    "static" => return Some(TokenType::Static),
                    "struct" => return Some(TokenType::Struct),
                    "switch" => return Some(TokenType::Switch),
                    "typedef" => return Some(TokenType::Typedef),
                    "union" => return Some(TokenType::Union),
                    "unsigned" => return Some(TokenType::Unsigned),
                    "void" => return Some(TokenType::Void),
                    "volatile" => return Some(TokenType::Volatile),
                    "while" => return Some(TokenType::While),

                    _ => {
                        let identifier = self.arena.push_str(identifier_string.as_str()).unwrap();
                        return Some(TokenType::Identifier(identifier));
                    }
                }
            }

            '"' => {

                let string_literal = &mut String::new();

                while let Some(c) = self.next_char() {
                    if c == '\"' {
                        break;
                    }

                    if c == '\\' {
                        let escaped_char = self.next_char()?;
                        string_literal.push(escaped_char);
                        
                    } else if c.is_ascii() {
                        string_literal.push(c);
                    }
                }

                let literal = self.arena.push_str(string_literal.as_str()).unwrap();
                return Some(TokenType::StringLiteral(literal));
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

                while let Some(c) = self.peek_char() {
                    if c.is_ascii_digit() || matches!(c, '.') {
                        num_literal_str.push(*c); 
                        
                        if *c == '.' {
                            l_type = LiteralType::Double;
                        }
                        self.next_char();
                    } else {
                        if *c == 'f' {
                            self.next_char();
                            l_type = LiteralType::Float;
                        }
                        break;
                    }
                }

                match l_type {
                    LiteralType::Int => return Some(TokenType::IntegerLiteral(num_literal_str.parse().ok()?)),
                    LiteralType::Float => return Some(TokenType::FloatLiteral(num_literal_str.parse::<f32>().ok()?)),
                    LiteralType::Double => return Some(TokenType::DoubleLiteral(num_literal_str.parse::<f64>().ok()?))
                }

            }

            '+' => {
                lex_operand!(lexer = self, 
                    fallback => return Some(TokenType::Plus), 
                    '+' => return Some(TokenType::Increment),  
                    '=' => return Some(TokenType::PlusAssign) 
                );
            }

            '-' => {
                lex_operand!(lexer = self, 
                    fallback => return Some(TokenType::Minus), 
                    '-' => return Some(TokenType::Decrement),  
                    '=' => return Some(TokenType::MinusAssign),  
                    '>' => return Some(TokenType::PtrOperand)
                );
            }

            '*' => {
                lex_operand!(lexer = self, 
                    fallback => return Some(TokenType::Asterisk), 
                    '=' => return Some(TokenType::MultAssign)  
                );
            }

            '/' => {
                lex_operand!(lexer = self, 
                    fallback => return Some(TokenType::Div), 
                    '=' => return Some(TokenType::DivAssign) 
                );
            }

            '%' => {
                lex_operand!(lexer = self, 
                    fallback => return Some(TokenType::Mod), 
                    '=' => return Some(TokenType::ModAssign)  
                )
            }

            '&' => {
                lex_operand!(lexer = self, 
                    fallback => return Some(TokenType::Ampersand), 
                    '&' => return Some(TokenType::ANDLogical),  
                    '=' => return Some(TokenType::ANDAssign)  
                );
            }

            '|' => {
                lex_operand!(lexer = self, 
                    fallback => return Some(TokenType::OR), 
                    '|' => return Some(TokenType::ORLogical),  
                    '=' => return Some(TokenType::ORAssign)
                );
            }

            '^' => {
                lex_operand!(lexer = self, 
                    fallback => return Some(TokenType::XOR), 
                    '=' => return Some(TokenType::XORAssign)
                );
            }

            '<' => {
                lex_operand!(lexer = self, 
                    fallback => return Some(TokenType::LessThan), 
                    '<' => {
                        lex_operand!(lexer = self, 
                            fallback => return Some(TokenType::LeftShift),
                            '=' => return Some(TokenType::LeftShiftAssign)
                        );
                    },  
                    '=' => return Some(TokenType::LessThanOrEq)  
                );
            }

            '>' => {
                lex_operand!(lexer = self, 
                    fallback => return Some(TokenType::GreaterThan), 
                    '>' => {
                        lex_operand!(lexer = self, 
                            fallback => return Some(TokenType::RightShift),
                            '=' => return Some(TokenType::RightShiftAssign)
                        );
                    },  
                    '=' => return Some(TokenType::GreaterThanOrEq)
                );
            } 


            '!' => {
                lex_operand!(lexer = self, 
                    fallback => return Some(TokenType::NegationLogical), 
                    '=' => return Some(TokenType::NotEquals)
                );
            }

            '=' => {
                lex_operand!(lexer = self, 
                    fallback => return Some(TokenType::Assign), 
                    '=' => return Some(TokenType::Equals)
                );
            }
            '~' => return Some(TokenType::Negation),
            '?' => return Some(TokenType::Ternary),
            '.' => return Some(TokenType::Dot),
            ',' => return Some(TokenType::Comma),
            ';' => return Some(TokenType::Semicolon),
            ':' => return Some(TokenType::Colon),
            '(' => return Some(TokenType::LParen), 
            ')' => return Some(TokenType::RParen),    
            '{' => return Some(TokenType::LCurlyBracket),  
            '}' => return Some(TokenType::RCurlyBracket),  
            '[' => return Some(TokenType::LBracket),  
            ']' => return Some(TokenType::RBracket),  

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

