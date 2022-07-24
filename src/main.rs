use std::io;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(String),
    
    // Arithmetic Operators
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
    Increment,
    Decrement,
    
    // Binary Operators
    AND,
    OR,
    XOR,
    Negation,
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
    Ternary,
    PtrOperand,
    Dot,
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
        fallback = $f:expr,
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
                            return Some($t);
                        }    
                    )*
                    _ => {
                        return Some($f);
                    }
                }
            },
            None => return Some($f),
        }
    }};
}

pub struct Lexer<'input> {
    char_stream : Peekable<Chars<'input>>,
}

impl <'input> Iterator for Lexer <'input> { 
   

    type Item = Token;
    

    fn next(&mut self) -> Option<Token> {    
        let token_string = &mut String::new();

        let mut first_char : char;
        
        loop {
            first_char = self.char_stream.next()?;

            if first_char.is_whitespace() == false {
                break;
            }
        }

        match first_char {
            'a'..='z' | 'A'..='Z' | '_' => {
                // Attempt to parse identifier
                token_string.push(first_char);

                loop {
                    // Only consume iterator if next char can be added to current token
                    match self.char_stream.peek() {
                        Some(c) => {
                            if c.is_ascii_alphanumeric() || *c == '_' {
                                token_string.push(*c);
                                self.char_stream.next();
                            } else {
                                break;
                                //return Token::Identifier(token_string.to_string());
                            }
                        },
                        None => break, 
                        //return Token::Identifier(token_string.to_string()),
                    }
                }
                
                if token_string.chars().count() >= 10 {
                    // No keyword is 10 characters or longer
                    return Some(Token::Identifier(token_string.to_string()));
                }

                match token_string.as_str() {
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

                    _ => return Some(Token::Identifier(token_string.to_string())),
                }

            }

            '+' => {
                lex_operand!(chars = self.char_stream, fallback = Token::Plus, 
                    '+' => Token::Increment, 
                    '=' => Token::PlusAssign
                );
            }

            '-' => {
                lex_operand!(chars = self.char_stream, fallback = Token::Minus,
                    '-' => Token::Decrement,
                    '=' => Token::MinusAssign,
                    '>' => Token::PtrOperand
                );
            }

            '*' => {
                lex_operand!(chars = self.char_stream, fallback = Token::Mult,
                    '=' => Token::MultAssign
                );
            }

            '/' => {
                lex_operand!(chars = self.char_stream, fallback = Token::Div,
                    '=' => Token::DivAssign
                );
            }

            '%' => {
                lex_operand!(chars = self.char_stream, fallback = Token::Mod,
                    '=' => Token::ModAssign
                )
            }

            '&' => {
                lex_operand!(chars = self.char_stream, fallback = Token::AND,
                    '&' => Token::ANDLogical,
                    '=' => Token::ANDAssign
                );
            }

            '|' => {
                lex_operand!(chars = self.char_stream, fallback = Token::OR,
                    '|' => Token::ORLogical,
                    '=' => Token::ORAssign
                );
            }

            '^' => {
                lex_operand!(chars = self.char_stream, fallback = Token::XOR,
                    '=' => Token::XORAssign
                );
            }

            '<' => {
                lex_operand!(chars = self.char_stream, fallback = Token::LessThan,
                    '<' => Token::LeftShift,
                    '=' => Token::LessThanOrEq
                );
            }

            '>' => {
                lex_operand!(chars = self.char_stream, fallback = Token::GreaterThan,
                    '>' => Token::RightShift,
                    '=' => Token::GreaterThanOrEq
                );
            } 

            '!' => {
                lex_operand!(chars = self.char_stream, fallback = Token::Negation,
                    '=' => Token::NegationAssign
                );
            }

            '=' => {
                lex_operand!(chars = self.char_stream, fallback = Token::Assign,
                    '=' => Token::Equals
                );
            }
            '?' => return Some(Token::Ternary),
            '.' => return Some(Token::Dot),
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

fn main() {
    let stdin = io::stdin();
    let input = &mut String::new();

    loop {
        input.clear();
        match stdin.read_line(input) {
            Ok(n) => {
                println!("Recieved {n} bytes: {input}");
                //let mut tokens : Vec<Token> = Vec::new();  
                let lexer = Lexer{ char_stream : input.chars().peekable()};

                for cur_token in lexer { 
                    println!("Token: {:?}", cur_token);
                }
            },
            Err(error) => println!("Error: {error}"),
        };
    } 
}

