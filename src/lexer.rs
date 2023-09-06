use std::iter::Peekable;
use std::str::Chars;

use crate::arena::*;
use crate::typechecker::TypeAlias;


#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'t> {
    pub pos : Pos,
    pub token_type : TokenType<'t>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Pos {
    pub start_col : usize,
    pub end_col : usize,
    pub line_num : usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType<'t> {
    Identifier(&'t str),
    TypeName(&'t TypeAlias<'t>),

    IntLiteral(i32),
    LongLiteral(i64),

    UIntLiteral(u32),
    ULongLiteral(u64),

    FloatLiteral(f32),
    DoubleLiteral(f64),
    StringLiteral(&'t str),
    CharLiteral(char),

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
    pub lines : Vec<&'input str>,
    // absolute position of last character lexed
    char_pos : usize,           
    // absolute pos of where the line currently being lexed starts 
    cur_line_start : usize,   

    peek_token : Option<Token<'input>>,

    // Types defined by typedef
    pub type_aliases : Vec<&'input TypeAlias<'input>>,
}


impl <'input> Lexer <'input> {

    pub fn peek_token(&mut self) -> Option<Token<'input>> {
        //TODO: Can be done cleaner with map
        if let Some(tok) = self.peek_token {
            return Some(tok);
        } else {
            let peek_tok = self.next_token(); 
            self.peek_token = peek_tok;
            return peek_tok;
        }
    }

    pub fn next_token(&mut self) -> Option<Token<'input>> { 
        if let Some(tok) = self.peek_token {
            self.peek_token = None;
            return Some(tok);
        }

        // get rid of white space and comments first
        loop {
            let c = *self.peek_char()?;

            if c.is_whitespace() {
                self.next_char()?;
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
                            let c = self.next_char()?;

                            if c == '*' && *self.peek_char()? == '/' {
                                self.next_char();
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
        let pos = Pos { start_col, line_num, end_col : self.char_pos - self.cur_line_start};

        let token = Token { pos, token_type};
        Some(token)
    }
}



impl <'input> Lexer<'input> {
    pub fn new(input : &'input str, arena : &'input Arena) -> Self {
        Lexer {
            input, char_stream : input.chars().peekable(), arena,
            lines : Vec::new(), cur_line_start : 0, char_pos : 0, peek_token : None,
            type_aliases : Vec::new()
        }
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.char_stream.peek()
    }

    pub fn next_char(&mut self) -> Option<char>{
        let c = self.char_stream.next()?;
        self.char_pos += 1;

        if c == '\n' {
            let new_line = &self.input[self.cur_line_start..self.char_pos - 1];
            self.lines.push(new_line);
            self.cur_line_start = self.char_pos;
        }

        Some(c)
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
                        // If the string of characters is not keyword, the only other option is
                        // a previously defined type or else an identifier.


                        for alias in &self.type_aliases {
                            if alias.name == identifier_string {
                                return Some(TokenType::TypeName(alias));
                            }
                        }
                        
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

            '\'' => {
                // Char literal
                let char = self.next_char().unwrap();
                if let Some('\'') = self.next_char() {
                    return Some(TokenType::CharLiteral(char))
                } else {
                    // TODO: The lexer should really return errors
                    panic!("Char literal is malformed.")
                }
            }

            

            '0'..='9' => { 
                let num_literal_str = &mut String::new();
                num_literal_str.push(first_char);

                let mut is_hex = false; 
                if first_char == '0' {
                    if let Some('x') = self.peek_char() {
                        num_literal_str.push(self.next_char().unwrap());
                        is_hex = true;
                    }
                }

                enum LiteralType {
                    UnsuffixedDecimal,
                    UnsuffixedHex,
                    SuffixedU,
                    SuffixedL,
                    SuffixedBoth,
                    Float,
                    Double,
                }

                let mut l_type = LiteralType::UnsuffixedDecimal;
                

                while let Some(c) = self.peek_char() {
                    match *c {
                        '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'  => num_literal_str.push(*c),
                        '.' => {
                            num_literal_str.push(*c);
                            l_type = LiteralType::Float;
                        }
                        'u' | 'U' => {
                            self.next_char();
                            if let Some('l' | 'L') = self.peek_char() {
                                self.next_char();
                                l_type = LiteralType::SuffixedBoth;
                            } else {
                                l_type = LiteralType::SuffixedU;
                            }
                            break;
                        }

                        'l' | 'L' => {
                            self.next_char();
                            if let LiteralType::Float = l_type {
                                l_type = LiteralType::Double;
                            } else {
                                if let Some('u' | 'U') = self.peek_char() {
                                    self.next_char();
                                    l_type = LiteralType::SuffixedBoth;
                                } else {
                                    l_type = LiteralType::SuffixedL;
                                }
                            }
                            
                            break;
                        }

                        'f' | 'F' => {
                            l_type = LiteralType::Float;
                            break;
                        }


                        _ => break,
                    }
                    self.next_char();
                }

                // From the C standard:

                // The type of an integer constant is the first of the corresponding list in which its value can be represented: 
                // Unsuffixed decimal: int, long int, unsigned long int; 
                // unsuffixed octal or hexadecimal: int, unsigned int, long int, unsigned long int; 
                // suffixed by the letter u or U: unsigned int, unsigned long int; 
                // suffixed by the letter l or L: long int, unsigned long int; 
                // suffixed by both the letters u or U and l or L: unsigned long int . 

                match l_type {
                    LiteralType::UnsuffixedDecimal => {
                        if let Ok(i) = num_literal_str.parse::<i32>() {
                            return Some(TokenType::IntLiteral(i));
                        } else if let Ok(i) = num_literal_str.parse::<i64>() {
                            return Some(TokenType::LongLiteral(i));
                        } else if let Ok(i) = num_literal_str.parse::<u64>() {
                            return Some(TokenType::ULongLiteral(i));
                        } else {
                            panic!("Integer literal {num_literal_str} malformed or too large");
                        }
                    }
                    LiteralType::UnsuffixedHex => {
                        panic!("Hex Not implemented")
                    }
                    LiteralType::SuffixedU => {
                        if let Ok(i) = num_literal_str.parse::<u32>() {
                            return Some(TokenType::UIntLiteral(i));
                        } else if let Ok(i) = num_literal_str.parse::<u64>() {
                            return Some(TokenType::ULongLiteral(i));
                        } else {
                            panic!("Integer literal {num_literal_str} malformed or too large");
                        }
                    }
                    LiteralType::SuffixedL => {
                        if let Ok(i) = num_literal_str.parse::<i64>() {
                            return Some(TokenType::LongLiteral(i));
                        } else if let Ok(i) = num_literal_str.parse::<u64>() {
                            return Some(TokenType::ULongLiteral(i));
                        } else {
                            panic!("Integer literal {num_literal_str} malformed or too large");
                        }
                    }
                    LiteralType::SuffixedBoth => {
                        if let Ok(i) = num_literal_str.parse::<u64>() {
                            return Some(TokenType::ULongLiteral(i));
                        } else {
                            panic!("Integer literal {num_literal_str} malformed or too large");
                        }
                    }
                    LiteralType::Float => {
                        if let Some(lit) = num_literal_str.parse::<f32>().ok() {
                            return Some(TokenType::FloatLiteral(lit));
                        } else {
                            panic!("Float literal {num_literal_str} malformed or too large");
                        }
                    }

                    LiteralType::Double => {
                        if let Some(lit) = num_literal_str.parse::<f64>().ok() {
                            return Some(TokenType::DoubleLiteral(lit));
                        } else {
                            panic!("Float literal {num_literal_str} malformed or too large");
                        }
                    }
                    _ => panic!("Not implemented")
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

