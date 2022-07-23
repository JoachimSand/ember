use std::io;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Eq)]
enum Token {
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

    let first_char : char;
    
    loop {
        match self.char_stream.peek() {
            Some(c) => {
                if c.is_whitespace() == false {
                    first_char = *c;
                    self.char_stream.next();
                    break;
                } else {
                    self.char_stream.next();
                }
            },
            None => return Some(Token::End),
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
            lex_operand!(chars = self.char_stream, chars, fallback = Token::Mult,
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
        '.' => return Some(Token::Dot,
        ';' => return Some(Token::Semicolon,
        ':' => return Some(Token::Colon,
        '(' => return Some(Token::LParen,
        ')' => return Some(Token::RParen,
        '{' => return Some(Token::LCurlyBracket,
        '}' => return Some(Token::RCurlyBracket,
        '[' => return Some(Token::LBracket,
        ']' => return Some(Token::RBracket,

        _ => {
            return Token::Unknown;
        }
    }
        
        return Some(Token::Unknown);
    }
}

fn lex_token(chars : &mut Peekable<Chars<'_>>) -> Token {
    
    let token_string = &mut String::new();

    let first_char : char;
    
    loop {
        match chars.peek() {
            Some(c) => {
                if c.is_whitespace() == false {
                    first_char = *c;
                    chars.next();
                    break;
                } else {
                    chars.next();
                }
            },
            None => return Token::End,
        }
    }

    match first_char {
        'a'..='z' | 'A'..='Z' | '_' => {
            // Attempt to parse identifier
            token_string.push(first_char);

            loop {
                // Only consume iterator if next char can be added to current token
                match chars.peek() {
                    Some(c) => {
                        if c.is_ascii_alphanumeric() || *c == '_' {
                            token_string.push(*c);
                            chars.next();
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
                return Token::Identifier(token_string.to_string());
            }

            match token_string.as_str() {
                "auto" => return Token::Auto,
                "break" => return Token::Break,
                "case" => return Token::Case,
                "char" => return Token::Char,
                "const" => return Token::Const,
                "continue" => return Token::Continue,
                "default" => return Token::Default,
                "do" => return Token::Do,
                "double" => return Token::Double,
                "else" => return Token::Else,
                "enum" => return Token::Enum,
                "extern" => return Token::Extern,
                "float" => return Token::Float,
                "for" => return Token::For,
                "goto" => return Token::Goto,
                "if" => return Token::If,
                "int" => return Token::Int,
                "long" => return Token::Long,
                "register" => return Token::Register,
                "return" => return Token::Return,
                "short" => return Token::Short,
                "signed" => return Token::Signed,
                "sizeof" => return Token::Sizeof,
                "static" => return Token::Static,
                "struct" => return Token::Struct,
                "switch" => return Token::Switch,
                "typedef" => return Token::Typedef,
                "union" => return Token::Union,
                "unsigned" => return Token::Unsigned,
                "void" => return Token::Void,
                "volatile" => return Token::Volatile,
                "while" => return Token::While,

                _ => return Token::Identifier(token_string.to_string()),
            }

        }

        '+' => {
            lex_operand!(chars = chars, fallback = Token::Plus, 
                '+' => Token::Increment, 
                '=' => Token::PlusAssign
            );
        }

        '-' => {
            lex_operand!(chars = chars, fallback = Token::Minus,
                '-' => Token::Decrement,
                '=' => Token::MinusAssign,
                '>' => Token::PtrOperand
            );
        }

        '*' => {
            lex_operand!(chars = chars, fallback = Token::Mult,
                '=' => Token::MultAssign
            );
        }

        '/' => {
            lex_operand!(chars = chars, fallback = Token::Div,
                '=' => Token::DivAssign
            );
        }

        '%' => {
            lex_operand!(chars = chars, fallback = Token::Mod,
                '=' => Token::ModAssign
            )
        }

        '&' => {
            lex_operand!(chars = chars, fallback = Token::AND,
                '&' => Token::ANDLogical,
                '=' => Token::ANDAssign
            );
        }

        '|' => {
            lex_operand!(chars = chars, fallback = Token::OR,
                '|' => Token::ORLogical,
                '=' => Token::ORAssign
            );
        }

        '^' => {
            lex_operand!(chars = chars, fallback = Token::XOR,
                '=' => Token::XORAssign
            );
        }

        '<' => {
            lex_operand!(chars = chars, fallback = Token::LessThan,
                '<' => Token::LeftShift,
                '=' => Token::LessThanOrEq
            );
        }

        '>' => {
            lex_operand!(chars = chars, fallback = Token::GreaterThan,
                '>' => Token::RightShift,
                '=' => Token::GreaterThanOrEq
            );
        } 

        '!' => {
            lex_operand!(chars = chars, fallback = Token::Negation,
                '=' => Token::NegationAssign
            );
        }

        '=' => {
            lex_operand!(chars = chars, fallback = Token::Assign,
                '=' => Token::Equals
            );
        }
        '?' => return Token::Ternary,
        '.' => return Token::Dot,
        ';' => return Token::Semicolon,
        ':' => return Token::Colon,
        '(' => return Token::LParen,
        ')' => return Token::RParen,
        '{' => return Token::LCurlyBracket,
        '}' => return Token::RCurlyBracket,
        '[' => return Token::LBracket,
        ']' => return Token::RBracket,

        _ => {
            return Token::Unknown;
        }
    }
}

fn main() {
    let stdin = io::stdin();
    let input = &mut String::new();

    let lexer : Lexer;
    loop {
        input.clear();
        match stdin.read_line(input) {
            Ok(n) => {
                println!("Recieved {n} bytes: {input}");
                //let mut tokens : Vec<Token> = Vec::new();  
                lexer.CharStream = input.chars().peekable();
                
                let mut cur_token : Token = lex_token(&mut char_stream);
                while cur_token != Token::End && cur_token != Token::Unknown {
                    println!("Token: {:?}", cur_token);
                    cur_token = lex_token(&mut char_stream);
                }
            },
            Err(error) => println!("Error: {error}"),
        };
    } 
}

