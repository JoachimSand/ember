use std::io;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Eq)]
enum Token {
    Identifier(String),
    
    // Expressions
    Equals,
    Plus,
    Minus,
    Mult,
    Increment,
    Decrement,
    PlusAssign,
    MinusAssign,
    MultAssign,    

    PtrOperand,
    

    End,
    Unknown,
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
        'a'..='z' | 'A'..='Z' => {
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
                            return Token::Identifier(token_string.to_string());
                        }
                    },
                    None => return Token::Identifier(token_string.to_string()),
                }
            }
        }

        '+' => {
            match chars.peek() {
                Some(c) => {
                    match *c {
                        '+' => {
                            chars.next();
                            return Token::Increment;
                        }
                        '=' => {
                            chars.next();
                            return Token::PlusAssign;
                        }
                        _ => {
                            return Token::Plus;
                        }
                    }
                },
                None => return Token::Plus,
            }
        }

        '-' => {
            match chars.peek() {
                Some(c) => {
                    match *c {
                        '-' => {
                            chars.next();
                            return Token::Decrement;
                        }
                        '=' => {
                            chars.next();
                            return Token::MinusAssign;
                        }
                        '>' => {
                            chars.next();
                            return Token::PtrOperand;
                        }
                        _ => {
                            return Token::Minus;
                        }
                    }
                },
                None => return Token::Minus,
            }
        }

        '*' => {
            match chars.peek() {
                Some(c) => {
                    match *c {
                        '=' => {
                            chars.next();
                            return Token::MultAssign;
                        }
                        _ => {
                            return Token::Mult;
                        }
                    }
                },
                None => return Token::Mult,
            }
            
        }


        
        _ => {
            return Token::Unknown;
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
                let mut chars : Peekable<Chars<'_>>= input.chars().peekable();
                
                let mut cur_token : Token = lex_token(&mut chars);
                while cur_token != Token::End && cur_token != Token::Unknown {
                    println!("Token: {:?}", cur_token);
                    cur_token = lex_token(&mut chars);
                }
            },
            Err(error) => println!("Error: {error}"),
        };
    } 
}

