use std::io;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Eq)]
enum Token {
    Identifier(String),
    Plus,
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
                        if c.is_ascii_alphanumeric() {
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
            return Token::Plus;
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

