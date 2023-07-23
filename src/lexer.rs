use std::fs;

#[derive(Debug)]
enum LexerError {
    EOF,
}

fn error_to_string(err: LexerError) -> String {
    match err {
        LexerError::EOF => { String::from("Reached End of File! ") }
        e =>  { unimplemented!("error_to_string() for {:?}", e) }
    }
}

#[derive(Debug)]
enum TokenType {
    Invalid,
    Name,
    Number,
    OpenParenthesis,
    ClosingParenthesis,
    OpenCurly,
    ClosingCurly,
    Symbol
}

#[derive(Debug)]
struct Location {
    file: String,
    row: usize,
    col: usize,
}

#[derive(Debug)]
struct Token {
    typ: TokenType,
    value: String,
    loc: Location
}

#[derive(Debug)]
pub struct Lexer {
    origin: String,
    source: Vec<char>,
    tokens: Vec<Token>,
    current_char: usize,
    current_line: usize,
    line_start: usize,
}

impl Lexer {
    pub fn new(origin_path: &String) -> Result<Self, String> {
        match fs::read_to_string(origin_path) {
            Ok(source) => {
                Ok(Lexer { origin: origin_path.to_string(), source: source.chars().collect(), tokens: vec![], current_char: 0, current_line: 1, line_start: 0 })
            },
            Err(e) => {
                Err(e.to_string())
            }
        }
    }

    fn get_location(&self) -> Location {
        Location { file: self.origin.clone(), row: self.current_line, col: self.current_char - self.line_start }
    }

    fn is_eof(&self) -> bool {
        self.current_char >= self.source.len()
    }

    fn next_char(&mut self) -> Result<char, String> {
        if self.is_eof() {
            Err(error_to_string(LexerError::EOF))
        } else {
            let c = self.source[self.current_char];
            self.current_char += 1;
            Ok(c)
        }
    }

    fn trim_whitespace(&mut self) -> Result<(), String> {
        while !self.is_eof() && self.source[self.current_char].is_whitespace() {
            match self.source[self.current_char] {
                '\r' => { self.current_char += 2; self.current_line += 1; self.line_start = self.current_char; }, // Windows
                '\n' => { self.current_char += 1; self.current_line += 1; self.line_start = self.current_char; }, // Linux
                _    => { self.current_char += 1; } // Normal space
            }
        }
        Ok(())
    }

    fn next_token(&mut self) -> Result<Token, String> {
        let c = self.next_char()?;
        match c {
            '0'..='9' => {
                let mut value = String::from(c);
                while let Ok(nc) = self.next_char() {
                    if !nc.is_ascii_digit() {
                        self.current_char -= 1; // Went too far, go a step back
                        break;
                    }
                    value.push(nc);
                }
                Ok( Token { typ: TokenType::Number, value, loc: self.get_location() } )
            },
            'A'..='z' => {
                let mut value = String::from(c);
                while let Ok(nc) = self.next_char() {
                    if !nc.is_alphanumeric() {
                        self.current_char -= 1; // Went too far, go a step back
                        break;
                    }
                    value.push(nc);
                }
                Ok( Token { typ: TokenType::Name, value, loc: self.get_location() } )
            },
            '(' => {
                Ok( Token { typ: TokenType::OpenParenthesis, value: String::from("("), loc: self.get_location() } )
            },
            ')' => {
                Ok( Token { typ: TokenType::ClosingParenthesis, value: String::from(")"), loc: self.get_location() } )
            },
            '{' => {
                Ok( Token { typ: TokenType::OpenCurly, value: String::from("{"), loc: self.get_location() } )
            },
            '}' => {
                Ok( Token { typ: TokenType::ClosingCurly, value: String::from("}"), loc: self.get_location() } )
            },
            '=' => {
                Ok( Token { typ: TokenType::Symbol, value: String::from(c), loc: self.get_location() } )
            }
            e => {
                Ok( Token { typ: TokenType::Invalid, value: String::from(e), loc: self.get_location() } )
            }
        }
    }

    pub fn tokenize(&mut self) -> Result<(), String> {
        while !self.is_eof() {
            self.trim_whitespace()?;
            let t = self.next_token()?;
            println!("{:?}", t);
            self.tokens.push(t);
        }
        Ok(())
    }
}