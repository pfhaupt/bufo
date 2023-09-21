use std::fmt::{Debug, Formatter};
use std::fs;

use crate::codegen::ERR_STR;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    File,
    CharLiteral,
    StrLiteral,
    IntLiteral,
    OpenRound,
    ClosingRound,
    OpenCurly,
    ClosingCurly,
    OpenSquare,
    ClosingSquare,
    ClassKeyword,
    FunctionKeyword,
    FeatureKeyword,
    LetKeyword,
    IfKeyword,
    ElseKeyword,
    ReturnKeyword,
    Colon,
    Semi,
    Comma,
    Dot,
    Arrow,
    Equal,
    Plus,
    Minus,
    Asterisk,
    ForwardSlash,
    CmpEq,
    CmpNeq,
    CmpLt,
    CmpLte,
    CmpGt,
    CmpGte,
    Identifier,
    Eof,
}

pub const COMPARATOR_TYPES: [TokenType; 6] = [
    TokenType::CmpEq,
    TokenType::CmpNeq,
    TokenType::CmpGt,
    TokenType::CmpGte,
    TokenType::CmpLt,
    TokenType::CmpLte,
];

#[derive(Clone, PartialEq, Eq)]
pub struct Location {
    file: String,
    row: usize,
    col: usize,
}

impl Location {
    pub fn new(file: String, row: usize, col: usize) -> Self {
        Self { file, row, col }
    }
}

impl Debug for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}:{}:{}", self.file, self.row, self.col)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    typ: TokenType, // type is a reserved keyword in Rust :(
    value: String,
    loc: Location,
}

impl Token {
    pub fn new(typ: TokenType, value: String, loc: Location) -> Self {
        Self { typ, value, loc }
    }

    pub fn get_type(&self) -> TokenType {
        self.typ
    }

    pub fn get_value(&self) -> String {
        self.value.clone()
    }

    pub fn get_loc(&self) -> Location {
        self.loc.clone()
    }
}

#[derive(Debug)]
pub struct Lexer {
    origin: String,
    source: Vec<char>,
    tokens: Vec<Token>,
    current_char: usize,
    current_line: usize,
    line_start: usize,
    #[allow(unused)]
    print_debug: bool,
}

impl Lexer {
    pub fn new(origin_path: &String, print_debug: bool) -> Result<Self, String> {
        match fs::read_to_string(origin_path) {
            Ok(source) => Ok(Lexer {
                origin: origin_path.to_string(),
                source: source.chars().collect(),
                tokens: vec![],
                current_char: 0,
                current_line: 1,
                line_start: 0,
                print_debug,
            }),
            Err(_) => Err(format!(
                "{}: Could not find input file `{}`.",
                ERR_STR, origin_path
            )),
        }
    }

    fn get_location(&self) -> Location {
        Location {
            file: self.origin.clone(),
            row: self.current_line,
            col: self.current_char - self.line_start,
        }
    }

    fn is_eof(&self) -> bool {
        self.current_char >= self.source.len()
    }

    fn next_char(&mut self) -> Result<char, String> {
        if self.is_eof() {
            Err(format!("{}: Reached End Of File while lexing.", ERR_STR))
        } else {
            let c = self.source[self.current_char];
            self.current_char += 1;
            Ok(c)
        }
    }

    fn trim_whitespace(&mut self) -> Result<(), String> {
        while !self.is_eof() && self.source[self.current_char].is_whitespace() {
            match self.source[self.current_char] {
                '\r' => {
                    self.current_char += 2;
                    self.current_line += 1;
                    self.line_start = self.current_char;
                } // Windows
                '\n' => {
                    self.current_char += 1;
                    self.current_line += 1;
                    self.line_start = self.current_char;
                } // Linux
                _ => {
                    self.current_char += 1;
                } // Normal space
            }
        }
        Ok(())
    }

    fn next_token(&mut self) -> Result<Token, String> {
        assert_eq!(
            TokenType::Eof as u8 + 1,
            35,
            "Not all TokenTypes are handled in next_token()"
        );
        let c = self.next_char()?;
        let loc = self.get_location();
        match c {
            '0'..='9' => {
                let mut value = String::from(c);
                while let Ok(nc) = self.next_char() {
                    if !nc.is_alphanumeric() {
                        self.current_char -= 1; // Went too far, go a step back
                        break;
                    }
                    value.push(nc);
                }
                Ok(Token {
                    typ: TokenType::IntLiteral,
                    value,
                    loc,
                })
            }
            'A'..='Z' | 'a'..='z' => {
                let mut value = String::from(c);
                while let Ok(nc) = self.next_char() {
                    if !nc.is_alphanumeric() {
                        self.current_char -= 1; // Went too far, go a step back
                        break;
                    }
                    value.push(nc);
                }
                let typ = match value.as_str() {
                    "class" => TokenType::ClassKeyword,
                    "func" => TokenType::FunctionKeyword,
                    "feat" => TokenType::FeatureKeyword,
                    "let" => TokenType::LetKeyword,
                    "if" => TokenType::IfKeyword,
                    "else" => TokenType::ElseKeyword,
                    "return" => TokenType::ReturnKeyword,
                    _ => TokenType::Identifier,
                };
                Ok(Token { typ, value, loc })
            }
            '"' => {
                let mut value = String::new();
                while let Ok(nc) = self.next_char() {
                    if nc == '"' {
                        break;
                    }
                    value.push(nc);
                }
                Ok(Token {
                    typ: TokenType::StrLiteral,
                    value,
                    loc
                })
            }
            '\'' => {
                let mut value = String::new();
                while let Ok(nc) = self.next_char() {
                    if nc == '\'' {
                        break;
                    }
                    value.push(nc);
                }
                if value.len() != 1 {
                    Err(format!("{}: {:?}: Char Literal is expected to be a single char, got `{}`.",
                        ERR_STR,
                        loc,
                        value
                    ))
                } else {
                    Ok(Token {
                        typ: TokenType::CharLiteral,
                        value,
                        loc
                    })
                }
            } 
            '(' => Ok(Token {
                typ: TokenType::OpenRound,
                value: String::from("("),
                loc,
            }),
            ')' => Ok(Token {
                typ: TokenType::ClosingRound,
                value: String::from(")"),
                loc,
            }),
            '{' => Ok(Token {
                typ: TokenType::OpenCurly,
                value: String::from("{"),
                loc,
            }),
            '}' => Ok(Token {
                typ: TokenType::ClosingCurly,
                value: String::from("}"),
                loc,
            }),
            '[' => Ok(Token {
                typ: TokenType::OpenSquare,
                value: String::from("["),
                loc,
            }),
            ']' => Ok(Token {
                typ: TokenType::ClosingSquare,
                value: String::from("]"),
                loc,
            }),
            ';' => Ok(Token {
                typ: TokenType::Semi,
                value: String::from(";"),
                loc,
            }),
            ',' => Ok(Token {
                typ: TokenType::Comma,
                value: String::from(","),
                loc,
            }),
            '!' => {
                if let Ok(nc) = self.next_char() {
                    match nc {
                        '=' => Ok(Token {
                            typ: TokenType::CmpNeq,
                            value: String::from("!="),
                            loc,
                        }),
                        _ => {
                            let mut v = String::from(c);
                            v.push(nc);
                            Err(format!(
                                "{}: {:?}: Unexpected Symbol `{}`",
                                ERR_STR,
                                self.get_location(),
                                v
                            ))
                        }
                    }
                } else {
                    Err(format!("{}: Reached End Of File while lexing.", ERR_STR))
                }
            }
            '=' => {
                let (typ, value) = if let Ok(nc) = self.next_char() {
                    match nc {
                        '=' => (TokenType::CmpEq, String::from("==")),
                        _ => {
                            self.current_char -= 1; // Went too far, go a step back
                            (TokenType::Equal, String::from("="))
                        }
                    }
                } else {
                    (TokenType::Equal, String::from("="))
                };
                Ok(Token { typ, value, loc })
            }
            '<' => {
                let (typ, value) = if let Ok(nc) = self.next_char() {
                    match nc {
                        '=' => (TokenType::CmpLte, String::from("<=")),
                        _ => {
                            self.current_char -= 1; // Went too far, go a step back
                            (TokenType::CmpLt, String::from("<"))
                        }
                    }
                } else {
                    (TokenType::CmpLt, String::from("<"))
                };
                Ok(Token { typ, value, loc })
            }
            '>' => {
                let (typ, value) = if let Ok(nc) = self.next_char() {
                    match nc {
                        '=' => (TokenType::CmpGte, String::from(">=")),
                        _ => {
                            self.current_char -= 1; // Went too far, go a step back
                            (TokenType::CmpGt, String::from(">"))
                        }
                    }
                } else {
                    (TokenType::CmpGt, String::from(">"))
                };
                Ok(Token { typ, value, loc })
            }
            ':' => Ok(Token {
                typ: TokenType::Colon,
                value: String::from(c),
                loc,
            }),
            '.' => Ok(Token {
                typ: TokenType::Dot,
                value: String::from(c),
                loc,
            }),
            '+' => Ok(Token {
                typ: TokenType::Plus,
                value: String::from(c),
                loc,
            }),
            '-' => {
                let (typ, value) = if let Ok(nc) = self.next_char() {
                    match nc {
                        '>' => (TokenType::Arrow, String::from("->")),
                        _ => {
                            self.current_char -= 1; // Went too far, go a step back
                            (TokenType::Minus, String::from("-"))
                        }
                    }
                } else {
                    (TokenType::Minus, String::from("-"))
                };
                Ok(Token { typ, value, loc })
            }
            '*' => Ok(Token {
                typ: TokenType::Asterisk,
                value: String::from(c),
                loc,
            }),
            '/' => {
                let (typ, value) = if let Ok(nc) = self.next_char() {
                    match nc {
                        '/' => {
                            while let Ok(c) = self.next_char() {
                                if c == '\r' || c == '\n' {
                                    self.trim_whitespace();
                                    break;
                                }
                            }
                            return self.next_token();
                        },
                        _ => {
                            self.current_char -= 1; // Went too far, go a step back
                            (TokenType::ForwardSlash, String::from("/"))
                        }
                    }
                } else {
                    (TokenType::ForwardSlash, String::from("/"))
                };
                Ok(Token { typ, value, loc })
            },
            e => Err(format!(
                "{}: {:?}: Unexpected Symbol `{}`",
                ERR_STR,
                self.get_location(),
                e
            )),
        }
    }

    pub fn tokenize(&mut self) -> Result<(), String> {
        while !self.is_eof() {
            self.trim_whitespace()?;
            if self.is_eof() {
                break;
            }
            let t = self.next_token()?;
            self.tokens.push(t);
        }
        Ok(())
    }

    pub fn get_tokens(&self) -> Vec<Token> {
        self.tokens.clone()
    }
}
