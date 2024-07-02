use crate::frontend::tokens::{Location, Token, TokenType};

pub struct Lexer<'src> {
    content: &'src str,
    ptr: usize,
    len: usize,
    whitespace_offset: usize,
}

impl<'src> Lexer<'src> {
    pub fn new() -> Self {
        Self {
            content: "",
            ptr: 0,
            len: 0,
            whitespace_offset: 0
        }
    }

    pub fn get_location(&self) -> Location {
        Location::new(0, self.ptr)
    }

    pub fn load(&mut self, content: &'src str) {
        self.content = content;
        self.ptr = 0;
        self.len = self.content.len();
        self.whitespace_offset = 0;
    }

    pub fn peek(&mut self) -> Option<Token<'src>> {
        let save = self.ptr;
        let orig = self.content;
        let ws = self.whitespace_offset;
        let t = self.next();
        self.ptr = save;
        self.content = orig;
        self.whitespace_offset = ws;
        t
    }

    pub fn next_with_whitespace(&mut self) -> (usize, Option<Token<'src>>) {
        let t = self.__next(true);
        let ws = self.whitespace_offset;
        self.whitespace_offset = 0;
        (ws, t)
    }

    pub fn next(&mut self) -> Option<Token<'src>> {
        let tkn = self.__next(false);
        tkn
    }
    
    fn __next(&mut self, comment_token: bool) -> Option<Token<'src>> {
        while self.content.starts_with(char::is_whitespace) {
            self.whitespace_offset += 1;
            self.ptr += 1;
            self.content = &self.content[1..];
            if self.ptr >= self.len {
                return None;
            }
        }
        if self.ptr >= self.len {
            return None;
        }
        let loc = self.get_location();
        if self.content.starts_with(|c: char| c == '_' || c.is_alphabetic()) {
            let mut len = 0;
            let mut tmp = self.content;
            while tmp.starts_with(|c: char| c == '_' || c.is_alphanumeric()) {
                len += 1;
                tmp = &tmp[1..];
            }
            let word = &self.content[0..len];
            self.ptr += len;
            self.content = tmp;
            if let Some(kw) = TokenType::try_from_keyword(&word) {
                // FIXME: Would be cooler if we only had TokenType::Keyword
                //        The Token knows its kind
                Some(Token::new(loc, word, kw))
            } else {
                Some(Token::new(loc, word, TokenType::Identifier))
            }
        } else if self.content.starts_with(char::is_numeric) {
            let mut len = 0;
            let mut tmp = self.content;
            while tmp.starts_with(char::is_alphanumeric) {
                len += 1;
                tmp = &tmp[1..];
            }
            let word = &self.content[0..len];
            self.ptr += len;
            self.content = tmp;
            Some(Token::new(loc, word, TokenType::LiteralInteger))
        } else if self.content.starts_with('"') {
            let mut len = 0;
            let mut tmp = &self.content[1..];
            while !tmp.starts_with('"') {
                len += 1;
                tmp = &tmp[1..];
            }
            let word = &self.content[1..(len+1)];
            self.ptr += len + 2;
            self.content = &tmp[1..];
            Some(Token::new(loc, word, TokenType::LiteralString))
        } else if self.content.starts_with('\'') {
            let mut len = 0;
            let mut tmp = &self.content[1..];
            while !tmp.starts_with('\'') {
                len += 1;
                tmp = &tmp[1..];
            }
            let word = &self.content[1..(len+1)];
            self.ptr += len + 2;
            self.content = &tmp[1..];
            Some(Token::new(loc, word, TokenType::LiteralChar))
        } else if self.content.starts_with("//") {
            let mut len = 0;
            let mut tmp = &self.content[0..];
            while !tmp.is_empty() && !tmp.starts_with("\n") {
                len += 1;
                tmp = &tmp[1..];
            }
            if !tmp.is_empty() {
                len += 1;
                tmp = &tmp[1..];
            }
            let comment = &self.content[0..len];
            self.ptr += len;
            self.content = tmp;
            if comment_token {
                Some(Token::new(loc, comment, TokenType::Comment))
            } else {
                self.next()
            }
        } else if self.content.starts_with("/*") {
            let mut len = 0;
            let mut tmp = &self.content[0..];
            while !tmp.starts_with("*/") {
                len += 1;
                tmp = &tmp[1..];
            }
            len += 2;
            tmp = &tmp[2..];
            let comment = &self.content[0..len];
            self.ptr += len;
            self.content = tmp;
            if comment_token {
                Some(Token::new(loc, comment, TokenType::Comment))
            } else {
                self.next()
            }
        } else {
            macro_rules! enumerate_tokens {
                ([ $op:literal $kind:ident ]$(,)*) => {
                    if self.content.starts_with($op) {
                        let _len = $op.len();
                        self.ptr += _len;
                        self.content = &self.content[_len..];
                        return Some(Token::new(loc, $op, TokenType::$kind));
                    }
                };
                ([ $op:literal $kind:ident ], $($rest:tt)*) => {
                    if self.content.starts_with($op) {
                        let _len = $op.len();
                        self.ptr += _len;
                        self.content = &self.content[_len..];
                        return Some(Token::new(loc, $op, TokenType::$kind));
                    }
                    enumerate_tokens!($($rest)*);
                };
            }
            enumerate_tokens!(
                ["(" OpenRound],
                [")" ClosingRound],
                ["{" OpenCurly],
                ["}" ClosingCurly],
                ["[" OpenSquare],
                ["]" ClosingSquare],
                [":" Colon],
                [";" Semi],
                ["," Comma],
                ["->" Arrow],
                ["-" Minus],
                ["+" Plus],
                ["*" Asterisk],
                ["/" ForwardSlash],
                ["%" Percent],
                ["&&" DoubleAmpersand],
                ["&" Ampersand],
                ["..." VarArg],
                ["." Dot],
                ["!=" CmpNeq],
                ["==" CmpEq],
                ["<=" CmpLte],
                [">=" CmpGte],
                ["!" Exclamation],
                ["=" Equal],
                ["<" CmpLt],
                [">" CmpGt],
                ["^" Caret],
                ["||" DoublePipe],
                ["|" Pipe],
                ["\0" Eof],
            );
            let mut len = 0;
            let mut tmp = self.content;
            while !tmp.starts_with(char::is_whitespace) {
                len += 1;
                tmp = &tmp[1..];
            }
            let word = &self.content[0..len];
            self.ptr += len;
            self.content = tmp;
            Some(Token::new(loc, word, TokenType::Unknown))
        }
    }
}