use crate::tracer::trace_call;
use crate::internal_panic;

use std::fmt::{Debug, Display, Formatter};

use once_cell::sync::Lazy;
use std::path::PathBuf;
static FILE_ANONYMOUS: usize = 0;
pub static mut FILENAMES: Lazy<Vec<PathBuf>> = Lazy::new(|| {
    vec![PathBuf::from("anonymous")]
});
#[derive(Copy, Clone, PartialEq, Eq, Default)]
pub struct Location {
    pub file_id: usize,
    pub byte: usize,
}

impl Location {
    pub fn add_or_get_filename(file: &PathBuf) -> usize {
        unsafe {
            for (i, f) in FILENAMES.iter().enumerate() {
                if f == file {
                    return i;
                }
            }
            let len = FILENAMES.len();
            FILENAMES.push(file.to_path_buf());
            len
        }
    }
    #[trace_call(extra)]
    pub fn new(file_id: usize, byte: usize) -> Self {
        Self { file_id, byte }
    }

    #[trace_call(extra)]
    pub fn anonymous() -> Self {
        Self::new(FILE_ANONYMOUS, 0)
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}
impl Debug for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let file = unsafe { FILENAMES.get(self.file_id) };
        if file.is_none() {
            internal_panic!("Invalid file_id {}", self.file_id)
        } else {
            let file = file.unwrap();
            write!(f, "{}:{}", file.to_str().unwrap(), self.byte)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token<'src> {
    pub location: Location,
    pub value: &'src str,
    pub token_type: TokenType,
}

impl<'src> Display for Token<'src> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.token_type == TokenType::LiteralString {
            write!(f, "\"{}\"", self.value)
        } else if self.token_type == TokenType::LiteralChar {
            write!(f, "\'{}\'", self.value)
        } else {
            write!(f, "{}", self.value)
        }
    }
}

impl<'src> Token<'src> {
    #[trace_call(extra)]
    pub fn new(location: Location, value: &'src str, token_type: TokenType) -> Self {
        Self {
            location,
            value,
            token_type,
        }
    }

    pub fn get_location(&self) -> Location {
        self.location
    }
}

pub const KEYWORD_BREAK: &str = "break";
pub const KEYWORD_COMPILER_FLAGS: &str = "compiler_flags";
pub const KEYWORD_COMPTIME: &str = "comptime";
pub const KEYWORD_CONTINUE: &str = "continue";
pub const KEYWORD_ELSE: &str = "else";
pub const KEYWORD_EXTERN: &str = "extern";
pub const KEYWORD_FALSE: &str = "false";
pub const KEYWORD_FILEMARKER: &str = "FILEMARKER_INTERNAL_USE_ONLY";
pub const KEYWORD_FOR: &str = "for";
pub const KEYWORD_FUNCTION: &str = "func";
pub const KEYWORD_IF: &str = "if";
pub const KEYWORD_IMPORT: &str = "import";
pub const KEYWORD_LET: &str = "let";
pub const KEYWORD_MUT: &str = "mut";
pub const KEYWORD_RETURN: &str = "return";
pub const KEYWORD_SIZEOF: &str = "sizeof";
pub const KEYWORD_STRUCT: &str = "struct";
pub const KEYWORD_THIS: &str = "this";
pub const KEYWORD_TRUE: &str = "true";
pub const KEYWORD_TYPE: &str = "type"; // Reserved for future use
pub const KEYWORD_UNSAFE: &str = "unsafe";
pub const KEYWORD_WHILE: &str = "while";


// NOTE: When adding a new TokenType, search for TOKEN_TYPE_HANDLE_HERE for places that need to be updated
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    Unknown,
    Comment,
    KeywordBreak,
    KeywordCompilerFlags,
    KeywordComptime,
    KeywordContinue,
    KeywordElse,
    KeywordExtern,
    KeywordFalse,
    KeywordFilemarker,
    KeywordFor,
    KeywordFunc,
    KeywordIf,
    KeywordImport,
    KeywordLet,
    KeywordMut,
    KeywordReturn,
    KeywordSizeof,
    KeywordStruct,
    KeywordThis,
    KeywordTrue,
    KeywordType,
    KeywordUnsafe,
    KeywordWhile,
    LiteralChar,
    LiteralString,
    LiteralInteger,
    Identifier,
    OpenRound,
    ClosingRound,
    OpenCurly,
    ClosingCurly,
    OpenSquare,
    ClosingSquare,
    Colon,
    Ampersand,
    DoubleAmpersand,
    Pipe,
    DoublePipe,
    Semi,
    Comma,
    Dot,
    Exclamation,
    VarArg,
    Arrow,
    Equal,
    Plus,
    Minus,
    Asterisk,
    Percent,
    Caret,
    ForwardSlash,
    CmpEq,
    CmpNeq,
    CmpLt,
    CmpLte,
    CmpGt,
    CmpGte,
    Eof,
}

impl TokenType {
    pub fn try_from_keyword(kw: &str) -> Option<Self> {
        match kw {
            KEYWORD_BREAK => Some(Self::KeywordBreak),
            KEYWORD_COMPILER_FLAGS => Some(Self::KeywordCompilerFlags),
            KEYWORD_COMPTIME => Some(Self::KeywordComptime),
            KEYWORD_CONTINUE => Some(Self::KeywordContinue),
            KEYWORD_ELSE => Some(Self::KeywordElse),
            KEYWORD_EXTERN => Some(Self::KeywordExtern),
            KEYWORD_FALSE => Some(Self::KeywordFalse),
            KEYWORD_FILEMARKER => Some(Self::KeywordFilemarker),
            KEYWORD_FOR => Some(Self::KeywordFor),
            KEYWORD_FUNCTION => Some(Self::KeywordFunc),
            KEYWORD_IF => Some(Self::KeywordIf),
            KEYWORD_IMPORT => Some(Self::KeywordImport),
            KEYWORD_LET => Some(Self::KeywordLet),
            KEYWORD_MUT => Some(Self::KeywordMut),
            KEYWORD_RETURN => Some(Self::KeywordReturn),
            KEYWORD_SIZEOF => Some(Self::KeywordSizeof),
            KEYWORD_STRUCT => Some(Self::KeywordStruct),
            KEYWORD_THIS => Some(Self::KeywordThis),
            KEYWORD_TRUE => Some(Self::KeywordTrue),
            KEYWORD_TYPE => Some(Self::KeywordType),
            KEYWORD_UNSAFE => Some(Self::KeywordUnsafe),
            KEYWORD_WHILE => Some(Self::KeywordWhile),
            _ => None
        }
    }
    pub fn is_opening_bracket(&self) -> bool {
        match self {
            Self::OpenRound | Self::OpenCurly => true,
            _ => false,
        }
    }
    pub fn is_closing_bracket(&self) -> bool {
        match self {
            Self::ClosingRound | Self::ClosingCurly => true,
            _ => false,
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // TOKEN_TYPE_HANDLE_HERE
        match self {
            Self::Unknown => write!(f, "Unknown Token"),
            Self::Comment => internal_panic!("TokenType::Comment is for internal use"),
            Self::LiteralChar => write!(f, "Char Literal"),
            Self::LiteralString => write!(f, "String Literal"),
            Self::LiteralInteger => write!(f, "Integer Literal"),
            Self::Identifier => write!(f, "Identifier"),
            Self::OpenRound => write!(f, "`(`"),
            Self::ClosingRound => write!(f, "`)`"),
            Self::OpenCurly => write!(f, "`{{`"),
            Self::ClosingCurly => write!(f, "`}}`"),
            Self::OpenSquare => write!(f, "`[`"),
            Self::ClosingSquare => write!(f, "`]`"),
            Self::KeywordBreak => write!(f, "`{}`", KEYWORD_BREAK),
            Self::KeywordCompilerFlags => write!(f, "`{}`", KEYWORD_COMPILER_FLAGS),
            Self::KeywordComptime => write!(f, "`{}`", KEYWORD_COMPTIME),
            Self::KeywordContinue => write!(f, "`{}`", KEYWORD_CONTINUE),
            Self::KeywordElse => write!(f, "`{}`", KEYWORD_ELSE),
            Self::KeywordExtern => write!(f, "`{}`", KEYWORD_EXTERN),
            Self::KeywordFalse => write!(f, "`{}`", KEYWORD_FALSE),
            Self::KeywordFilemarker => write!(f, "`{}`", KEYWORD_FILEMARKER),
            Self::KeywordFor => write!(f, "`{}`", KEYWORD_FOR),
            Self::KeywordFunc => write!(f, "`{}`", KEYWORD_FUNCTION),
            Self::KeywordIf => write!(f, "`{}`", KEYWORD_IF),
            Self::KeywordImport => write!(f, "`{}`", KEYWORD_IMPORT),
            Self::KeywordLet => write!(f, "`{}`", KEYWORD_LET),
            Self::KeywordMut => write!(f, "`{}`", KEYWORD_MUT),
            Self::KeywordReturn => write!(f, "`{}`", KEYWORD_RETURN),
            Self::KeywordSizeof => write!(f, "`{}`", KEYWORD_SIZEOF),
            Self::KeywordStruct => write!(f, "`{}`", KEYWORD_STRUCT),
            Self::KeywordTrue => write!(f, "`{}`", KEYWORD_TRUE),
            Self::KeywordThis => write!(f, "`{}`", KEYWORD_THIS),
            Self::KeywordType => write!(f, "`{}`", KEYWORD_TYPE),
            Self::KeywordUnsafe => write!(f, "`{}`", KEYWORD_UNSAFE),
            Self::KeywordWhile => write!(f, "`{}`", KEYWORD_WHILE),
            Self::Colon => write!(f, "`:`"),
            Self::Ampersand => write!(f, "`&`"),
            Self::DoubleAmpersand => write!(f, "`&&`"),
            Self::Pipe => write!(f, "`|`"),
            Self::DoublePipe => write!(f, "`||`"),
            Self::Semi => write!(f, "`;`"),
            Self::Comma => write!(f, "`,`"),
            Self::Dot => write!(f, "`.`"),
            Self::Exclamation => write!(f, "`!`"),
            Self::VarArg => write!(f, "`...`"),
            Self::Arrow => write!(f, "`->`"),
            Self::Equal => write!(f, "`=`"),
            Self::Plus => write!(f, "`+`"),
            Self::Minus => write!(f, "`-`"),
            Self::Asterisk => write!(f, "`*`"),
            Self::ForwardSlash => write!(f, "`/`"),
            Self::Percent => write!(f, "`%`"),
            Self::Caret => write!(f, "`^`"),
            Self::CmpEq => write!(f, "`==`"),
            Self::CmpNeq => write!(f, "`!=`"),
            Self::CmpLt => write!(f, "`<`"),
            Self::CmpLte => write!(f, "`<=`"),
            Self::CmpGt => write!(f, "`>`"),
            Self::CmpGte => write!(f, "`>=`"),
            Self::Eof => write!(f, "End of File"),
        }
    }
}
