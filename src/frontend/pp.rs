use crate::compiler::{FILE_EXT, WARN_STR};
use crate::frontend::tokens::{Token, TokenType, KEYWORD_FILEMARKER};
use crate::internal_panic;
use crate::util::flags::Flags;
use crate::frontend::lexer::Lexer;
use std::collections::HashSet;
use std::fmt::Display;
use std::fs;
use std::path::PathBuf;

fn path_buf_to_str(path: &PathBuf) -> &str {
    path.to_str().unwrap()
}
enum PpError<'src> {
    FileNotFoundInImportPaths(String),
    OtherErrors(String),
    UnexpectedEOF,
    UnexpectedToken(TokenType, Token<'src>),
}

impl Display for PpError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FileNotFoundInImportPaths(filepath) => write!(f, "error: File {filepath} was not found in the list of import paths."), 
            Self::OtherErrors(_other) => todo!(),
            Self::UnexpectedEOF => write!(f, "error: Expected String literal after `import`, found End Of File."),
            Self::UnexpectedToken(_expected, _received) => todo!(),
        }
    }
}

pub struct Preprocessor<'flags, 'lexer, 'src> {
    flags: &'flags Flags,
    lexer: &'lexer mut Lexer<'src>,
    import_paths: Vec<PathBuf>,
}

impl<'flags, 'lexer, 'src> Preprocessor<'flags, 'lexer, 'src> {
    pub fn new(flags: &'flags Flags, lexer: &'lexer mut Lexer<'src>) -> Self {
        Self {
            flags,
            lexer,
            import_paths: Vec::new()
        }
    }

    fn process(&mut self, imported_files: &mut HashSet<PathBuf>, origin: &PathBuf, content: &'src str) -> Result<String, Vec<PpError>> {
        let mut after = String::with_capacity(content.len());
        self.lexer.load(content);
        while let (ws, Some(t)) = self.lexer.next_with_whitespace() {
            if self.flags.debug {
                println!("[DEBUG] Whitespace: {}", ws);
                println!("[DEBUG] Preproc::next() -> {:?}", t);
            }
            let Token { token_type: TokenType::KeywordImport, .. } = t else {
                after.push_str(&" ".repeat(ws));
                after.push_str(&t.to_string());
                continue;
            };
            let Some(path) = self.lexer.next() else {
                return Err(vec![PpError::UnexpectedEOF]);
            };
            let Token { value: filename, token_type: TokenType::LiteralString, .. } = path else {
                return Err(vec![PpError::UnexpectedToken(TokenType::LiteralString, path)]);
            };
            let mut filename = filename.to_string();
            let Some(semi) = self.lexer.next() else {
                return Err(vec![PpError::UnexpectedEOF]);
            };
            let Token { token_type: TokenType::Semi, .. } = semi else {
                return Err(vec![PpError::UnexpectedToken(TokenType::Semi, semi)]);
            };
            let mut valid = false;
            for path in &self.import_paths {
                if !filename.ends_with(FILE_EXT) {
                    filename += FILE_EXT;
                }
                let mut filepath = path.clone();
                filepath.push(&filename);
                let filepath = PathBuf::from(filepath);
                if fs::metadata(&filepath).is_ok() {
                    let Ok(import_data) = fs::read_to_string(&filepath) else {
                        let _s = path_buf_to_str(&filepath);
                        internal_panic!("fs::metadata(&{_s}).is_ok() == true, yet reading failed")
                    };
                    let import = pre_process(origin, &self.flags, imported_files, &self.import_paths, &filepath, &import_data)
                        .map_err(|vpe|vec![PpError::OtherErrors(vpe)])?;
                    let fname = filepath.to_str().unwrap();
                    after.push_str(&format!("{KEYWORD_FILEMARKER} START \"{fname}\""));
                    after.push_str(&import);
                    after.push_str(&format!("{KEYWORD_FILEMARKER} END \"{fname}\""));
                    after.push(' ');
                    valid = true;
                    break;
                }
            }
            if !valid {
                return Err(vec![PpError::FileNotFoundInImportPaths(filename)]);
            }
        }
        Ok(after)
    }

    fn add_import(&mut self, path: PathBuf) {
        self.import_paths.push(path);
    }
}

fn pre_process(
    origin: &PathBuf,
    flags: &Flags,
    imported_files: &mut HashSet<PathBuf>,
    imports: &Vec<PathBuf>,
    path: &PathBuf,
    content: &str
) -> Result<String, String> {
    if imported_files.contains(path) {
        println!("{WARN_STR}: When processing {0}: File {1} is already imported.",
            path_buf_to_str(origin),
            path_buf_to_str(path));
        return Ok(String::new());
    }
    imported_files.insert(path.to_path_buf());
    if flags.verbose {
        println!("[INFO] Reading file {0}", path_buf_to_str(path))
    }
    let mut lexer = Lexer::new();
    let mut pp = Preprocessor::new(flags, &mut lexer);
    for path in imports {
        pp.add_import(path.to_path_buf());
    }
    let tmp = pp.process(imported_files, &path, &content);
    tmp.map_err(|vpe|vpe.iter().map(|e|e.to_string()).collect::<Vec<_>>().join("\n"))
}

pub fn load_project(flags: &Flags) -> Result<String, String> {
    let mut imports = Vec::with_capacity(flags.imports.len());
    for i in &flags.imports {
        imports.push(PathBuf::from(i));
    }
    imports.push(PathBuf::from("./prelude/"));
    if let Some(parent) = flags.input.parent() {
        imports.push(parent.to_path_buf());
    }
    match fs::read_to_string(&flags.input) {
        Ok(content) => {
            let content = format!("import \"prelude.bufo\";{KEYWORD_FILEMARKER} START \"{0}\"{content} {KEYWORD_FILEMARKER} END \"{0}\"", flags.input.to_str().unwrap());
            pre_process(&flags.input, flags, &mut HashSet::new(), &imports, &flags.input, &content)
        },
        Err(_e) => todo!()
    }
}