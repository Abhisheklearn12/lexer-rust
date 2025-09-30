
// - Tried to build my own lexer in Rust from scratch.
// - Single-file
// - Clear comments and span/position tracking
// - Handles identifiers, keywords, integers, floats, string literals with escapes,
//   line comments (//...), block comments (/* ... */), punctuation, and two-character operators.
// - Returns precise errors with location spans.
//
// I learnt to build lexer in C, then transfer my knowledge in Rust.
// Written to be readable and educational -> adapt tokens/keywords to your language spec.

use std::fmt;

/// Position in the source: 1-based line and column
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
}

impl Pos {
    pub fn new(line: usize, col: usize) -> Self {
        Pos { line, col }
    }
}

/// Span covering an inclusive start and exclusive end position (start..end)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub fn new(start: Pos, end: Pos) -> Self {
        Span { start, end }
    }
}

/// Token kinds recognized by the lexer.
/// Extend this enum with new token kinds as your language requires.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Structural / punctuation
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Dot,
    Semicolon,
    Colon,
    Arrow, // "->"

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Bang,      // !
    Assign,    // =
    Eq,        // ==
    Ne,        // !=
    Lt,        // <
    Gt,        // >
    Le,        // <=
    Ge,        // >=
    And,       // &&
    Or,        // ||
    PlusEq,    // +=
    MinusEq,   // -=
    StarEq,    // *=
    SlashEq,   // /=
    // ... add more as needed

    // Literals & identifiers
    Identifier(String),
    Keyword(String),
    IntLiteral(String),
    FloatLiteral(String),
    StringLiteral(String),

    // Special
    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;
        match self {
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            LBracket => write!(f, "["),
            RBracket => write!(f, "]"),
            Comma => write!(f, ","),
            Dot => write!(f, "."),
            Semicolon => write!(f, ";"),
            Colon => write!(f, ":"),
            Arrow => write!(f, "->"),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Star => write!(f, "*"),
            Slash => write!(f, "/"),
            Percent => write!(f, "%"),
            Caret => write!(f, "^"),
            Bang => write!(f, "!"),
            Assign => write!(f, "="),
            Eq => write!(f, "=="),
            Ne => write!(f, "!="),
            Lt => write!(f, "<"),
            Gt => write!(f, ">"),
            Le => write!(f, "<="),
            Ge => write!(f, ">="),
            And => write!(f, "&&"),
            Or => write!(f, "||"),
            PlusEq => write!(f, "+="),
            MinusEq => write!(f, "-="),
            StarEq => write!(f, "*="),
            SlashEq => write!(f, "/="),
            Identifier(s) => write!(f, "Identifier({})", s),
            Keyword(s) => write!(f, "Keyword({})", s),
            IntLiteral(s) => write!(f, "Int({})", s),
            FloatLiteral(s) => write!(f, "Float({})", s),
            StringLiteral(s) => write!(f, "String({})", s),
            Eof => write!(f, "<EOF>"),
        }
    }
}

/// A token: kind + span in the source text
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }
}

/// Lexer error: message + span where it occurred
#[derive(Debug, Clone)]
pub struct LexError {
    pub msg: String,
    pub span: Span,
}

impl LexError {
    pub fn new<S: Into<String>>(msg: S, span: Span) -> Self {
        LexError {
            msg: msg.into(),
            span,
        }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = &self.span;
        write!(
            f,
            "LexError at line {}, col {}: {}",
            s.start.line, s.start.col, self.msg
        )
    }
}

impl std::error::Error for LexError {}

/// The Lexer struct: holds the source, current index, current char, and position info.
pub struct Lexer {
    src: Vec<char>, // input as vector of chars for easy indexing
    idx: usize,     // current index in src (0..=src.len())
    cur_line: usize,
    cur_col: usize,
}

impl Lexer {
    /// Create a lexer from a source string.
    pub fn new(input: &str) -> Self {
        let src: Vec<char> = input.chars().collect();
        Lexer {
            src,
            idx: 0,
            cur_line: 1,
            cur_col: 1,
        }
    }

    /// peek current character (None if EOF)
    fn peek(&self) -> Option<char> {
        self.src.get(self.idx).copied()
    }

    /// peek the next character (lookahead of 1)
    fn peek_next(&self) -> Option<char> {
        self.src.get(self.idx + 1).copied()
    }

    /// advance and return the consumed character (None if EOF)
    fn bump(&mut self) -> Option<char> {
        let ch = self.src.get(self.idx).copied();
        if let Some(c) = ch {
            self.idx += 1;
            if c == '\n' {
                self.cur_line += 1;
                self.cur_col = 1;
            } else {
                self.cur_col += 1;
            }
        }
        ch
    }

    /// create a Pos for current location (position of next char)
    fn cur_pos(&self) -> Pos {
        Pos::new(self.cur_line, self.cur_col)
    }

    /// helper: consume while predicate is true, returning the consumed string
    fn take_while<F>(&mut self, mut f: F) -> String
    where
        F: FnMut(char) -> bool,
    {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if f(c) {
                s.push(c);
                self.bump();
            } else {
                break;
            }
        }
        s
    }

    /// skip whitespace (spaces, tabs, newlines) and comments.
    /// returns Ok(()) or a LexError if a block comment isn't closed.
        /// skip whitespace (spaces, tabs, newlines) and comments.
    /// returns Ok(()) or a LexError if a block comment isn't closed.
    fn skip_whitespace_and_comments(&mut self) -> Result<(), LexError> {
        loop {
            let mut progressed = false;

            // skip whitespace
            while let Some(c) = self.peek() {
                if c == ' ' || c == '\t' || c == '\r' || c == '\n' {
                    progressed = true;
                    self.bump();
                } else {
                    break;
                }
            }

            // comments: // line comment
            if self.peek() == Some('/') && self.peek_next() == Some('/') {
                // consume '//' and then until newline or EOF
                self.bump();
                self.bump();
                while let Some(c) = self.peek() {
                    if c == '\n' {
                        break;
                    }
                    self.bump();
                }
                continue; // loop again
            }

            // block comment: /* ... */
            if self.peek() == Some('/') && self.peek_next() == Some('*') {
                // consume '/*'
                self.bump();
                self.bump();
                // find closing '*/'
                loop {
                    match self.peek() {
                        Some('*') if self.peek_next() == Some('/') => {
                            self.bump(); // '*'
                            self.bump(); // '/'
                            break;
                        }
                        Some(_) => {
                            self.bump();
                        }
                        None => {
                            // unclosed block comment
                            let pos = self.cur_pos();
                            return Err(LexError::new(
                                "Unterminated block comment",
                                Span::new(pos, pos),
                            ));
                        }
                    }
                }
                continue; // loop again
            }

            // if nothing was consumed, break
            if !progressed {
                break;
            }
        }
        Ok(())
    }

    /// Main function: produce the next token.
    /// Returns Token or LexError.
    pub fn next_token(&mut self) -> Result<Token, LexError> {
        // Skip whitespace and comments before lexing a meaningful token.
        self.skip_whitespace_and_comments()?;

        // starting span position
        let start_pos = self.cur_pos();

        // end_pos will be updated; for simple single-char tokens it's start->after char.
        macro_rules! make_span {
            ($start:expr) => {{
                let end = self.cur_pos();
                Span::new($start, end)
            }};
        }

        // If EOF, return Eof token
        if self.peek().is_none() {
            let sp = Span::new(start_pos, start_pos);
            return Ok(Token::new(TokenKind::Eof, sp));
        }

        let c = self.peek().unwrap();

        // Identifiers or keywords: [a-zA-Z_][a-zA-Z0-9_]*
        if is_ident_start(c) {
            let mut ident = String::new();
            ident.push(c);
            self.bump();
            ident.push_str(&self.take_while(is_ident_continue));
            let end_span = make_span!(start_pos);
            // Keywords set — adjust to your language.
            if is_keyword(&ident) {
                return Ok(Token::new(TokenKind::Keyword(ident), end_span));
            }
            return Ok(Token::new(TokenKind::Identifier(ident), end_span));
        }

        // Number: integer or float with optional exponent
        if c.is_ascii_digit() {
            // We'll construct the literal as characters and choose Int or Float.
            let mut s = String::new();
            // integer part
            s.push(c);
            self.bump();
            s.push_str(&self.take_while(|ch| ch.is_ascii_digit()));

            // fraction: .digits
            let is_float = if self.peek() == Some('.') && self.peek_next().map(|d| d.is_ascii_digit()).unwrap_or(false) {
                // consume '.' and following digits
                s.push('.');
                self.bump(); // '.'
                let frac = self.take_while(|ch| ch.is_ascii_digit());
                if frac.is_empty() {
                    // This should not happen because we checked peek_next
                } else {
                    s.push_str(&frac);
                }
                true
            } else {
                false
            };

            // exponent: [eE][+-]?digits
            let mut is_float = is_float;
            if let Some('e') | Some('E') = self.peek() {
                // consume 'e' or 'E'
                s.push(self.peek().unwrap());
                self.bump();
                // optional + or -
                if let Some('+') | Some('-') = self.peek() {
                    s.push(self.peek().unwrap());
                    self.bump();
                }
                // digits (require at least one)
                let digits = self.take_while(|ch| ch.is_ascii_digit());
                if digits.is_empty() {
                    let sp = make_span!(start_pos);
                    return Err(LexError::new("Invalid float literal: exponent has no digits", sp));
                }
                s.push_str(&digits);
                is_float = true;
            }

            let end_span = make_span!(start_pos);
            if is_float {
                return Ok(Token::new(TokenKind::FloatLiteral(s), end_span));
            } else {
                return Ok(Token::new(TokenKind::IntLiteral(s), end_span));
            }
        }

        // String literal starting with double quote " ... "
        if c == '"' {
            // consume opening quote
            self.bump();
            let mut value = String::new();
            loop {
                match self.peek() {
                    Some('"') => {
                        // end of string
                        self.bump();
                        break;
                    }
                    Some('\\') => {
                        // escape sequence
                        self.bump(); // consume '\'
                        match self.peek() {
                            Some('n') => {
                                value.push('\n');
                                self.bump();
                            }
                            Some('r') => {
                                value.push('\r');
                                self.bump();
                            }
                            Some('t') => {
                                value.push('\t');
                                self.bump();
                            }
                            Some('\\') => {
                                value.push('\\');
                                self.bump();
                            }
                            Some('"') => {
                                value.push('"');
                                self.bump();
                            }
                            Some('0') => {
                                value.push('\0');
                                self.bump();
                            }
                            Some(c2) => {
                                // unknown/unsupported escape — we still accept it as char after backslash
                                value.push(c2);
                                self.bump();
                            }
                            None => {
                                let pos = self.cur_pos();
                                let sp = Span::new(start_pos, pos);
                                return Err(LexError::new("Unterminated escape in string", sp));
                            }
                        }
                    }
                    Some(ch) => {
                        // normal char
                        value.push(ch);
                        self.bump();
                    }
                    None => {
                        // EOF inside string — error
                        let pos = self.cur_pos();
                        let sp = Span::new(start_pos, pos);
                        return Err(LexError::new("Unterminated string literal", sp));
                    }
                }
            }
            let sp = make_span!(start_pos);
            return Ok(Token::new(TokenKind::StringLiteral(value), sp));
        }

        // Two-character operators / tokens: check lookahead first
        // handle '==', '!=', '<=', '>=', '&&', '||', '+=', '-=', '*=', '/=', '->'
        if let Some(next) = self.peek_next() {
            match (c, next) {
                ('=', '=') => {
                    self.bump();
                    self.bump();
                    return Ok(Token::new(TokenKind::Eq, make_span!(start_pos)));
                }
                ('!', '=') => {
                    self.bump();
                    self.bump();
                    return Ok(Token::new(TokenKind::Ne, make_span!(start_pos)));
                }
                ('<', '=') => {
                    self.bump();
                    self.bump();
                    return Ok(Token::new(TokenKind::Le, make_span!(start_pos)));
                }
                ('>', '=') => {
                    self.bump();
                    self.bump();
                    return Ok(Token::new(TokenKind::Ge, make_span!(start_pos)));
                }
                ('&', '&') => {
                    self.bump();
                    self.bump();
                    return Ok(Token::new(TokenKind::And, make_span!(start_pos)));
                }
                ('|', '|') => {
                    self.bump();
                    self.bump();
                    return Ok(Token::new(TokenKind::Or, make_span!(start_pos)));
                }
                ('+', '=') => {
                    self.bump();
                    self.bump();
                    return Ok(Token::new(TokenKind::PlusEq, make_span!(start_pos)));
                }
                ('-', '=') => {
                    self.bump();
                    self.bump();
                    return Ok(Token::new(TokenKind::MinusEq, make_span!(start_pos)));
                }
                ('*', '=') => {
                    self.bump();
                    self.bump();
                    return Ok(Token::new(TokenKind::StarEq, make_span!(start_pos)));
                }
                ('/', '=') => {
                    self.bump();
                    self.bump();
                    return Ok(Token::new(TokenKind::SlashEq, make_span!(start_pos)));
                }
                ('-', '>') => {
                    self.bump();
                    self.bump();
                    return Ok(Token::new(TokenKind::Arrow, make_span!(start_pos)));
                }
                _ => {}
            }
        }

        // Single-character tokens and simple operators
        match c {
            '(' => {
                self.bump();
                return Ok(Token::new(TokenKind::LParen, make_span!(start_pos)));
            }
            ')' => {
                self.bump();
                return Ok(Token::new(TokenKind::RParen, make_span!(start_pos)));
            }
            '{' => {
                self.bump();
                return Ok(Token::new(TokenKind::LBrace, make_span!(start_pos)));
            }
            '}' => {
                self.bump();
                return Ok(Token::new(TokenKind::RBrace, make_span!(start_pos)));
            }
            '[' => {
                self.bump();
                return Ok(Token::new(TokenKind::LBracket, make_span!(start_pos)));
            }
            ']' => {
                self.bump();
                return Ok(Token::new(TokenKind::RBracket, make_span!(start_pos)));
            }
            ',' => {
                self.bump();
                return Ok(Token::new(TokenKind::Comma, make_span!(start_pos)));
            }
            '.' => {
                // Note: '.' might start a float when preceding digits in some languages.
                // Here we treat '.' as punctuation unless earlier code handles numbers that start with digits.
                self.bump();
                return Ok(Token::new(TokenKind::Dot, make_span!(start_pos)));
            }
            ';' => {
                self.bump();
                return Ok(Token::new(TokenKind::Semicolon, make_span!(start_pos)));
            }
            ':' => {
                self.bump();
                return Ok(Token::new(TokenKind::Colon, make_span!(start_pos)));
            }
            '+' => {
                self.bump();
                return Ok(Token::new(TokenKind::Plus, make_span!(start_pos)));
            }
            '-' => {
                self.bump();
                return Ok(Token::new(TokenKind::Minus, make_span!(start_pos)));
            }
            '*' => {
                self.bump();
                return Ok(Token::new(TokenKind::Star, make_span!(start_pos)));
            }
            '/' => {
                self.bump();
                return Ok(Token::new(TokenKind::Slash, make_span!(start_pos)));
            }
            '%' => {
                self.bump();
                return Ok(Token::new(TokenKind::Percent, make_span!(start_pos)));
            }
            '^' => {
                self.bump();
                return Ok(Token::new(TokenKind::Caret, make_span!(start_pos)));
            }
            '!' => {
                self.bump();
                return Ok(Token::new(TokenKind::Bang, make_span!(start_pos)));
            }
            '=' => {
                self.bump();
                return Ok(Token::new(TokenKind::Assign, make_span!(start_pos)));
            }
            '<' => {
                self.bump();
                return Ok(Token::new(TokenKind::Lt, make_span!(start_pos)));
            }
            '>' => {
                self.bump();
                return Ok(Token::new(TokenKind::Gt, make_span!(start_pos)));
            }
            _ => {
                // Unknown/unexpected character
                let pos = self.cur_pos();
                let sp = Span::new(start_pos, pos);
                let msg = format!("Unexpected character '{}'", c);
                return Err(LexError::new(msg, sp));
            }
        }
    }

    /// Convenience: collect all tokens until EOF or error.
    pub fn tokenize_all(&mut self) -> Result<Vec<Token>, LexError> {
        let mut out = Vec::new();
        loop {
            let tok = self.next_token()?;
            if tok.kind == TokenKind::Eof {
                out.push(tok);
                break;
            } else {
                out.push(tok);
            }
        }
        Ok(out)
    }
}

/// helper: whether char can start an identifier
fn is_ident_start(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic()
}

/// helper: whether char can continue an identifier
fn is_ident_continue(c: char) -> bool {
    c == '_' || c.is_ascii_alphanumeric()
}

/// set of keywords — adjust depending on language needs
fn is_keyword(ident: &str) -> bool {
    // Example small set
    matches!(
        ident,
        "fn" | "let" | "mut" | "if" | "else" | "while" | "for" | "return" | "true" | "false"
    )
}

/// --- Demo / quick tests ---
fn main() {
    let source = r#"
    // Demo program
    fn add(a: i32, b: i32) -> i32 {
        let sum = a + b;
        return sum;
    }

    /* block comment
       spanning lines
    */

    let x = 123;
    let y = 3.14;
    let s = "hello\nworld \"quoted\"";
    if x > 10 && y < 4.0 {
        x += 1;
    }
    "#;

    let mut lexer = Lexer::new(source);

    match lexer.tokenize_all() {
        Ok(tokens) => {
            for t in tokens {
                match &t.kind {
                    TokenKind::Identifier(_) |
                    TokenKind::Keyword(_) |
                    TokenKind::IntLiteral(_) |
                    TokenKind::FloatLiteral(_) |
                    TokenKind::StringLiteral(_) => {
                        println!("{:>4}:{:>3}-{:>3}:{:>3}  {:<20}  {:?}", t.span.start.line, t.span.start.col, t.span.end.line, t.span.end.col, format!("{}", t.kind), t.kind);
                    }
                    other => {
                        println!("{:>4}:{:>3}-{:>3}:{:>3}  {:<20}", t.span.start.line, t.span.start.col, t.span.end.line, t.span.end.col, format!("{}", other));
                    }
                }
            }
        }
        Err(e) => {
            eprintln!("Lexing failed: {}", e);
        }
    }
}

// In the future, my goal is to turn this into full fledge compiler, this project is just for learning purpose, it's super fun btw.
