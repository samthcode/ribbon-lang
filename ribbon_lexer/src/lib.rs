use std::iter::Peekable;
use std::str::Chars;

#[macro_use]
pub mod tok;
pub use tok::{LitKind, OpKind, Tok, TokKind};

pub mod span;

pub struct Lexer<'a> {
    cursor: Cursor<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            cursor: Cursor::new(source),
        }
    }

    /// Returns the next lexed token
    /// The Lexer is lazy so it only lexes one token at a time as needed
    fn next_token(&mut self) -> Option<Tok> {
        let res = if let Some(c) = self.curr_char() {
            match c {
                // Whitespace
                c if is_whitespace(&c) => {
                    self.next_char();
                    return self.next_token()
                }
                '"' => Some(self.tok_str()),
                // Identifiers and keywords
                c if is_ident_head(&c) => Some(self.tok_ident()),
                // Operators
                c if is_op_head(&c) => Some(self.tok_op()),
                _ => todo!(),
            }
        } else {
            None
        };
        self.next_char();
        res
    }

    /// Advances the source character cursor
    fn next_char(&mut self) -> Option<char> {
        self.cursor.next()
    }

    /// Peeks the cursor to the source characters
    fn peek_char(&mut self) -> Option<&char> {
        self.cursor.peek()
    }

    /// Returns the current character under the cursor
    fn curr_char(&self) -> Option<char> {
        self.cursor.curr
    }

    /// Creates an identifier or keyword token
    fn tok_ident(&mut self) -> Tok {
        let mut res = self.curr_char().unwrap().to_string();
        let start = self.cursor.pos;
        while let Some(c) = self.peek_char() {
            if is_ident_tail(c) {
                res.push(*c);
            } else {
                // `?` can only appear at the end of a string so this must be the end
                if *c == '?' {
                    res.push(*c);
                    self.next_char();
                };
                break;
            }
            self.next_char();
        }
        tok!(@maybe_conv Ident(res), start, self.cursor.pos)
    }

    fn tok_str(&mut self) -> Tok {
        assert!(
            matches!(self.curr_char(), Some('"')),
            "Called tok_str while cursor not over double quote"
        );
        let mut res = String::new();
        let start = self.cursor.pos;
        while let Some(c) = self.next_char() {
            match c {
                '"' => return tok!(Lit(Str(res)), start, self.cursor.pos),
                '\\' => match self.next_char() {
                    Some('\\') => res.push('\\'),
                    Some('"') => res.push('"'),
                    Some('n') => res.push('\n'),
                    Some('r') => res.push('\r'),
                    Some('t') => res.push('\t'),
                    Some('0') => res.push('\0'),
                    None => {
                        // Note that the first `\` of the escape isn't appended to the string
                        // This may be changed in the future depending on what we need
                        return tok!(
                            Lit(InvalidStr(res, UnterminatedEscape)),
                            start,
                            self.cursor.pos
                        );
                    }
                    _ => res.push(c),
                },
                _ => res.push(c),
            }
        }
        // The current position is past the end of the file (on the None character) so we need to bring it back
        tok!(Lit(InvalidStr(res, Unclosed)), start, self.cursor.pos - 1)
    }

    /// Creates an operator
    /// This function is greedy, it takes the largest valid operator it can make
    /// - these may need to be split up once there is more context.
    fn tok_op(&mut self) -> Tok {
        if let Some(c) = self.curr_char() {
            use OpKind::*;
            let mut kind = match c {
                '+' => Plus,
                '-' => Minus,
                '*' => Mul,
                '/' => Div,
                '%' => Mod,
                '(' => LParen,
                ')' => RParen,
                '[' => LSquare,
                ']' => RSquare,
                '{' => LCurly,
                '}' => RCurly,
                '.' => Dot,
                ':' => Colon,
                ';' => Semi,
                '@' => At,
                '#' => Hash,
                '~' => Tilde,
                '&' => Amp,
                '|' => Pipe,
                '!' => Bang,
                '=' => Eq,
                '$' => Dollar,
                '<' => Lt,
                '>' => Gt,
                _ => panic!("Called tok_op with cursor not over operator character."),
            };
            let start = self.cursor.pos;
            let mut end = start;
            while let Some(c) = self.peek_char() {
                if is_op_tail(c)
                    && let Some(new_kind) = kind.try_expand(c)
                {
                    self.next_char();
                    end = self.cursor.pos;
                    kind = new_kind
                } else {
                    break;
                }
            }
            Tok::new(TokKind::Op(kind), (start, end).into())
        } else {
            panic!("Called tok_op when cursor is not on a character.")
        }
    }
}

pub struct TokenStream<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Tok;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next_token()
    }
}

impl<'a> IntoIterator for Lexer<'a> {
    type Item = Tok;

    type IntoIter = TokenStream<'a>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter { lexer: self }
    }
}

/// Peekable iterator over the characters of a source
pub struct Cursor<'a> {
    chars: Peekable<Chars<'a>>,
    pos: u32,
    curr: Option<char>,
}

impl<'a> Cursor<'a> {
    fn new(source: &'a str) -> Self {
        let mut cursor = Cursor {
            chars: source.chars().peekable(),
            pos: 0,
            curr: None,
        };
        // Start on the first character
        cursor.next();
        // We want tokens to be zero-indexed
        cursor.pos = 0;
        cursor
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }
}

impl<'a> Iterator for Cursor<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        self.pos += 1;
        self.curr = self.chars.next();
        self.curr
    }
}

#[inline(always)]
fn is_ident_head(c: &char) -> bool {
    c.is_alphabetic() || *c == '_'
}

#[inline(always)]
fn is_ident_tail(c: &char) -> bool {
    c.is_alphanumeric() || *c == '_'
}

fn is_op_head(c: &char) -> bool {
    match c {
        '+' | '-' | '*' | '/' | '%' | '(' | ')' | '[' | ']' | '{' | '}' | '.' | ':' | ';' | '@'
        | '#' | '~' | '&' | '|' | '!' | '=' | '$' | '<' | '>' => true,
        _ => false,
    }
}

fn is_op_tail(c: &char) -> bool {
    matches!(c, '=' | '&' | '|' | ':' | '.' | '<' | '>' | '?')
}

/// Returns true if the given character is considered whitespace
/// This is lifted from Rust's definition of whitespace
/// https://github.com/rust-lang/rust/blob/main/compiler/rustc_lexer/src/lib.rs
fn is_whitespace(c: &char) -> bool {
    matches!(
        *c,
        // End-of-line characters
        | '\u{000A}' // line feed (\n)
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // carriage return (\r)
        | '\u{0085}' // next line (from latin1)
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR

        // `Default_Ignorable_Code_Point` characters
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Horizontal space characters
        | '\u{0009}'   // tab (\t)
        | '\u{0020}' // space
    )
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! test {
        ($source:literal, $($tok:expr),* $(,)?) => {
            assert_eq!(Lexer::new($source).into_iter().collect::<Vec<Tok>>(), vec![$(
                $tok,
            )*])
        }
    }

    #[test]
    fn ident() {
        test!("hello", tok!(Ident("hello".to_string()), 0, 4));
        test!("_hello", tok!(Ident("_hello".to_string()), 0, 5));
        test!("t1_var_?", tok!(Ident("t1_var_?".to_string()), 0, 7));
    }

    #[test]
    fn operators() {
        use OpKind::*;
        test!(
            "+=+-/~?<<=",
            tok!(Op(PlusEq), 0, 1),
            tok!(Op(Plus), 2),
            tok!(Op(Minus), 3),
            tok!(Op(Div), 4),
            tok!(Op(TildeQuestion), 5, 6),
            tok!(Op(ShiftLEq), 7, 9)
        )
    }

    #[test]
    fn whitespace() {
        test!("     ident", tok!(Ident("ident".to_string()), 5, 9));
        test!("     \n\r\tident", tok!(Ident("ident".to_string()), 8, 12))
    }

    #[test]
    fn strings() {
        test!(
            "\"Hello World\"",
            tok!(Lit(Str("Hello World".to_string())), 0, 12)
        );
        test!(
            "\"Hello World",
            tok!(Lit(InvalidStr("Hello World".to_string(), Unclosed)), 0, 11)
        );
        test!("\"\\t\"", tok!(Lit(Str("\t".to_string())), 0, 3))
    }

    #[test]
    fn bools() {
        test!("true", tok!(Lit(LitKind::Bool(true)), 0, 3));
        test!("false", tok!(Lit(LitKind::Bool(false)), 0, 4))
    }
}
