#[inline(always)]
pub fn is_ident_head(c: &char) -> bool {
    c.is_alphabetic() || *c == '_'
}

#[inline(always)]
pub fn is_ident_tail(c: &char) -> bool {
    c.is_alphanumeric() || *c == '_'
}

// Allowed due to poor formatting of matches macro by rust-analyzer
#[allow(clippy::match_like_matches_macro)]
#[inline(always)]
pub fn is_op_head(c: &char) -> bool {
    match c {
        '+' | '-' | '*' | '/' | '%' | '(' | ')' | '[' | ']' | '{' | '}' | ',' | '.' | ':' | ';'
        | '@' | '#' | '~' | '&' | '^' | '|' | '!' | '=' | '$' | '<' | '>' => true,
        _ => false,
    }
}

#[inline(always)]
pub fn is_op_tail(c: &char) -> bool {
    matches!(c, '=' | '&' | '|' | ':' | '.' | '<' | '>' | '?')
}

/// Returns true if the given character is considered whitespace
/// This is lifted from Rust's definition of whitespace
/// https://github.com/rust-lang/rust/blob/main/compiler/rustc_lexer/src/lib.rs
#[inline(always)]
pub fn is_whitespace(c: &char) -> bool {
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

#[inline(always)]
pub fn is_newline(c: &char) -> bool {
    matches!(
        *c,
        | '\u{000A}' // line feed (\n)
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // carriage return (\r)
        | '\u{0085}' // next line (from latin1)
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}
