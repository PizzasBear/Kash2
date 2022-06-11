use crate::{lines, Ascii7, Span};
use std::{error::Error, fmt, path::Path};

// artemis fowl vs johan liebert

#[derive(Debug)]
pub struct Ident<'a> {
    pub name: String,
    pub span: Span<'a>,
}

#[derive(Debug)]
pub struct Punct7<'a> {
    pub punct: Ascii7,
    pub span: Span<'a>,
}

#[derive(Debug)]
pub struct PunctString<'a> {
    pub punct: String,
    pub span: Span<'a>,
}

enum Punct<'a> {
    String(PunctString<'a>),
    Ascii7(Punct7<'a>),
}

pub const PUNCT_CHARS: &'static [u8] = &[
    b'+', b'-', b'*', b'/', b'%', b'<', b'>', // math ops
    b'!', b'^', b'&', b'|', // bool & bit ops
    b'=', b'.', b';', b',', // general ops
    b'@', b'#', b'$', b':', b'?', b'`', b'~', b'\\', // reserved puncts
];

pub const SPECIAL_CHARS: &'static [u8] = &[b'(', b')', b'[', b']', b'{', b'}', b'\"', b'\''];

#[derive(Debug)]
pub struct StrLiteral<'a> {
    pub value: String,
    pub span: Span<'a>,
}

#[derive(Debug)]
pub struct IntLiteral<'a> {
    pub value: i64,
    pub span: Span<'a>,
}

#[derive(Debug)]
pub struct FloatLiteral<'a> {
    pub value: f64,
    pub span: Span<'a>,
}

#[derive(Debug)]
pub enum NumberLiteral<'a> {
    Int(IntLiteral<'a>),
    Float(FloatLiteral<'a>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Delimiter {
    Parentheses,
    Brackets,
    Braces,
    None,
}

impl Delimiter {
    pub const fn is_opening(ch: char) -> bool {
        matches!(ch, '(' | '[' | '{')
    }

    pub const fn is_opening_ascii(ch: u8) -> bool {
        matches!(ch, b'(' | b'[' | b'{')
    }

    pub const fn is_closing(ch: char) -> bool {
        matches!(ch, ')' | ']' | '}')
    }

    pub const fn is_closing_ascii(ch: u8) -> bool {
        matches!(ch, b')' | b']' | b'}')
    }

    pub const fn get_open(self) -> char {
        match self {
            Self::Parentheses => '(',
            Self::Brackets => '[',
            Self::Braces => '{',
            Self::None => '\0',
        }
    }

    pub const fn get_open_ascii(self) -> u8 {
        match self {
            Self::Parentheses => b'(',
            Self::Brackets => b'[',
            Self::Braces => b'{',
            Self::None => b'\0',
        }
    }

    pub const fn get_close(self) -> char {
        match self {
            Self::Parentheses => ')',
            Self::Brackets => ']',
            Self::Braces => '}',
            Self::None => '\0',
        }
    }

    pub const fn get_close_ascii(self) -> u8 {
        match self {
            Self::Parentheses => b')',
            Self::Brackets => b']',
            Self::Braces => b'}',
            Self::None => b'\0',
        }
    }
}

impl From<char> for Delimiter {
    fn from(ch: char) -> Self {
        match ch {
            '(' | ')' => Self::Parentheses,
            '[' | ']' => Self::Brackets,
            '{' | '}' => Self::Braces,
            _ => Self::None,
        }
    }
}

#[derive(Debug)]
pub struct Group<'a> {
    pub delim: Delimiter,
    pub tokens: Vec<TokenTree<'a>>,
    pub span: Span<'a>,
}

#[derive(Debug)]
pub struct NewLine<'a> {
    pub span: Span<'a>,
}

#[derive(Debug)]
pub enum TokenTree<'a> {
    NewLine(NewLine<'a>),
    Ident(Ident<'a>),
    Punct7(Punct7<'a>),
    PunctString(PunctString<'a>),
    Group(Group<'a>),
    StrLiteral(StrLiteral<'a>),
    IntLiteral(IntLiteral<'a>),
    FloatLiteral(FloatLiteral<'a>),
}

#[derive(Debug)]
pub enum UngroupedToken<'a> {
    NewLine(NewLine<'a>),
    Ident(Ident<'a>),
    Punct7(Punct7<'a>),
    PunctString(PunctString<'a>),
    StrLiteral(StrLiteral<'a>),
    IntLiteral(IntLiteral<'a>),
    FloatLiteral(FloatLiteral<'a>),
    OpenGroup { delim: Delimiter, span: Span<'a> },
    CloseGroup { delim: Delimiter, span: Span<'a> },
}

#[derive(Debug)]
pub enum LexingError<'a> {
    MissingOpenDelim {
        span: Span<'a>,
    },
    MissingCloseDelim {
        open_span: Span<'a>,
        close_span: Span<'a>,
    },
    NonMatchingDelim {
        open_span: Span<'a>,
        close_span: Span<'a>,
    },
    UnsupportedNumberBase {
        span: Span<'a>,
        base: char,
    },
    NonDecimalFloat {
        span: Span<'a>,
        base: u8,
    },
    UnclosedString {
        span: Span<'a>,
    },
    ParseIntError {
        span: Span<'a>,
        err: std::num::ParseIntError,
    },
    OutOfRangeAsciiEscape {
        span: Span<'a>,
    },
    Expected {
        span: Span<'a>,
        s: &'static str,
    },
    InvalidUnicodeEscape {
        span: Span<'a>,
    },
    UnsupportedEscapeChar {
        span: Span<'a>,
        ch: char,
    },
}

impl<'a> LexingError<'a> {
    pub fn spans(&self) -> Vec<Span<'a>> {
        match *self {
            Self::MissingOpenDelim { span } => vec![span],
            Self::MissingCloseDelim {
                open_span,
                close_span,
            } => vec![open_span, close_span],
            Self::NonMatchingDelim {
                open_span,
                close_span,
            } => vec![open_span, close_span],
            Self::UnsupportedNumberBase { span, .. } => vec![span],
            Self::NonDecimalFloat { span, .. } => vec![span],
            Self::UnclosedString { span } => vec![span],
            Self::ParseIntError { span, .. } => vec![span],
            Self::OutOfRangeAsciiEscape { span } => vec![span],
            Self::Expected { span, .. } => vec![span],
            Self::InvalidUnicodeEscape { span } => vec![span],
            Self::UnsupportedEscapeChar { span, .. } => vec![span],
        }
    }
}

impl<'a> fmt::Display for LexingError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::MissingOpenDelim { span } => write!(
                f,
                "Found closing delimiter at {span} without a matching opening delimiter"
            ),
            Self::MissingCloseDelim {
                open_span,
                close_span,
            } => write!(
                f,
                "Missing closing delimiter at {close_span} for opening at {open_span}"
            ),
            Self::NonMatchingDelim {
                open_span,
                close_span,
            } => {
                write!(
                    f,
                    "Non-matching delimiters at {open_span} and at {close_span}"
                )
            }
            Self::UnsupportedNumberBase { span, base } => {
                write!(f, "Unsupported number base `{base}` at {span}")
            }
            Self::NonDecimalFloat { span, base } => {
                write!(f, "Non decimal float with base {base} at {span}")
            }
            Self::UnclosedString { span } => {
                write!(f, "Unclosed string at {span}")
            }
            Self::ParseIntError { span, ref err } => {
                write!(f, "Parse int error at {span}: {err}")
            }
            Self::OutOfRangeAsciiEscape { span } => {
                write!(
                    f,
                    "Out of range hex escape at {span}, must be in range [\\x00-\\x7f]"
                )
            }
            Self::Expected { span, s } => {
                write!(f, "Expected `{s}` at {span}")
            }
            Self::InvalidUnicodeEscape { span } => {
                write!(f, "Invalid unicode escape at {span}")
            }
            Self::UnsupportedEscapeChar { span, ch } => {
                write!(f, "Unsupported string escape character `{ch}` at {span}")
            }
        }
    }
}

impl<'a> Error for LexingError<'a> {}

impl<'a> Group<'a> {
    pub fn post_process(&mut self) {
        let mut tokens = Vec::with_capacity(self.tokens.len());
        for token in self.tokens.drain(..) {
            match (tokens.last(), token) {
                (None | Some(TokenTree::NewLine(_)), TokenTree::NewLine(_)) => {}
                (
                    Some(TokenTree::Punct7(Punct7 {
                        punct:
                            Ascii7 {
                                len: 1,
                                chars: [b'\\', 0, 0, 0, 0, 0, 0],
                            },
                        ..
                    })),
                    TokenTree::NewLine(_),
                ) => {
                    tokens.pop();
                }
                (
                    _,
                    TokenTree::Punct7(Punct7 {
                        punct:
                            Ascii7 {
                                len: 1,
                                chars: [b'\\', 0, 0, 0, 0, 0, 0],
                            },
                        span,
                    }),
                ) => {
                    tokens.push(TokenTree::NewLine(NewLine { span }));
                }
                (_, TokenTree::Group(mut group)) => {
                    group.post_process();
                    tokens.push(TokenTree::Group(group));
                }
                (_, token) => {
                    tokens.push(token);
                }
            }
        }
        self.tokens = tokens;
    }
}

impl<'a> TokenTree<'a> {
    pub fn span(&self) -> Span<'a> {
        match *self {
            Self::Ident(Ident { span, .. }) => span,
            Self::Group(Group { span, .. }) => span,
            Self::Punct7(Punct7 { span, .. }) => span,
            Self::PunctString(PunctString { span, .. }) => span,
            Self::NewLine(NewLine { span, .. }) => span,
            Self::StrLiteral(StrLiteral { span, .. }) => span,
            Self::IntLiteral(IntLiteral { span, .. }) => span,
            Self::FloatLiteral(FloatLiteral { span, .. }) => span,
        }
    }

    pub fn post_process(&mut self) {
        match self {
            Self::Group(group) => group.post_process(),
            _ => {}
        }
    }

    pub fn tokenize<P: AsRef<Path> + ?Sized>(
        path: &'a P,
        s: &str,
    ) -> Result<Group<'a>, LexingError<'a>> {
        let path = path.as_ref();
        let mut group = Group {
            delim: Delimiter::None,
            tokens: Vec::new(),
            span: Span::new(path, 0, s.len() - 1),
        };

        let mut i = 0;
        while let Some(token) = Self::next_token(&mut i, path, s)? {
            match token {
                UngroupedToken::OpenGroup { delim, span } => {
                    group
                        .tokens
                        .push(Self::Group(Self::tokenize_rec(&mut i, span, s, delim)?));
                }
                UngroupedToken::CloseGroup { span, .. } => {
                    // panic!("Found closing delimiter without a matching opening delimiter");
                    return Err(LexingError::MissingOpenDelim { span });
                }
                UngroupedToken::NewLine(new_line) => {
                    group.tokens.push(Self::NewLine(new_line));
                }
                UngroupedToken::Ident(ident) => {
                    group.tokens.push(Self::Ident(ident));
                }
                UngroupedToken::StrLiteral(str_literal) => {
                    group.tokens.push(Self::StrLiteral(str_literal));
                }
                UngroupedToken::IntLiteral(int_literal) => {
                    group.tokens.push(Self::IntLiteral(int_literal));
                }
                UngroupedToken::FloatLiteral(float_literal) => {
                    group.tokens.push(Self::FloatLiteral(float_literal));
                }
                UngroupedToken::Punct7(punct) => {
                    group.tokens.push(Self::Punct7(punct));
                }
                UngroupedToken::PunctString(punct) => {
                    group.tokens.push(Self::PunctString(punct));
                }
            }
        }

        Ok(group)
    }

    pub fn tokenize_rec(
        ptr: &mut usize,
        open_span: Span<'a>,
        s: &str,
        delim: Delimiter,
    ) -> Result<Group<'a>, LexingError<'a>> {
        let path = open_span.path;

        let mut group = Group {
            delim,
            tokens: Vec::new(),
            span: open_span,
        };

        while let Some(token) = Self::next_token(ptr, path, s)? {
            match token {
                UngroupedToken::OpenGroup { delim, span } => {
                    group
                        .tokens
                        .push(Self::Group(Self::tokenize_rec(ptr, span, s, delim)?));
                }
                UngroupedToken::CloseGroup {
                    span,
                    delim: close_delim,
                } => {
                    if delim != close_delim {
                        // span.display(&lines(s), s);
                        return Err(LexingError::NonMatchingDelim {
                            open_span,
                            close_span: span,
                        });
                    }
                    group.span.end = span.end;
                    return Ok(group);
                }
                UngroupedToken::NewLine(new_line) => {
                    group.tokens.push(Self::NewLine(new_line));
                }
                UngroupedToken::Ident(ident) => {
                    group.tokens.push(Self::Ident(ident));
                }
                UngroupedToken::StrLiteral(str_literal) => {
                    group.tokens.push(Self::StrLiteral(str_literal));
                }
                UngroupedToken::IntLiteral(int_literal) => {
                    group.tokens.push(Self::IntLiteral(int_literal));
                }
                UngroupedToken::FloatLiteral(float_literal) => {
                    group.tokens.push(Self::FloatLiteral(float_literal));
                }
                UngroupedToken::Punct7(punct) => {
                    group.tokens.push(Self::Punct7(punct));
                }
                UngroupedToken::PunctString(punct) => {
                    group.tokens.push(Self::PunctString(punct));
                }
            }
        }

        // open_span.display(&lines(s), s);
        Err(LexingError::MissingCloseDelim {
            open_span,
            close_span: Span::new(path, s.len(), s.len()),
        })
        // panic!("Didn't find closing delimiter")
    }

    fn next_num(
        ptr: &mut usize,
        path: &'a Path,
        s: &str,
    ) -> Result<NumberLiteral<'a>, LexingError<'a>> {
        let start = *ptr;
        let sign;
        (sign, *ptr) = match s.as_bytes()[start] {
            b'+' => (1, start + 1),
            b'-' => (-1, start + 1),
            _ => (1, start),
        };
        let post_sign_start = *ptr;

        let radix = if s.as_bytes()[post_sign_start] == b'0' {
            *ptr += 2;
            match s.as_bytes()[post_sign_start + 1] {
                b'x' => 16,
                b'o' => 8,
                b'b' => 2,
                b'0'..=b'9' => 10,
                ch if ch.is_ascii_whitespace()
                    || PUNCT_CHARS.contains(&ch)
                    || SPECIAL_CHARS.contains(&ch) =>
                {
                    *ptr -= 1;
                    10
                }
                _ => {
                    return Err(LexingError::UnsupportedNumberBase {
                        span: Span::new(
                            path,
                            post_sign_start + 1,
                            s.get(post_sign_start + 1..)
                                .unwrap()
                                .char_indices()
                                .nth(1)
                                .map(|(i, _)| post_sign_start + 1 + i)
                                .unwrap(),
                        ),
                        base: s
                            .get(post_sign_start + 1..)
                            .unwrap()
                            .chars()
                            .nth(1)
                            .unwrap(),
                    });
                    // panic!("Unsupported number base");
                }
            }
        } else {
            10
        };
        while s.as_bytes()[*ptr].is_ascii_digit() {
            *ptr += 1;
        }

        if b'.' == s.as_bytes()[*ptr] && {
            let ch = s.get(*ptr + 1..).unwrap().chars().next().unwrap();
            ch.is_ascii_digit()
                || ch.is_whitespace()
                || Delimiter::from(ch) != Delimiter::None
                || ch != '.' && PUNCT_CHARS.contains(&(ch as u8))
                || SPECIAL_CHARS.contains(&(ch as u8))
        } {
            *ptr += 1;
            if radix != 10 {
                // panic!("Non decimal floats not supported");
                return Err(LexingError::NonDecimalFloat {
                    span: Span::new(path, start, *ptr),
                    base: radix as _,
                });
            }
            while s.as_bytes()[*ptr].is_ascii_digit() {
                *ptr += 1;
            }
            Ok(NumberLiteral::Float(FloatLiteral {
                value: { s.get(start..*ptr).unwrap().parse().unwrap() },
                span: Span::new(path, start, *ptr),
            }))
        } else {
            Ok(NumberLiteral::Int(IntLiteral {
                value: {
                    sign * i64::from_str_radix(
                        s.get(post_sign_start + if radix == 10 { 0 } else { 2 }..*ptr)
                            .unwrap(),
                        radix,
                    )
                    .unwrap()
                },
                span: Span::new(path, start, *ptr),
            }))
        }
    }

    fn next_punct(ptr: &mut usize, path: &'a Path, s: &str) -> Result<Punct<'a>, LexingError<'a>> {
        let start = *ptr;
        while PUNCT_CHARS.contains(&s.as_bytes()[*ptr]) {
            *ptr += 1;
        }
        let span = Span::new(path, start, *ptr);
        // println!(
        //     "Punct: {:?}",
        //     std::str::from_utf8(&s.as_bytes()[start..*ptr]).unwrap(),
        // );
        if let Ok(s) = Ascii7::try_from(&s.as_bytes()[start..*ptr]) {
            Ok(Punct::Ascii7(Punct7 { punct: s, span }))
        } else {
            Ok(Punct::String(PunctString {
                punct: std::str::from_utf8(&s.as_bytes()[start..*ptr])
                    .unwrap()
                    .to_owned(),
                span,
            }))
        }
    }

    fn next_str(
        ptr: &mut usize,
        path: &'a Path,
        s: &str,
        tiny: bool,
    ) -> Result<StrLiteral<'a>, LexingError<'a>> {
        let start = *ptr;
        let mut char_indices = s.get(start + 1..).unwrap().char_indices();
        let mut value = String::new();

        let unclosed_string = |end| {
            // panic!("Unclosed string");
            LexingError::UnclosedString {
                span: Span::new(path, start, start + 1 + end),
            }
        };

        let mut last_ch = s.as_bytes()[start + 1] as char;
        let mut last_idx = 0;
        loop {
            let (i, ch) = char_indices
                .next()
                .ok_or_else(|| unclosed_string(last_idx + last_ch.len_utf8()))?;
            match ch {
                '\\' => match char_indices.next().ok_or_else(|| unclosed_string(i + 1))?.1 {
                    '\\' => value.push('\\'),
                    ch if ch.is_whitespace() => value.push(ch),
                    '\'' => value.push('\''),
                    '"' => value.push('"'),
                    '0' => value.push('\0'),
                    'n' => value.push('\n'),
                    'r' => value.push('\r'),
                    't' => value.push('\t'),
                    'x' => {
                        let (j, ch2) = char_indices.next().ok_or_else(|| unclosed_string(i + 2))?;
                        let (j, ch2) = char_indices
                            .next()
                            .ok_or_else(|| unclosed_string(j + ch2.len_utf8()))?;
                        let x = u8::from_str_radix(
                            s.get(start + i + 2..start + 1 + j + ch2.len_utf8())
                                .unwrap(),
                            16,
                        )
                        .map_err(|err| {
                            LexingError::ParseIntError {
                                span: Span::new(
                                    path,
                                    start + i + 3,
                                    start + 1 + j + ch2.len_utf8(),
                                ),
                                err,
                            }
                            // panic!(
                            //     "Invalid characters in numeric character escape (hex) ({})",
                            //     err
                            // );
                        })?;
                        if 0x80 <= x {
                            // panic!("Out of range hex escape, must be in range [\\x00-\\x7f]");
                            return Err(LexingError::OutOfRangeAsciiEscape {
                                span: Span::new(path, start + i + 2, start + i + 4),
                            });
                        }
                        value.push(x as _);
                    }
                    'u' => {
                        {
                            let (j, ch2) =
                                char_indices.next().ok_or_else(|| unclosed_string(i + 2))?;
                            if ch2 != '{' {
                                // panic!("Missing '{{' to start unicode escape");
                                return Err(LexingError::Expected {
                                    span: Span::new(
                                        path,
                                        start + i + 3,
                                        start + 1 + j + ch2.len_utf8(),
                                    ),
                                    s: "{",
                                });
                            }
                        }
                        let mut next_j = i + 3;
                        let j = loop {
                            let (j, ch) =
                                char_indices.next().ok_or_else(|| unclosed_string(next_j))?;
                            if ch == '}' {
                                break j;
                            } else {
                                next_j = j + ch.len_utf8();
                            }
                        };
                        value.push(
                            char::from_u32(
                                u32::from_str_radix(
                                    s.get(start + i + 4..start + 1 + j).unwrap(),
                                    16,
                                )
                                .map_err(|err| {
                                    // panic!("Invalid hex in unicode escape ({})", err);
                                    LexingError::ParseIntError {
                                        span: Span::new(path, start + i + 4, start + 1 + j),
                                        err,
                                    }
                                })?,
                            )
                            .ok_or_else(|| {
                                // panic!("Invalid unicode escape");
                                LexingError::InvalidUnicodeEscape {
                                    span: Span::new(path, start + i + 4, start + 1 + j),
                                }
                            })?,
                        );
                    }
                    ch => {
                        // panic!("Bad escape character: '{}'", ch);
                        return Err(LexingError::UnsupportedEscapeChar {
                            span: Span::new(
                                path,
                                start + i + 1,
                                start + char_indices.nth(1).map(|(i, _)| i).unwrap_or(s.len()),
                            ),
                            ch,
                        });
                    }
                },
                '"' if !tiny => {
                    *ptr = start + 2 + i;
                    return Ok(StrLiteral {
                        value,
                        span: Span::new(path, start, *ptr),
                    });
                }
                _ if tiny && ch.is_whitespace() => {
                    *ptr = start + 1 + i;
                    return Ok(StrLiteral {
                        value,
                        span: Span::new(path, start, *ptr),
                    });
                }
                _ => value.push(ch),
            }
            last_ch = ch;
            last_idx = i;
        }
    }

    fn next_ident(ptr: &mut usize, path: &'a Path, s: &str) -> Result<Ident<'a>, LexingError<'a>> {
        let start = *ptr;
        let mut char_indices = s.get(start..).unwrap().char_indices();
        loop {
            let (i, ch) = char_indices.next().unwrap();
            if Self::is_stop_char(ch) {
                *ptr = start + i;
                return Ok(Ident {
                    name: s.get(start..*ptr).unwrap().to_owned(),
                    span: Span::new(path, start, *ptr),
                });
            }
        }
    }

    fn next_token(
        ptr: &mut usize,
        path: &'a Path,
        s: &str,
    ) -> Result<Option<UngroupedToken<'a>>, LexingError<'a>> {
        let mut char_indices;
        if let Some(s) = s.get(*ptr..) {
            char_indices = s.char_indices();
        } else {
            return Ok(None);
        };

        // Skips the whitespace
        let start = *ptr;
        let ch = loop {
            let (i, ch) = if let Some(x) = char_indices.next() {
                x
            } else {
                return Ok(None);
            };
            if !ch.is_whitespace() || ch == '\n' {
                *ptr = start + i;
                break ch;
            }
        };

        match ch {
            '0'..='9' => Ok(Some(match Self::next_num(ptr, path, s)? {
                NumberLiteral::Int(int_literal) => UngroupedToken::IntLiteral(int_literal),
                NumberLiteral::Float(float_literal) => UngroupedToken::FloatLiteral(float_literal),
            })),
            '"' => Ok(Some(UngroupedToken::StrLiteral(Self::next_str(
                ptr, path, s, false,
            )?))),
            '\'' => Ok(Some(UngroupedToken::StrLiteral(Self::next_str(
                ptr, path, s, true,
            )?))),
            '\n' => {
                *ptr += 1;
                Ok(Some(UngroupedToken::NewLine(NewLine {
                    span: Span::new(path, *ptr - 1, *ptr),
                })))
            }
            '#' => {
                let (i, ch);
                if let Some(x) = char_indices.next() {
                    (i, ch) = x;
                } else {
                    return Ok(None);
                };
                match ch {
                    '(' => {
                        let mut depth = 1;
                        loop {
                            let ch = if let Some((_, ch)) = char_indices.next() {
                                ch
                            } else {
                                return Ok(None);
                            };
                            match ch {
                                ')' => {
                                    let (i, ch) = if let Some(x) = char_indices.next() {
                                        x
                                    } else {
                                        return Ok(None);
                                    };
                                    if ch == '#' {
                                        depth -= 1;
                                        if depth == 0 {
                                            *ptr = start + i + 1;
                                            break Self::next_token(ptr, path, s);
                                        }
                                    }
                                }
                                '#' if (if let Some((_, ch)) = char_indices.next() {
                                    ch == '('
                                } else {
                                    return Ok(None);
                                }) =>
                                {
                                    depth += 1;
                                }
                                _ => {}
                            }
                        }
                    }
                    '\n' => {
                        *ptr = start + i + 1;
                        Ok(Some(UngroupedToken::NewLine(NewLine {
                            span: Span::new(path, *ptr - 1, *ptr),
                        })))
                    }
                    _ => loop {
                        let (i, ch) = if let Some(x) = char_indices.next() {
                            x
                        } else {
                            return Ok(None);
                        };
                        if ch == '\n' {
                            *ptr = start + i + 1;
                            break Ok(Some(UngroupedToken::NewLine(NewLine {
                                span: Span::new(path, *ptr - 1, *ptr),
                            })));
                        }
                    },
                }
            }
            '-' | '+' if matches!(s.as_bytes()[*ptr + 1], b'0'..=b'9') => {
                Ok(Some(match Self::next_num(ptr, path, s)? {
                    NumberLiteral::Int(int_literal) => UngroupedToken::IntLiteral(int_literal),
                    NumberLiteral::Float(float_literal) => {
                        UngroupedToken::FloatLiteral(float_literal)
                    }
                }))
            }
            ch if Delimiter::is_opening(ch) => {
                *ptr += 1;
                Ok(Some(UngroupedToken::OpenGroup {
                    delim: ch.into(),
                    span: Span::new(path, *ptr - 1, *ptr),
                }))
            }
            ch if Delimiter::is_closing(ch) => {
                *ptr += 1;
                Ok(Some(UngroupedToken::CloseGroup {
                    delim: ch.into(),
                    span: Span::new(path, *ptr - 1, *ptr),
                }))
            }
            ch if PUNCT_CHARS.contains(&(ch as u8)) => match Self::next_punct(ptr, path, s)? {
                Punct::Ascii7(punct) => Ok(Some(UngroupedToken::Punct7(punct))),
                Punct::String(punct) => Ok(Some(UngroupedToken::PunctString(punct))),
            },
            _ => Ok(Some(UngroupedToken::Ident(Self::next_ident(ptr, path, s)?))),
        }
    }

    fn is_stop_char(ch: char) -> bool {
        ch.is_whitespace()
            || PUNCT_CHARS.contains(&(ch as u8))
            || SPECIAL_CHARS.contains(&(ch as u8))
    }
}
