use crate::Span;
use std::{error::Error, fmt, path::Path};

// artemis fowl vs johan liebert

pub struct Ident<'a> {
    pub name: String,
    pub span: Span<'a>,
}

impl<'a> fmt::Debug for Ident<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Ident({:?} at '{}')", self.name, self.span)
    }
}

pub struct Punct<'a> {
    pub ch: u8,
    pub span: Span<'a>,
}

impl<'a> Punct<'a> {
    pub const END: u8 = !0;

    #[inline]
    pub const fn end(span: Span<'a>) -> Self {
        Self {
            ch: Self::END,
            span,
        }
    }

    #[inline]
    pub const fn is_end(&self) -> bool {
        self.ch == Self::END
    }
}

impl<'a> fmt::Debug for Punct<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_end() {
            write!(f, "Punct(END at '{}')", self.span)
        } else {
            write!(f, "Punct({:?} at '{}')", self.ch as char, self.span)
        }
    }
}

#[derive(Debug)]
struct PunctString<'a> {
    puncts: Vec<u8>,
    span: Span<'a>,
}
// #[derive(Debug)]
// pub struct Punct7<'a> {
//     pub punct: Ascii7,
//     pub span: Span<'a>,
// }
//
// #[derive(Debug)]
// pub struct PunctString<'a> {
//     pub punct: String,
//     pub span: Span<'a>,
// }
//
// enum Punct<'a> {
//     String(PunctString<'a>),
//     Ascii7(Punct7<'a>),
// }

pub const PUNCT_CHARS: &'static [u8] = &[
    b'+', b'-', b'*', b'/', b'%', b'<', b'>', // math ops
    b'!', b'^', b'&', b'|', // bool & bit ops
    b'=', b'.', b';', b',', // general ops
    b'@', b'#', b'$', b':', b'?', b'`', b'~', b'\\', // reserved puncts
];

pub const SPECIAL_CHARS: &'static [u8] = &[b'(', b')', b'[', b']', b'{', b'}', b'\"', b'\''];

pub struct StrLiteral<'a> {
    pub value: String,
    pub span: Span<'a>,
}

impl<'a> fmt::Debug for StrLiteral<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "StrLiteral({:?} at '{}')", &self.value, self.span)
    }
}

pub struct IntLiteral<'a> {
    pub value: i64,
    pub span: Span<'a>,
}

impl<'a> fmt::Debug for IntLiteral<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "IntLiteral({:?} at '{}')", &self.value, self.span)
    }
}

pub struct FloatLiteral<'a> {
    pub value: f64,
    pub span: Span<'a>,
}

impl<'a> fmt::Debug for FloatLiteral<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FloatLiteral({:?} at '{}')", &self.value, self.span)
    }
}

#[derive(Debug)]
pub enum NumberLiteral<'a> {
    Int(IntLiteral<'a>),
    Float(FloatLiteral<'a>),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Delimiter {
    Parentheses,
    Brackets,
    Braces,
    None,
}

impl fmt::Debug for Delimiter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parentheses => write!(f, "Delim()"),
            Self::Brackets => write!(f, "Delim[]"),
            Self::Braces => write!(f, "Delim{{}}"),
            Self::None => write!(f, "Delim::None"),
        }
    }
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

pub struct Group<'a> {
    pub delim: Delimiter,
    pub tokens: Vec<TokenTree<'a>>,
    pub span: Span<'a>,
}

impl<'a> fmt::Debug for Group<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Group(at '{}', {:?} of ", self.span, self.delim)?;
        f.debug_list().entries(self.tokens.iter()).finish()?;
        write!(f, ")")
    }
}

pub struct NewLine<'a> {
    pub span: Span<'a>,
}

impl<'a> fmt::Debug for NewLine<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NewLine(at '{}')", self.span)
    }
}

pub mod pat {
    macro_rules! new_line {
        () => {
            $crate::lexer::TokenTree::NewLine(_)
        };
        (_) => {
            $crate::lexer::pat::new_line!()
        };
        ({ $($tt:tt)+ }) => {
            $crate::lexer::TokenTree::NewLine($crate::lexer::NewLine { $($tt)+ })
        };
    }
    macro_rules! punct {
        () => {
            $crate::lexer::TokenTree::Punct(_)
        };
        (_) => {
            $crate::lexer::pat::punct!()
        };
        ($ch:literal) => {
            $crate::lexer::TokenTree::Punct($crate::lexer::Punct { ch: $ch, .. })
        };
        (END) => {
            $crate::lexer::TokenTree::Punct($crate::lexer::Punct {
                ch: $crate::lexer::Punct::END,
                ..
            })
        };
        ({ $($tt:tt)+ }) => {
            $crate::lexer::TokenTree::Punct($crate::lexer::Punct { $($tt)+ })
        };
        ([ $($tt:tt)|+ ]) => {
            $($crate::lexer::pat::punct!($tt))|+
        };
    }
    macro_rules! ident {
        () => {
            $crate::lexer::TokenTree::Ident(_)
        };
        (_) => {
            $crate::lexer::pat::ident!()
        };
        // ($name:literal) => {
        //     $crate::lexer::TokenTree::Ident($crate::lexer::Ident { name: $name, .. })
        // };
        ({ $($tt:tt)+ }) => {
            $crate::lexer::TokenTree::Ident($crate::lexer::Ident { $($tt)+ })
        };
        ([ $($tt:tt)|+ ]) => {
            $($crate::lexer::pat::ident!($tt))|+
        };
    }
    macro_rules! int {
        () => {
            $crate::lexer::TokenTree::IntLiteral(_)
        };
        (_) => {
            $crate::lexer::pat::int!()
        };
        ($num:literal) => {
            $crate::lexer::TokenTree::IntLiteral($crate::lexer::IntLiteral { value: $num, .. })
        };
        ({ $($tt:tt)+ }) => {
            $crate::lexer::TokenTree::IntLiteral($crate::lexer::IntLiteral { $($tt)+ })
        };
        ([ $($tt:tt)|+ ]) => {
            $($crate::lexer::pat::int!($tt))|+
        };
    }
    macro_rules! float {
        () => {
            $crate::lexer::TokenTree::FloatLiteral(_)
        };
        (_) => {
            $crate::lexer::pat::float!()
        };
        ($num:literal) => {
            $crate::lexer::TokenTree::FloatLiteral($crate::lexer::FloatLiteral { value: $num, .. })
        };
        ({ $($tt:tt)+ }) => {
            $crate::lexer::TokenTree::FloatLiteral($crate::lexer::FloatLiteral { $($tt)+ })
        };
        ([ $($tt:tt)|+ ]) => {
            $($crate::lexer::pat::float!($tt))|+
        };
    }
    macro_rules! str {
        () => {
            $crate::lexer::TokenTree::StrLiteral(_)
        };
        (_) => {
            $crate::lexer::pat::str!()
        };
        ($text:literal) => {
            $crate::lexer::TokenTree::StrLiteral($crate::lexer::StrLiteral { value: $text, .. })
        };
        ({ $($tt:tt)+ }) => {
            $crate::lexer::TokenTree::StrLiteral($crate::lexer::StrLiteral { $($tt)+ })
        };
        ([ $($tt:tt)|+ ]) => {
            $($crate::lexer::pat::str!($tt))|+
        };
    }
    macro_rules! group {
        () => {
            $crate::lexer::TokenTree::Group(_)
        };
        (_) => {
            $crate::lexer::pat::group!()
        };
        ($delim:ident) => {
            $crate::lexer::TokenTree::Group($crate::lexer::Group { delim: $crate::lexer::Delimiter::$delim, .. })
        };
        ({ $($tt:tt)+ }) => {
            $crate::lexer::TokenTree::Group($crate::lexer::Group { $($tt)+ })
        };
        ([ $($tt:tt)|+ ]) => {
            $($crate::lexer::pat::group!($tt))|+
        };
    }
    macro_rules! token {
        (_) => {
            _
        };
        (new_line $(: $tt:tt)?) => {
            $crate::lexer::pat::new_line!($($tt)?)
        };
        (punct $(: $tt:tt)?) => {
            $crate::lexer::pat::punct!($($tt)?)
        };
        (ident $(: $tt:tt)?) => {
            $crate::lexer::pat::ident!($($tt)?)
        };
        (int $(: $tt:tt)?) => {
            $crate::lexer::pat::int!($($tt)?)
        };
        (float $(: $tt:tt)?) => {
            $crate::lexer::pat::float!($($tt)?)
        };
        (str $(: $tt:tt)?) => {
            $crate::lexer::pat::str!($($tt)?)
        };
        (group $(: $tt:tt)?) => {
            $crate::lexer::pat::group!($($tt)?)
        };
        (multi: [ $($name:ident $(: $tt:tt)?)|+ ]) => {
            $($crate::lexer::pat::token!($name $(: $tt)?))|+
        };
        (pat: ($pat:pat)) => {
            $pat
        };
    }
    macro_rules! tokens {
        ($($type:ident $(:$token:tt)?,)*) => {
            $crate::lexer::pat::tokens![$($type $(:$token)?),*]
        };
        ($($type:ident $(:$token:tt)?),*) => {
            &[$($crate::lexer::pat::token![$type $(:$token)?]),*]
        };
    }
    macro_rules! puncts {
        ($($punct:tt,)*) => {
            $crate::lexer::pat::puncts![$($punct),*]
        };
        ($($punct:tt),*) => {
            &[$($crate::lexer::pat::punct![$punct]),*]
        };
    }
    macro_rules! new_lines {
        ($($punct:tt,)*) => {
            $crate::lexer::pat::new_lines![$($punct),*]
        };
        ($($punct:tt),*) => {
            &[$($crate::lexer::pat::new_line![$punct]),*]
        };
    }
    pub(crate) use {
        float, group, ident, int, new_line, new_lines, punct, puncts, str, token, tokens,
    };
}

pub enum TokenTree<'a> {
    NewLine(NewLine<'a>),
    Ident(Ident<'a>),
    Punct(Punct<'a>),
    Group(Group<'a>),
    StrLiteral(StrLiteral<'a>),
    IntLiteral(IntLiteral<'a>),
    FloatLiteral(FloatLiteral<'a>),
}

impl<'a> fmt::Debug for TokenTree<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Tt::")?;
        match self {
            Self::NewLine(x) => fmt::Debug::fmt(x, f),
            Self::Ident(x) => fmt::Debug::fmt(x, f),
            Self::Punct(x) => fmt::Debug::fmt(x, f),
            Self::Group(x) => fmt::Debug::fmt(x, f),
            Self::StrLiteral(x) => fmt::Debug::fmt(x, f),
            Self::IntLiteral(x) => fmt::Debug::fmt(x, f),
            Self::FloatLiteral(x) => fmt::Debug::fmt(x, f),
        }
    }
}

#[derive(Debug)]
enum UngroupedToken<'a> {
    NewLine(NewLine<'a>),
    Ident(Ident<'a>),
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
                "Found closing delimiter at '{span}' without a matching opening delimiter"
            ),
            Self::MissingCloseDelim {
                open_span,
                close_span,
            } => write!(
                f,
                "Missing closing delimiter at '{close_span}' for opening at '{open_span}'"
            ),
            Self::NonMatchingDelim {
                open_span,
                close_span,
            } => {
                write!(
                    f,
                    "Non-matching delimiters at '{open_span}' and at '{close_span}'"
                )
            }
            Self::UnsupportedNumberBase { span, base } => {
                write!(f, "Unsupported number base `{base}` at '{span}'")
            }
            Self::NonDecimalFloat { span, base } => {
                write!(f, "Non decimal float with base {base} at '{span}'")
            }
            Self::UnclosedString { span } => {
                write!(f, "Unclosed string at '{span}'")
            }
            Self::ParseIntError { span, ref err } => {
                write!(f, "Parse int error at '{span}': {err}")
            }
            Self::OutOfRangeAsciiEscape { span } => {
                write!(
                    f,
                    "Out of range hex escape at '{span}', must be in range [\\x00-\\x7f]"
                )
            }
            Self::Expected { span, s } => {
                write!(f, "Expected `{s}` at '{span}'")
            }
            Self::InvalidUnicodeEscape { span } => {
                write!(f, "Invalid unicode escape at '{span}'")
            }
            Self::UnsupportedEscapeChar { span, ch } => {
                write!(f, "Unsupported string escape character `{ch}` at '{span}'")
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
                (Some(TokenTree::Punct(Punct { ch: Punct::END, .. })), TokenTree::NewLine(_))
                    if 2 <= tokens.len()
                        && matches!(
                            tokens[tokens.len() - 2],
                            TokenTree::Punct(Punct { ch: b'\\', .. })
                        ) =>
                {
                    tokens.pop();
                    tokens.pop();
                }
                (Some(TokenTree::Punct(_)), TokenTree::Punct(Punct { ch: b';', span })) => {
                    tokens.push(TokenTree::Punct(Punct::end(Span {
                        end: span.start,
                        ..span
                    })));
                    tokens.push(TokenTree::NewLine(NewLine { span }));
                }
                (_, TokenTree::Punct(Punct { ch: b';', span })) => {
                    tokens.push(TokenTree::NewLine(NewLine { span }));
                }
                (
                    Some(TokenTree::Punct(_)),
                    token @ TokenTree::Punct(Punct { ch: Punct::END, .. }),
                ) => {
                    tokens.push(token);
                }
                (_, TokenTree::Punct(Punct { ch: Punct::END, .. })) => {}
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
            Self::Punct(Punct { span, .. }) => span,
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
                UngroupedToken::PunctString(PunctString { puncts, span }) => {
                    for (i, ch) in puncts.into_iter().enumerate() {
                        group.tokens.push(Self::Punct(Punct {
                            ch,
                            span: Span::new(span.path, span.start + i, span.start + i + 1),
                        }));
                    }
                    debug_assert_eq!(
                        group.tokens.last().unwrap().span(),
                        Span::new(span.path, span.end - 1, span.end)
                    );
                    group.tokens.push(Self::Punct(Punct::end(Span::new(
                        span.path, span.end, span.end,
                    ))));
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
                UngroupedToken::PunctString(PunctString { puncts, span }) => {
                    for (i, ch) in puncts.into_iter().enumerate() {
                        group.tokens.push(Self::Punct(Punct {
                            ch,
                            span: Span::new(span.path, span.start + i, span.start + i + 1),
                        }));
                    }
                    debug_assert_eq!(
                        group.tokens.last().unwrap().span(),
                        Span::new(span.path, span.end - 1, span.end)
                    );
                    group.tokens.push(Self::Punct(Punct::end(Span::new(
                        span.path, span.end, span.end,
                    ))));
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
        let mut num_str = String::new();
        match s.as_bytes()[*ptr] {
            b @ (b'+' | b'-') => {
                num_str.push(b as char);
                *ptr += 1;
            }
            _ => {}
        }
        let mut underscore_allowed = false;
        let radix = if s.as_bytes()[*ptr] == b'0' {
            num_str.push('0');
            *ptr += 2;
            match s.as_bytes()[*ptr - 1] {
                b'x' => 16,
                b'o' => 8,
                b'b' => 2,
                b'0'..=b'9' => {
                    underscore_allowed = true;
                    10
                }
                b'_' => {
                    underscore_allowed = false;
                    10
                }
                ch if ch.is_ascii_whitespace()
                    || PUNCT_CHARS.contains(&ch)
                    || SPECIAL_CHARS.contains(&ch) =>
                {
                    *ptr -= 1;
                    return Ok(NumberLiteral::Int(IntLiteral {
                        value: i64::from_str_radix(&num_str, 10).unwrap(),
                        span: Span::new(path, start, *ptr),
                    }));
                }
                _ => {
                    return Err(LexingError::UnsupportedNumberBase {
                        span: Span::new(
                            path,
                            *ptr - 1,
                            s.get(*ptr - 1..)
                                .unwrap()
                                .char_indices()
                                .nth(1)
                                .map(|(i, _)| *ptr - 1 + i)
                                .unwrap(),
                        ),
                        base: s.get(*ptr - 1..).unwrap().chars().next().unwrap(),
                    });
                }
            }
        } else {
            10
        };
        loop {
            let ch = s.as_bytes()[*ptr];
            match ch {
                _ if !ch.is_ascii() => break,
                b'_' if underscore_allowed => {
                    underscore_allowed = false;
                }
                _ if char::is_digit(ch as _, radix) => {
                    underscore_allowed = true;
                    num_str.push(ch as _);
                }
                _ => break,
            }
            *ptr += 1;
        }
        if b'.' == s.as_bytes()[*ptr] && {
            let ch = s.get(*ptr + 1..).unwrap().chars().next().unwrap();
            ch.is_ascii_digit()
                || ch.is_whitespace()
                // || ch != '.' && PUNCT_CHARS.contains(&(ch as u8))
                || SPECIAL_CHARS.contains(&(ch as u8))
        } {
            num_str.push('.');
            *ptr += 1;
            if radix != 10 {
                // panic!("Non decimal floats not supported");
                return Err(LexingError::NonDecimalFloat {
                    span: Span::new(path, start, *ptr),
                    base: radix as _,
                });
            }
            underscore_allowed = false;
            loop {
                let ch = s.as_bytes()[*ptr];
                match ch {
                    _ if !ch.is_ascii() => break,
                    b'_' if underscore_allowed => {
                        underscore_allowed = false;
                    }
                    _ if char::is_digit(ch as _, radix) => {
                        underscore_allowed = true;
                        num_str.push(ch as _);
                    }
                    _ => break,
                }
                *ptr += 1;
            }
            Ok(NumberLiteral::Float(FloatLiteral {
                value: num_str.parse().unwrap(),
                span: Span::new(path, start, *ptr),
            }))
        } else {
            Ok(NumberLiteral::Int(IntLiteral {
                value: i64::from_str_radix(&num_str, radix).unwrap(),
                span: Span::new(path, start, *ptr),
            }))
        }
        // let start = *ptr;
        // let sign;
        // (sign, *ptr) = match s.as_bytes()[start] {
        //     b'+' => (1, start + 1),
        //     b'-' => (-1, start + 1),
        //     _ => (1, start),
        // };
        // let post_sign_start = *ptr;

        // let mut underscore_sep = false;
        // let radix = if s.as_bytes()[post_sign_start] == b'0' {
        //     *ptr += 2;
        //     match s.as_bytes()[post_sign_start + 1] {
        //         b'x' => 16,
        //         b'o' => 8,
        //         b'b' => 2,
        //         b'0'..=b'9' => 10,
        //         b'_' => {
        //             underscore_sep = true;
        //             10
        //         }
        //         ch if ch.is_ascii_whitespace()
        //             || PUNCT_CHARS.contains(&ch)
        //             || SPECIAL_CHARS.contains(&ch) =>
        //         {
        //             *ptr -= 1;
        //             10
        //         }
        //         _ => {
        //             return Err(LexingError::UnsupportedNumberBase {
        //                 span: Span::new(
        //                     path,
        //                     post_sign_start + 1,
        //                     s.get(post_sign_start + 1..)
        //                         .unwrap()
        //                         .char_indices()
        //                         .nth(1)
        //                         .map(|(i, _)| post_sign_start + 1 + i)
        //                         .unwrap(),
        //                 ),
        //                 base: s
        //                     .get(post_sign_start + 1..)
        //                     .unwrap()
        //                     .chars()
        //                     .next()
        //                     .unwrap(),
        //             });
        //             // panic!("Unsupported number base");
        //         }
        //     }
        // } else {
        //     10
        // };
        // loop {
        //     let ch = s.as_bytes()[*ptr];
        //     if (ch != b'_' || underscore_sep) && char::is_digit(ch as _, radix) {
        //         *ptr -= underscore_sep as usize;
        //         break;
        //     }
        //     underscore_sep = ch == b'_';
        //     *ptr += 1;
        // }

        // if b'.' == s.as_bytes()[*ptr] && {
        //     let ch = s.get(*ptr + 1..).unwrap().chars().next().unwrap();
        //     ch.is_ascii_digit()
        //         || ch.is_whitespace()
        //         || Delimiter::from(ch) != Delimiter::None
        //         || ch != '.' && PUNCT_CHARS.contains(&(ch as u8))
        //         || SPECIAL_CHARS.contains(&(ch as u8))
        // } {
        //     *ptr += 1;
        //     if radix != 10 {
        //         // panic!("Non decimal floats not supported");
        //         return Err(LexingError::NonDecimalFloat {
        //             span: Span::new(path, start, *ptr),
        //             base: radix as _,
        //         });
        //     }
        //     while s.as_bytes()[*ptr].is_ascii_digit() {
        //         *ptr += 1;
        //     }
        //     Ok(NumberLiteral::Float(FloatLiteral {
        //         value: { s.get(start..*ptr).unwrap().parse().unwrap() },
        //         span: Span::new(path, start, *ptr),
        //     }))
        // } else {
        //     Ok(NumberLiteral::Int(IntLiteral {
        //         value: {
        //             sign * i64::from_str_radix(
        //                 s.get(post_sign_start + if radix == 10 { 0 } else { 2 }..*ptr)
        //                     .unwrap(),
        //                 radix,
        //             )
        //             .unwrap()
        //         },
        //         span: Span::new(path, start, *ptr),
        //     }))
        // }
    }

    fn next_punct_str(
        ptr: &mut usize,
        path: &'a Path,
        s: &str,
    ) -> Result<PunctString<'a>, LexingError<'a>> {
        let start = *ptr;
        while PUNCT_CHARS.contains(&s.as_bytes()[*ptr]) {
            *ptr += 1;
        }
        Ok(PunctString {
            puncts: s.as_bytes()[start..*ptr].to_owned(),
            span: Span::new(path, start, *ptr),
        })
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
                            s.get(start + i + 3..start + 1 + j + ch2.len_utf8())
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
                                span: Span::new(path, start + i + 3, start + i + 5),
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
            ch if PUNCT_CHARS.contains(&(ch as u8)) => Ok(Some(UngroupedToken::PunctString(
                Self::next_punct_str(ptr, path, s)?,
            ))),
            _ => Ok(Some(UngroupedToken::Ident(Self::next_ident(ptr, path, s)?))),
        }
    }

    fn is_stop_char(ch: char) -> bool {
        ch.is_whitespace()
            || PUNCT_CHARS.contains(&(ch as u8))
            || SPECIAL_CHARS.contains(&(ch as u8))
    }
}
