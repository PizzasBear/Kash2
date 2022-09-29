use crate::{
    container::{self, Container, ContainerMask},
    lexer::{self, Delimiter},
    parser_old::Tokens,
    Span, Spanned,
};
use std::{error::Error, fmt};

pub const KEYWORDS: &[&str] = &[
    "let", "mut", "if", "else", "for", "while", "loop", "async", "await", "fn", "class", "struct",
    "trait", "impl", "false", "true", "_",
];

pub enum ParseError<'a> {
    Unexpected {
        name: &'static str,
        span: Span<'a>,
    },
    ExpectedDyn {
        name: String,
        span: Span<'a>,
    },
    ExpectedMatchParser {
        parser: MatchParser<'static, container::Owned>,
        span: Span<'a>,
    },
    Expected {
        name: &'static str,
        span: Span<'a>,
    },
    Multi(Vec<Self>),
}

impl<'a> ParseError<'a> {
    pub fn spans(&self) -> Vec<Span<'a>> {
        match *self {
            Self::Expected { span, .. } => vec![span],
            Self::ExpectedDyn { span, .. } => vec![span],
            Self::ExpectedMatchParser { span, .. } => vec![span],
            Self::Unexpected { span, .. } => vec![span],
            Self::Multi(ref errors) => errors.iter().map(Self::spans).flatten().collect(),
        }
    }
}

impl<'a> Spanned<'a> for ParseError<'a> {
    fn span(&self) -> Span<'a> {
        match *self {
            Self::Expected { span, .. } => span,
            Self::ExpectedDyn { span, .. } => span,
            Self::ExpectedMatchParser { span, .. } => span,
            Self::Unexpected { span, .. } => span,
            Self::Multi(ref errors) => errors
                .iter()
                .map(Spanned::span)
                .reduce(Span::union)
                .unwrap(),
        }
    }
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn write_match_parser<'a, C: crate::container::ContainerMask<'a>>(
            f: &mut fmt::Formatter<'_>,
            parser: &MatchParser<'a, C>,
        ) -> fmt::Result {
            match *parser {
                MatchParser::Empty => write!(f, "nothing"),
                MatchParser::SkipWhitespace { skip_newline } => write!(
                    f,
                    "skip whitespace {}",
                    if skip_newline {
                        "(including newline)"
                    } else {
                        "(excluding newline)"
                    }
                ),
                MatchParser::Newline => write!(f, "a newline"),
                MatchParser::Punct(punct) => write!(
                    f,
                    "the punct {}",
                    std::str::from_utf8(&*punct).expect("found out that punct isn't utf-8")
                ),
                MatchParser::Int(x) => write!(f, "the integer {x}"),
                MatchParser::Float(x) => {
                    write!(f, "the floating point number {x}")
                }
                MatchParser::Str(s) => write!(f, "the string {:?}", &*s),
                MatchParser::Keyword(kw) => write!(f, "the keyword `{}`", &*kw),
                MatchParser::Group(delim, parser) => {
                    write!(f, "a group deliminated by {delim:?} containing ")?;
                    write_match_parser(f, &*parser)
                }
                MatchParser::Parsers(parsers) => {
                    let parsers = &*parsers;
                    if let Some((head, tail)) = parsers.split_first() {
                        write!(f, "a sequence of [")?;
                        write_match_parser(f, head)?;
                        for parser in tail {
                            write!(f, ", ")?;
                            write_match_parser(f, parser)?;
                        }
                        write!(f, "]")
                    } else {
                        write!(f, "an empty sequence")
                    }
                }
            }
        }
        match *self {
            Self::Unexpected { name, span } => {
                write!(f, "Unexpected {name} at '{span}'")
            }
            Self::ExpectedDyn { ref name, span } => write!(f, "Expected {name} at '{span}'"),
            Self::Expected { name, span } => write!(f, "Expected {name} at '{span}'"),
            Self::ExpectedMatchParser { ref parser, span } => {
                write!(f, "Expected ")?;
                write_match_parser(f, parser)?;
                write!(f, " at '{span}'")
            }
            Self::Multi(ref errors) => {
                if let Some((head, tail)) = errors.split_first() {
                    write!(f, "Parsing has encountered multiple errors: {head}")?;
                    for err in tail.iter() {
                        write!(f, ", {err}")?;
                    }
                    Ok(())
                } else {
                    write!(f, "Parsing has encountered zero errors. I don't know why it crashed, makes you wonder ...")
                }
            }
        }
    }
}
impl<'a> fmt::Debug for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
impl<'a> Error for ParseError<'a> {}

pub trait Parser<'a> {
    type Output: Spanned<'a>;
    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>>;
}

impl<'a, 'b, T: Parser<'a>> Parser<'a> for &'b T {
    type Output = T::Output;
    #[inline]
    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        (**self).parse(tokens)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Repeat<Body, Sep> {
    pub body: Body,
    pub sep: Sep,
    pub allow_empty: bool,
    pub allow_trailing_sep: bool,
}

#[derive(Debug, Clone)]
pub struct RepeatValue<'a, Body, Sep> {
    pub body: Vec<Body>,
    pub sep: Vec<Sep>,
    pub span: Span<'a>,
}

impl<'a, Body, Sep> Spanned<'a> for RepeatValue<'a, Body, Sep> {
    #[inline]
    fn span(&self) -> Span<'a> {
        self.span
    }
}

impl<'a, Body, Sep> Parser<'a> for Repeat<Body, Sep>
where
    Body: Parser<'a>,
    Sep: Parser<'a>,
{
    type Output = RepeatValue<'a, Body::Output, Sep::Output>;

    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let mut inner_tokens = *tokens;
        let out = match self.body.parse(&mut inner_tokens) {
            Ok(first) => {
                let mut span = first.span();
                let mut body_values = vec![first];
                let mut sep_values = vec![];
                loop {
                    match self.sep.parse(&mut inner_tokens) {
                        Ok(value) => {
                            span.unite_with(value.span());
                            sep_values.push(value);
                        }
                        Err(_) => break,
                    }
                    match self.body.parse(&mut inner_tokens) {
                        Ok(value) => {
                            span.unite_with(value.span());
                            body_values.push(value);
                        }
                        Err(_) if self.allow_trailing_sep => break,
                        Err(err) => return Err(err),
                    }
                }
                RepeatValue {
                    body: body_values,
                    sep: sep_values,
                    span,
                }
            }
            Err(_) if self.allow_empty => RepeatValue {
                body: vec![],
                sep: vec![],
                span: inner_tokens.span().begining(),
            },
            Err(err) => return Err(err),
        };
        *tokens = inner_tokens;
        Ok(out)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct OneOf<'a, Id: Clone, Parser> {
    pub parsers: &'a [(Id, Parser)],
    pub name: &'static str,
}

pub struct OneOfValue<Id, T> {
    pub id: Id,
    pub value: T,
}

impl<'a, Id, T: Spanned<'a>> Spanned<'a> for OneOfValue<Id, T> {
    #[inline]
    fn span(&self) -> Span<'a> {
        self.value.span()
    }
}

impl<'a, 'b, Id: Clone, P: Parser<'a>> Parser<'a> for OneOf<'b, Id, P> {
    type Output = OneOfValue<Id, P::Output>;

    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let mut inner_tokens = *tokens;
        for (id, parser) in self.parsers.iter() {
            if let Ok(value) = parser.parse(&mut inner_tokens) {
                *tokens = inner_tokens;
                return Ok(OneOfValue {
                    id: id.clone(),
                    value,
                });
            }
        }
        Err(ParseError::Expected {
            name: self.name,
            span: inner_tokens.span(),
        })
    }
}

macro_rules! def_one_of {
    ($name:ident, $out_name:ident: $($n:tt $type:ident),+) => {
        #[derive(Debug, Clone)]
        pub struct $name<$($type),+> {
            pub name: &'static str,
            pub parsers: ($($type),+),
        }
        #[derive(Debug, Clone)]
        pub enum $out_name<$($type),+> {
            $($type($type),)+
        }
        impl<'a, $($type: Spanned<'a>),+> Spanned<'a> for $out_name<$($type),+> {
            fn span(&self) -> Span<'a> {
                match self {
                    $(Self::$type(x) => x.span(),)+
                }
            }
        }
        impl<'a, $($type: Parser<'a>),+> Parser<'a> for $name<$($type),+> {
            type Output = $out_name<$($type::Output),+>;
            fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
                let mut inner_tokens = *tokens;
                let out = $(
                    if let Ok(value) = self.parsers.$n.parse(&mut inner_tokens) {
                        $out_name::$type(value)
                    } else
                )+ {
                    return Err(ParseError::Expected { name: self.name, span: inner_tokens.span() });
                };
                *tokens = inner_tokens;
                Ok(out)
            }
        }
    };
}

def_one_of!(OneOf2,  OneOfValue2:  0 T0, 1 T1);
def_one_of!(OneOf3,  OneOfValue3:  0 T0, 1 T1, 2 T2);
def_one_of!(OneOf4,  OneOfValue4:  0 T0, 1 T1, 2 T2, 3 T3);
def_one_of!(OneOf5,  OneOfValue5:  0 T0, 1 T1, 2 T2, 3 T3, 4 T4);
def_one_of!(OneOf6,  OneOfValue6:  0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5);
def_one_of!(OneOf7,  OneOfValue7:  0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6);
def_one_of!(OneOf8,  OneOfValue8:  0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7);
def_one_of!(OneOf9,  OneOfValue9:  0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8);
def_one_of!(OneOf10, OneOfValue10: 0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9);
def_one_of!(OneOf11, OneOfValue11: 0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10);
def_one_of!(OneOf12, OneOfValue12: 0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10, 11 T11);
// def_one_of!(OneOf13, OneOfValue13: 0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10, 11 T11, 12 T12);
// def_one_of!(OneOf14, OneOfValue14: 0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10, 11 T11, 12 T12, 13 T13);
// def_one_of!(OneOf15, OneOfValue15: 0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10, 11 T11, 12 T12, 13 T13, 14 T14);
// def_one_of!(OneOf16, OneOfValue16: 0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10, 11 T11, 12 T12, 13 T13, 14 T14, 15 T15);

macro_rules! def_chained {
    ($name:ident, $out_name:ident($last:tt): $($n:tt $type:ident),+) => {
        #[derive(Debug, Clone)]
        pub struct $name<$($type),+>($(pub $type),+);
        #[derive(Debug, Clone)]
        pub struct $out_name<$($type),+>($(pub $type),+);
        impl<'a, $($type: Spanned<'a>),+> Spanned<'a> for $out_name<$($type),+> {
            fn span(&self) -> Span<'a> {
                self.0.span().union(self.$last.span())
            }
        }
        impl<'a, $($type: Parser<'a>),+> Parser<'a> for $name<$($type),+> {
            type Output = $out_name<$($type::Output),+>;
            fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
                let mut inner_tokens = *tokens;
                let out = $out_name($(
                    self.$n.parse(&mut inner_tokens)?,
                )+);
                *tokens = inner_tokens;
                Ok(out)
            }
        }
    };
}

def_chained!(Chained2,  ChainedValue2  (1):  0 T0, 1 T1);
def_chained!(Chained3,  ChainedValue3  (2):  0 T0, 1 T1, 2 T2);
def_chained!(Chained4,  ChainedValue4  (3):  0 T0, 1 T1, 2 T2, 3 T3);
def_chained!(Chained5,  ChainedValue5  (4):  0 T0, 1 T1, 2 T2, 3 T3, 4 T4);
def_chained!(Chained6,  ChainedValue6  (5):  0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5);
def_chained!(Chained7,  ChainedValue7  (6):  0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6);
def_chained!(Chained8,  ChainedValue8  (7):  0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7);
def_chained!(Chained9,  ChainedValue9  (8):  0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8);
def_chained!(Chained10, ChainedValue10 (9):  0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9);
def_chained!(Chained11, ChainedValue11 (10): 0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10);
def_chained!(Chained12, ChainedValue12 (11): 0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10, 11 T11);
def_chained!(Chained13, ChainedValue13 (12): 0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10, 11 T11, 12 T12);
def_chained!(Chained14, ChainedValue14 (13): 0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10, 11 T11, 12 T12, 13 T13);
def_chained!(Chained15, ChainedValue15 (14): 0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10, 11 T11, 12 T12, 13 T13, 14 T14);
def_chained!(Chained16, ChainedValue16 (15): 0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10, 11 T11, 12 T12, 13 T13, 14 T14, 15 T15);

pub struct Optional<T>(pub T);
pub struct OptionalValue<'a, T> {
    pub span: Span<'a>,
    pub value: Option<T>,
}
impl<'a, T> Spanned<'a> for OptionalValue<'a, T> {
    #[inline]
    fn span(&self) -> Span<'a> {
        self.span
    }
}
impl<'a, T: Parser<'a>> Parser<'a> for Optional<T> {
    type Output = OptionalValue<'a, T::Output>;
    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let value = self.0.parse(tokens).ok();
        Ok(OptionalValue {
            span: match value {
                Some(ref x) => x.span(),
                None => tokens.span().begining(),
            },
            value,
        })
    }
}

macro_rules! def_multiparser {
    (0 $name:ident, $out_name:ident: $($type:ident),+) => {
        #[derive(Debug, Clone)]
        pub enum $name<$($type),+> { $($type($type),)+ }
        #[derive(Debug, Clone)]
        pub enum $out_name<$($type),+> { $($type($type),)+ }
        impl<'a, $($type: Spanned<'a>),+> Spanned<'a> for $out_name<$($type),+> {
            fn span(&self) -> Span<'a> {
                match self {
                    $(Self::$type(value) => value.span(),)+
                }
            }
        }
        impl<'a, $($type: Parser<'a>),+> Parser<'a> for $name<$($type),+> {
            type Output = $out_name<$($type::Output),+>;
            fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
                Ok(match self {
                    $(Self::$type(parser) => Self::Output::$type(parser.parse(tokens)?),)+
                })
            }
        }
    };
}
def_multiparser!(0 MutliParser2,  MutliParserValue2:  T0, T1);
def_multiparser!(0 MutliParser3,  MutliParserValue3:  T0, T1, T2);
def_multiparser!(0 MutliParser4,  MutliParserValue4:  T0, T1, T2, T3);
def_multiparser!(0 MutliParser5,  MutliParserValue5:  T0, T1, T2, T3, T4);
def_multiparser!(0 MutliParser6,  MutliParserValue6:  T0, T1, T2, T3, T4, T5);
def_multiparser!(0 MutliParser7,  MutliParserValue7:  T0, T1, T2, T3, T4, T5, T6);
def_multiparser!(0 MutliParser8,  MutliParserValue8:  T0, T1, T2, T3, T4, T5, T6, T7);
def_multiparser!(0 MutliParser9,  MutliParserValue9:  T0, T1, T2, T3, T4, T5, T6, T7, T8);
def_multiparser!(0 MutliParser10, MutliParserValue10: T0, T1, T2, T3, T4, T5, T6, T7, T8, T9);
def_multiparser!(0 MutliParser11, MutliParserValue11: T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);
def_multiparser!(0 MutliParser12, MutliParserValue12: T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
def_multiparser!(0 MultiParser13, MultiParserValue13: T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);
def_multiparser!(0 MultiParser14, MultiParserValue14: T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13);
def_multiparser!(0 MultiParser15, MultiParserValue15: T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14);
def_multiparser!(0 MultiParser16, MultiParserValue16: T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15);

#[derive(Debug, Clone)]
pub enum MatchParser<'a, M: ContainerMask<'a>> {
    Empty,
    Newline,
    SkipWhitespace { skip_newline: bool },
    Parsers(Container<'a, M, [Self]>),
    Punct(Container<'a, M, [u8]>),
    Keyword(Container<'a, M, str>),
    Str(Container<'a, M, str>),
    Int(u64),
    Float(f64),
    Group(Delimiter, Container<'a, M, Self>),
}

impl<'a, M: ContainerMask<'a>> MatchParser<'a, M> {
    pub fn into_owned(&self) -> MatchParser<'static, container::Owned> {
        match *self {
            Self::Empty => MatchParser::Empty,
            Self::Newline => MatchParser::Newline,
            Self::SkipWhitespace { skip_newline } => MatchParser::SkipWhitespace { skip_newline },
            Self::Parsers(parsers) => MatchParser::Parsers(container::make_owned(
                parsers.iter().map(|x| x.into_owned()).collect(),
            )),
            Self::Punct(punct) => MatchParser::Punct(punct.into_owned()),
            Self::Keyword(keyword) => MatchParser::Keyword(keyword.into_owned()),
            Self::Str(s) => MatchParser::Str(s.into_owned()),
            Self::Int(x) => MatchParser::Int(x),
            Self::Float(x) => MatchParser::Float(x),
            Self::Group(delim, group) => {
                MatchParser::Group(delim, container::make_owned((*group).into_owned()))
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MatchValue<'a>(Span<'a>);

impl<'a> Spanned<'a> for MatchValue<'a> {
    #[inline]
    fn span(&self) -> Span<'a> {
        self.0
    }
}

impl<'a, C: ContainerMask<'a>> Parser<'a> for MatchParser<'a, C> {
    type Output = MatchValue<'a>;

    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let mut inner_tokens = *tokens;
        let out = match *self {
            MatchParser::Empty => MatchValue(inner_tokens.span().begining()),
            MatchParser::SkipWhitespace { skip_newline } => {
                let span = inner_tokens.span().begining();
                inner_tokens.skip_whitespace(skip_newline);
                MatchValue(span.union(inner_tokens.span().begining()))
            }
            MatchParser::Newline => match inner_tokens.pop_first() {
                Some(&lexer::pat::newline!({ span })) => MatchValue(span),
                Some(token) => {
                    return Err(ParseError::ExpectedMatchParser {
                        span: token.span().begining(),
                        parser: self.into(),
                    })
                }
                None => {
                    return Err(ParseError::ExpectedMatchParser {
                        span: inner_tokens.span().begining(),
                        parser: self.into_owned(),
                    })
                }
            },
            MatchParser::Punct(punct) => {
                let mut mod_span = inner_tokens
                    .first()
                    .map(|x| x.span())
                    .unwrap_or(inner_tokens.span())
                    .begining();
                for &ch in &*punct {
                    match inner_tokens.pop_first() {
                        Some(&lexer::pat::punct!({ ch: tk_ch, span })) if ch == tk_ch => {
                            mod_span.unite_with(span);
                        }
                        _ => {
                            return Err(ParseError::ExpectedMatchParser {
                                parser: self.into_owned(),
                                span: mod_span.begining(),
                            })
                        }
                    }
                }
                inner_tokens.skip_whitespace(false);
                MatchValue(mod_span)
            }
            MatchParser::Group(delim, parser) => match inner_tokens.pop_first() {
                Some(&lexer::pat::group!((ref group @ { delim: tk_delim, span, .. })))
                    if tk_delim == delim =>
                {
                    let mut tokens = Tokens::new(group);
                    match parser.parse(&mut tokens) {
                        Ok(_) if tokens.is_empty() => MatchValue(span),
                        Ok(_) => {
                            return Err(ParseError::Unexpected {
                                name: "token",
                                span: tokens.span().begining(),
                            })
                        }
                        Err(err) => return Err(err),
                    }
                }
                Some(token) => {
                    return Err(ParseError::ExpectedMatchParser {
                        span: token.span().begining(),
                        parser: self.into_owned(),
                    })
                }
                None => {
                    return Err(ParseError::ExpectedMatchParser {
                        span: inner_tokens.span().begining(),
                        parser: self.into_owned(),
                    })
                }
            },
            MatchParser::Keyword(kw) => match inner_tokens.pop_first() {
                Some(&lexer::pat::ident!({ ref name, span })) if name == &*kw => MatchValue(span),
                Some(token) => {
                    return Err(ParseError::ExpectedMatchParser {
                        span: token.span().begining(),
                        parser: self.into_owned(),
                    })
                }
                None => {
                    return Err(ParseError::ExpectedMatchParser {
                        span: inner_tokens.span().begining(),
                        parser: self.into_owned(),
                    })
                }
            },
            MatchParser::Str(s) => match inner_tokens.pop_first() {
                Some(&lexer::pat::str!({ ref value, span })) if value == &*s => MatchValue(span),
                Some(token) => {
                    return Err(ParseError::ExpectedMatchParser {
                        span: token.span().begining(),
                        parser: self.into_owned(),
                    })
                }
                None => {
                    return Err(ParseError::ExpectedMatchParser {
                        span: inner_tokens.span().begining(),
                        parser: self.into_owned(),
                    })
                }
            },
            MatchParser::Int(x) => match inner_tokens.pop_first() {
                Some(&lexer::pat::int!({ value, span })) if value == x => MatchValue(span),
                Some(token) => {
                    return Err(ParseError::ExpectedMatchParser {
                        span: token.span().begining(),
                        parser: self.into_owned(),
                    })
                }
                None => {
                    return Err(ParseError::ExpectedMatchParser {
                        span: inner_tokens.span().begining(),
                        parser: self.into_owned(),
                    })
                }
            },
            MatchParser::Float(x) => match inner_tokens.pop_first() {
                Some(&lexer::pat::float!({ value, span })) if value == x => MatchValue(span),
                Some(token) => {
                    return Err(ParseError::ExpectedMatchParser {
                        span: token.span().begining(),
                        parser: self.into_owned(),
                    })
                }
                None => {
                    return Err(ParseError::ExpectedMatchParser {
                        span: inner_tokens.span().begining(),
                        parser: self.into_owned(),
                    })
                }
            },
            MatchParser::Parsers(matches) => {
                if let Some((head, tail)) = matches.split_first() {
                    let mut span = head.parse(&mut inner_tokens)?.span();
                    for bmatch in tail {
                        span.unite_with(bmatch.parse(&mut inner_tokens)?.span());
                    }
                    MatchValue(span)
                } else {
                    MatchValue(inner_tokens.span().begining())
                }
            }
        };
        *tokens = inner_tokens;
        Ok(out)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IdentParser;

#[derive(Debug, Clone)]
pub struct IdentValue<'a> {
    pub name: String,
    pub span: Span<'a>,
}

impl<'a> Spanned<'a> for IdentValue<'a> {
    #[inline]
    fn span(&self) -> Span<'a> {
        self.span
    }
}

impl<'a> Parser<'a> for IdentParser {
    type Output = IdentValue<'a>;

    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let mut inner_tokens = *tokens;
        let out = match inner_tokens.pop_first() {
            Some(&lexer::pat::ident!({ ref name, span })) => {
                if KEYWORDS.contains(&name.as_str()) {
                    return Err(ParseError::Unexpected {
                        name: "keyword",
                        span,
                    });
                } else {
                    IdentValue {
                        name: name.clone(),
                        span,
                    }
                }
            }
            token => {
                return Err(ParseError::Expected {
                    name: "an identifier",
                    span: match token {
                        Some(token) => token.span(),
                        None => inner_tokens.span(),
                    }
                    .begining(),
                })
            }
        };
        *tokens = inner_tokens;
        Ok(out)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StrParser;

#[derive(Debug, Clone)]
pub struct StrValue<'a> {
    pub value: String,
    pub span: Span<'a>,
}

impl<'a> Spanned<'a> for StrValue<'a> {
    #[inline]
    fn span(&self) -> Span<'a> {
        self.span
    }
}

impl<'a> Parser<'a> for StrParser {
    type Output = StrValue<'a>;

    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let mut inner_tokens = *tokens;
        let out = match inner_tokens.pop_first() {
            Some(&lexer::pat::str!({ ref value, span })) => StrValue {
                value: value.clone(),
                span,
            },
            token => {
                return Err(ParseError::Expected {
                    name: "a string",
                    span: match token {
                        Some(token) => token.span(),
                        None => inner_tokens.span(),
                    }
                    .begining(),
                })
            }
        };
        *tokens = inner_tokens;
        Ok(out)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IntParser;

#[derive(Debug, Clone, Copy)]
pub struct IntValue<'a> {
    pub value: u64,
    pub span: Span<'a>,
}

impl<'a> Spanned<'a> for IntValue<'a> {
    #[inline]
    fn span(&self) -> Span<'a> {
        self.span
    }
}

impl<'a> Parser<'a> for IntParser {
    type Output = IntValue<'a>;

    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let mut inner_tokens = *tokens;
        let out = match inner_tokens.pop_first() {
            Some(&lexer::pat::int!({ value, span })) => IntValue { value, span },
            token => {
                return Err(ParseError::Expected {
                    name: "an integer",
                    span: match token {
                        Some(token) => token.span(),
                        None => inner_tokens.span(),
                    }
                    .begining(),
                })
            }
        };
        *tokens = inner_tokens;
        Ok(out)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FloatParser;

#[derive(Debug, Clone, Copy)]
pub struct FloatValue<'a> {
    pub value: f64,
    pub span: Span<'a>,
}

impl<'a> Spanned<'a> for FloatValue<'a> {
    #[inline]
    fn span(&self) -> Span<'a> {
        self.span
    }
}

impl<'a> Parser<'a> for FloatParser {
    type Output = FloatValue<'a>;

    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let mut inner_tokens = *tokens;
        let out = match inner_tokens.pop_first() {
            Some(&lexer::pat::float!({ value, span })) => FloatValue { value, span },
            token => {
                return Err(ParseError::Expected {
                    name: "a floating point number",
                    span: match token {
                        Some(token) => token.span(),
                        None => inner_tokens.span(),
                    }
                    .begining(),
                })
            }
        };
        *tokens = inner_tokens;
        Ok(out)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct GroupParser<T> {
    pub delim: Delimiter,
    pub body: T,
}

#[derive(Debug, Clone, Copy)]
pub struct GroupValue<'a, T> {
    pub span: Span<'a>,
    pub body: T,
}

impl<'a, T> Spanned<'a> for GroupValue<'a, T> {
    #[inline]
    fn span(&self) -> Span<'a> {
        self.span
    }
}

impl<'a, T: Parser<'a>> Parser<'a> for GroupParser<T> {
    type Output = GroupValue<'a, T::Output>;

    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let mut inner_tokens = *tokens;
        let out = match inner_tokens.pop_first() {
            Some(&lexer::pat::group!((ref group @ { delim, span, .. }))) if delim == self.delim => {
                let mut tokens = Tokens::new(&group);
                GroupValue {
                    body: self.body.parse(&mut tokens)?,
                    span,
                }
            }
            token => {
                return Err(ParseError::Expected {
                    name: match self.delim {
                        Delimiter::Braces => "a group deliminated by Delim{}",
                        Delimiter::Brackets => "a group deliminated by Delim[]",
                        Delimiter::Parentheses => "a group deliminated by Delim()",
                        Delimiter::None => unimplemented!(),
                    },
                    span: match token {
                        Some(token) => token.span(),
                        None => inner_tokens.span(),
                    }
                    .begining(),
                })
            }
        };
        *tokens = inner_tokens;
        Ok(out)
    }
}

// pub struct Expr;
// impl Expr {
//     static PARSER: Repeat<Value, DuoOp> = Repeat {
//         body: Value,
//         sep: DuoOp,
//     };
// }
