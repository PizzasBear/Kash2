use std::{
    fmt, fs,
    ops::{self, Bound},
    path::{Path, PathBuf},
    sync::Arc,
};

use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_till, take_while, take_while1, take_while_m_n},
    character::complete::{anychar, char, one_of, satisfy},
    combinator::{cut, eof, fail, map_res, opt, peek, recognize, value},
    multi::{many0, many0_count},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult, Parser,
};
use nom_locate::LocatedSpan;
// use std::ops::{RangeFrom, RangeTo};

pub const SPECIAL_CHARS: &'static [u8] = br#"#()[]{}\"#;
pub const PUNCT_CHARS: &'static [u8] = br#"+-*/%,!&|^~@$<>=.,:;?'"`"#;
pub const NEWLINE_CHARS: &'static [u8] = b"\n\r\x08";

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct Pos {
    pub offset: usize,
    pub line: u32,
    pub column: usize,
}

impl PartialOrd for Pos {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Pos {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({}:{})", self.offset, self.line, self.column)
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Span {
    file_name: Arc<PathBuf>,
    start: Pos,
    end: Pos,
}

impl Span {
    pub fn file_name(&self) -> &Path {
        &self.file_name
    }
    pub fn arc_file_name(&self) -> &Arc<PathBuf> {
        &self.file_name
    }
    pub fn start(&self) -> &Pos {
        &self.start
    }
    pub fn end(&self) -> &Pos {
        &self.end
    }

    pub fn beginning(&self) -> Self {
        Self {
            file_name: self.file_name.clone(),
            start: self.start,
            end: self.start,
        }
    }
    pub fn ending(&self) -> Self {
        Self {
            file_name: self.file_name.clone(),
            start: self.end,
            end: self.end,
        }
    }
    pub fn join(&self, other: &Self) -> Option<Self> {
        if self.file_name == other.file_name
        // && self.start <= other.end && other.start <= self.end
        {
            Some(Self {
                file_name: self.file_name.clone(),
                start: self.start.min(other.start),
                end: self.end.max(other.end),
            })
        } else {
            None
        }
    }

    pub fn show(&self) -> ShowSpan {
        ShowSpan(self)
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "'{}:[{:?} - {:?}]'",
            self.file_name.display(),
            self.start,
            self.end,
        )
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "'{}:[{} - {}]'",
            self.file_name.display(),
            self.start,
            self.end,
        )
    }
}

pub struct ShowSpan<'a>(&'a Span);

impl<'a> fmt::Display for ShowSpan<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let file = fs::read_to_string(self.0.file_name()).map_err(|_| fmt::Error)?;

        writeln!(f, "at {}", self.0)?;

        let start = self.0.start();
        let end = self.0.end();

        let num_lines = end.line - start.line;
        match num_lines {
            0 => {
                let line = file[start.offset + 1 - start.column..]
                    .lines()
                    .next()
                    .ok_or(fmt::Error)?
                    .trim_end();

                writeln!(f, "{} | {line}", start.line)?;
                for _ in 0..itoa::Buffer::new().format(start.line).len() + 3 {
                    write!(f, " ")?;
                }
                for (mut i, _) in line.char_indices() {
                    i += 1;
                    if i < start.column {
                        write!(f, " ")?;
                    } else if i < end.column {
                        write!(f, "^")?;
                    } else {
                        break;
                    }
                }
                writeln!(f)?;
            }
            1..=10 => {
                let mut lines = (start.line..=end.line).zip(
                    file[start.offset + 1 - start.column..]
                        .lines()
                        .map(str::trim_end),
                );

                let line_num_len = itoa::Buffer::new().format(end.line).len();

                let (start_line_num, start_line) = lines.next().ok_or(fmt::Error)?;

                let mut start_line_chars = start_line.char_indices();
                for _ in 0..start_line_num + 3 {
                    write!(f, " ")?;
                }
                for _ in (&mut start_line_chars).take_while(|&(i, _)| i + 1 < start.column) {
                    write!(f, " ")?;
                }
                for _ in start_line_chars {
                    write!(f, "v")?;
                }
                writeln!(f)?;

                writeln!(f, "{start_line_num:>line_num_len$} | {start_line}")?;
                for (line_num, line) in lines {
                    writeln!(f, "{line_num:>line_num_len$} | {line}")?;
                }

                for _ in 0..line_num_len + 3 {
                    write!(f, " ")?;
                }
                'ss: {
                    for (i, _) in file[end.offset + 1 - end.column..]
                        .char_indices()
                        .take_while(|&(i, _)| i < end.column)
                    {
                        write!(f, "^")?;
                        if i + 1 == end.column {
                            writeln!(f)?;
                            break 'ss;
                        }
                    }
                    println!();
                    panic!("There's no line #{}", end.line);
                }
            }
            _ => todo!(),
        }

        Ok(())
    }
}

pub trait Spanned {
    fn span(&self) -> &Span;
}

impl Spanned for Span {
    fn span(&self) -> &Span {
        self
    }
}

fn is_special(ch: char) -> bool {
    SPECIAL_CHARS.contains(&(ch as u8))
}

fn is_punct(ch: char) -> bool {
    PUNCT_CHARS.contains(&(ch as u8))
}

fn is_newline(ch: char) -> bool {
    NEWLINE_CHARS.contains(&(ch as u8))
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum PunctSpacing {
    /// A `Punct` is not immediately followed by another `Punct`. E.g. `+` is `Alone` in `+ =`, `+ident` and `+()`.
    Alone,
    /// A `Punct` is immediately followed by another `Punct`. E.g. `+` is `Joint` in `+=` and `++`.
    ///
    /// Additionally, single quote `'` can join with identifiers to form lifetimes: `'ident`.
    Joint,
}

impl PunctSpacing {
    pub fn does_match(&self, s_match: &Self) -> bool {
        match s_match {
            Self::Alone => true,
            Self::Joint => self == &Self::Joint,
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct Punct {
    pub ch: u8,
    pub spacing: PunctSpacing,
}

impl fmt::Debug for Punct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Punct({:?} {:?})", self.ch as char, self.spacing)
    }
}

#[derive(Debug, Clone)]
pub struct StrSpanInfo {
    pub scope_name: &'static str,
    pub file_name: Arc<PathBuf>,
}
type StrSpan<'a> = LocatedSpan<&'a str, StrSpanInfo>;

macro_rules! name_span {
    // (&mut $input:ident, $name:expr, $block:block) => {{
    //     let top_scope = $input.extra.scope_name;
    //     $input.extra.scope_name = $name;
    //     let (mut $input, output) = $block?;
    //     $input.extra.scope_name = top_scope;
    //     Ok(($input, output))
    // }};
    ($input:ident($name:expr), $block:block) => {{
        let top_scope = $input.extra.scope_name;
        let mut $input = $input;
        $input.extra.scope_name = $name;
        let $input = $input;
        let (mut $input, output) = $block?;
        $input.extra.scope_name = top_scope;
        Ok(($input, output))
    }};
}

fn logging<'a, O, E, F>(
    name: &'static str,
    mut parser: F,
) -> impl FnMut(StrSpan<'a>) -> IResult<StrSpan<'a>, O, E>
where
    E: nom::error::ParseError<StrSpan<'a>> + fmt::Debug,
    F: nom::Parser<StrSpan<'a>, O, E>,
{
    move |input| name_span!(input(name), { parser.parse(input) })
}

fn sep<I, O1, O2, E, F, G, GenF>(
    mut parser: GenF,
    sep_parser: G,
) -> impl FnMut(I) -> IResult<I, usize, E>
where
    GenF: FnMut() -> F,
    I: nom::InputLength + Clone,
    E: nom::error::ParseError<I>,
    F: nom::Parser<I, O1, E>,
    G: nom::Parser<I, O2, E>,
{
    preceded(parser(), many0_count(pair(sep_parser, parser())))
}

fn recognize_decimal(input: StrSpan) -> IResult<StrSpan, StrSpan> {
    recognize(sep(
        || take_while1(|ch: char| ch.is_ascii_digit()),
        char('_'),
    ))(input)
}

pub fn parse_int_lit(input: StrSpan) -> IResult<StrSpan, IntLiteral> {
    spanned(alt((
        preceded(
            tag_no_case("0x"),
            map_res(
                recognize(sep(|| take_while1(nom::AsChar::is_hex_digit), char('_'))),
                |out: StrSpan| u64::from_str_radix(&out.replace('_', ""), 16),
            ),
        ),
        preceded(
            tag_no_case("0d"),
            map_res(
                recognize(sep(|| take_while1(|ch: char| ch.is_digit(12)), char('_'))),
                |out: StrSpan| u64::from_str_radix(&out.replace('_', ""), 12),
            ),
        ),
        preceded(
            tag_no_case("0o"),
            map_res(
                recognize(sep(|| take_while1(nom::AsChar::is_oct_digit), char('_'))),
                |out: StrSpan| u64::from_str_radix(&out.replace('_', ""), 8),
            ),
        ),
        preceded(
            tag_no_case("0s"),
            map_res(
                recognize(sep(|| take_while1(|ch: char| ch.is_digit(6)), char('_'))),
                |out: StrSpan| u64::from_str_radix(&out.replace('_', ""), 6),
            ),
        ),
        preceded(
            tag_no_case("0t"),
            map_res(
                recognize(sep(|| take_while1(|ch: char| ch.is_digit(3)), char('_'))),
                |out: StrSpan| u64::from_str_radix(&out.replace('_', ""), 3),
            ),
        ),
        preceded(
            tag_no_case("0b"),
            map_res(
                recognize(sep(|| take_while1(|ch: char| ch.is_digit(2)), char('_'))),
                |out: StrSpan| u64::from_str_radix(&out.replace('_', ""), 2),
            ),
        ),
        map_res(recognize_decimal, |out| {
            u64::from_str_radix(&out.replace('_', ""), 10)
        }),
    )))
    .map(|(span, value)| IntLiteral { value, span })
    .parse(input)
}

pub fn parse_float_lit(input: StrSpan) -> IResult<StrSpan, FloatLiteral> {
    spanned(map_res(
        alt((
            recognize(tuple((
                opt(recognize_decimal),
                char('.'),
                recognize_decimal,
                opt(tuple((char('e'), one_of("-+"), recognize_decimal))),
            ))),
            recognize(tuple((
                recognize_decimal,
                char('e'),
                one_of("-+"),
                recognize_decimal,
            ))),
            recognize(pair(recognize_decimal, char('.'))),
        )),
        |s| s.replace('_', "").parse(),
    ))
    .map(|(span, value)| FloatLiteral { value, span })
    .parse(input)
}

pub fn parse_escaped_char(input: StrSpan) -> IResult<StrSpan, char> {
    let hex_digit = || satisfy(|ch| ch.is_ascii_hexdigit());

    alt((
        preceded(
            char('\\'),
            alt((
                map_res(
                    preceded(char('x'), tuple((hex_digit(), hex_digit()))),
                    |(a, b)| {
                        let a = a.to_digit(16).unwrap();
                        let b = b.to_digit(16).unwrap();
                        char::from_u32(16 * a + b).ok_or("Invalid ASCII hex {a:x}{b:x}")
                    },
                ),
                map_res(
                    map_res(
                        delimited(
                            tag("u{"),
                            recognize(take_while_m_n(1, 6, |ch: char| ch.is_ascii_hexdigit())),
                            char('}'),
                        ),
                        |s: StrSpan| u32::from_str_radix(s.fragment(), 16),
                    ),
                    |n| char::from_u32(n).ok_or("Invalid unicode hex"),
                ),
                value('\n', char('n')),
                value('\r', char('r')),
                value('\t', char('t')),
                value('\x08', char('b')),
                value('\x0C', char('f')),
                value('\\', char('\\')),
                value('/', char('/')),
                value('"', char('"')),
            )),
        ),
        anychar,
    ))(input)
}

pub fn parse_char_lit(input: StrSpan) -> IResult<StrSpan, CharLiteral> {
    spanned(delimited(char('\''), parse_escaped_char, char('\'')))
        .map(|(span, value)| CharLiteral { value, span })
        .parse(input)
}

pub fn parse_str_lit(input: StrSpan) -> IResult<StrSpan, StrLiteral> {
    spanned(delimited(
        char('\"'),
        many0(parse_escaped_char).map(String::from_iter),
        char('\"'),
    ))
    .map(|(span, value)| StrLiteral { value, span })
    .parse(input)
}

// pub fn parse_bool_lit(input: StrSpan) -> IResult<StrSpan, bool> {
//     alt((value(true, tag("true")), value(false, tag("false"))))(input)
// }

#[derive(Clone, PartialEq)]
pub struct StrLiteral {
    pub value: String,
    pub span: Span,
}

#[derive(Clone, PartialEq)]
pub struct CharLiteral {
    pub value: char,
    pub span: Span,
}

#[derive(Clone, PartialEq)]
pub struct IntLiteral {
    pub value: u64,
    pub span: Span,
}

#[derive(Clone, PartialEq)]
pub struct FloatLiteral {
    pub value: f64,
    pub span: Span,
}

#[derive(Clone, PartialEq)]
pub enum Literal {
    Str(StrLiteral),
    Char(CharLiteral),
    Int(IntLiteral),
    Float(FloatLiteral),
}

impl fmt::Debug for StrLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} at {:?}", &self.value, &self.span)
    }
}

impl fmt::Debug for CharLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} at {:?}", &self.value, &self.span)
    }
}

impl fmt::Debug for IntLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} at {:?}", &self.value, &self.span)
    }
}

impl fmt::Debug for FloatLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} at {:?}", &self.value, &self.span)
    }
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Str(s) => write!(f, "Lit {s:?}"),
            Self::Char(ch) => write!(f, "Lit {ch:?}"),
            Self::Int(n) => write!(f, "Lit {n:?}"),
            Self::Float(x) => write!(f, "Lit {x:?}"),
        }
    }
}

impl Spanned for StrLiteral {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl Spanned for CharLiteral {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl Spanned for IntLiteral {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl Spanned for FloatLiteral {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl Spanned for Literal {
    fn span(&self) -> &Span {
        match self {
            Self::Str(s) => s.span(),
            Self::Char(ch) => ch.span(),
            Self::Int(n) => n.span(),
            Self::Float(x) => x.span(),
        }
    }
}

pub fn parse_literal(input: StrSpan) -> IResult<StrSpan, Literal> {
    alt((
        parse_float_lit.map(|x| Literal::Float(x)),
        parse_int_lit.map(|n| Literal::Int(n)),
        parse_char_lit.map(|ch| Literal::Char(ch)),
        parse_str_lit.map(|s| Literal::Str(s)),
    ))(input)
}

pub fn parse_ident(input: StrSpan) -> IResult<StrSpan, StrSpan> {
    recognize(pair(
        satisfy(|ch| {
            !ch.is_whitespace() && !ch.is_ascii_digit() && !is_special(ch) && !is_punct(ch)
        }),
        take_while(|ch: char| !ch.is_whitespace() && !is_special(ch) && !is_punct(ch)),
    ))(input)
}

pub fn parse_punct(input: StrSpan) -> IResult<StrSpan, Punct> {
    alt((
        pair(one_of(PUNCT_CHARS), peek(opt(one_of(PUNCT_CHARS)))).map(|(ch, next_ch)| Punct {
            ch: ch as _,
            spacing: match next_ch {
                Some(_) => PunctSpacing::Joint,
                None => PunctSpacing::Alone,
            },
        }),
        value(
            Punct {
                ch: b'\'',
                spacing: PunctSpacing::Joint,
            },
            pair(char('\''), peek(satisfy(|ch| ch.is_ascii_alphabetic()))),
        ),
    ))(input)
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum Delim {
    /// Round Parentheses `(...)`
    Parens,
    /// Square Brackets `[...]`
    Brackets,
    /// Curly Braces `{...}`
    Braces,
}

impl fmt::Debug for Delim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parens => write!(f, "Delim()"),
            Self::Brackets => write!(f, "Delim[]"),
            Self::Braces => write!(f, "Delim{{}}"),
        }
    }
}

impl Delim {
    pub const fn opening_char(&self) -> char {
        match self {
            Self::Parens => '(',
            Self::Brackets => '[',
            Self::Braces => '{',
        }
    }

    pub const fn closing_char(&self) -> char {
        match self {
            Self::Parens => ')',
            Self::Brackets => ']',
            Self::Braces => '}',
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Group {
    pub delim: Delim,
    pub tokens: TokensVec,
}

impl Spanned for Group {
    fn span(&self) -> &Span {
        &self.tokens.span()
    }
}

impl fmt::Debug for Group {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct Indent<'a, 'b> {
            f: &'a mut fmt::Formatter<'b>,
            on_newline: bool,
        }

        impl<'a, 'b> fmt::Write for Indent<'a, 'b> {
            fn write_str(&mut self, mut s: &str) -> fmt::Result {
                while !s.is_empty() {
                    if self.on_newline {
                        self.f.write_str("    ")?;
                    }

                    let split = match s.find('\n') {
                        Some(pos) => {
                            self.on_newline = true;
                            pos + 1
                        }
                        None => {
                            self.on_newline = false;
                            s.len()
                        }
                    };
                    self.f.write_str(&s[..split])?;
                    s = &s[split..];
                }

                Ok(())
            }
        }

        write!(f, "Group{}", self.delim.opening_char())?;
        if f.alternate() {
            match self.tokens.len() {
                0 => {}
                1 => write!(f, "{:#?}", &self.tokens[0])?,
                _ => {
                    use fmt::Write;

                    let indent_f = &mut Indent {
                        f,
                        on_newline: false,
                    };
                    writeln!(indent_f)?;
                    for token in &self.tokens {
                        writeln!(indent_f, "{token:#?},")?;
                    }
                }
            }
        } else {
            let mut iter = self.tokens.iter();
            if let Some(token) = iter.next() {
                write!(f, "{token:?}")?;
                for token in iter {
                    write!(f, ", {token:?}")?;
                }
            }
        }
        write!(f, "{} at {:?}", self.delim.closing_char(), self.span())?;

        Ok(())
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct PunctToken {
    pub punct: Punct,
    pub span: Span,
}

impl ops::Deref for PunctToken {
    type Target = Punct;

    fn deref(&self) -> &Punct {
        &self.punct
    }
}

impl ops::DerefMut for PunctToken {
    fn deref_mut(&mut self) -> &mut Punct {
        &mut self.punct
    }
}

impl Spanned for PunctToken {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Debug for PunctToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} at {:?}", &self.punct, &self.span)
    }
}

#[derive(Clone, PartialEq)]
pub struct Ident {
    pub ident: String,
    pub span: Span,
}

impl ops::Deref for Ident {
    type Target = String;

    #[inline]
    fn deref(&self) -> &String {
        &self.ident
    }
}

impl ops::DerefMut for Ident {
    #[inline]
    fn deref_mut(&mut self) -> &mut String {
        &mut self.ident
    }
}

impl Spanned for Ident {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Ident(`{}` at {:?})", &self.ident, &self.span)
    }
}

#[derive(Clone, PartialEq)]
pub struct Newline {
    pub span: Span,
}

impl Spanned for Newline {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Debug for Newline {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Newline at {:?}", &self.span)
    }
}

#[derive(Clone, PartialEq)]
pub enum Token {
    Literal(Literal),
    Punct(PunctToken),
    Ident(Ident),
    Group(Group),
    Newline(Newline),
}

impl Token {
    pub fn span(&self) -> &Span {
        match self {
            Self::Literal(lit) => lit.span(),
            Self::Punct(punct) => punct.span(),
            Self::Ident(ident) => ident.span(),
            Self::Group(group) => group.span(),
            Self::Newline(newline) => newline.span(),
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Token::")?;
        match self {
            Self::Literal(literal) => write!(f, "{literal:?}"),
            Self::Punct(punct) => write!(f, "{punct:?}"),
            Self::Ident(ident) => write!(f, "{ident:?}"),
            Self::Group(group) => {
                if f.alternate() {
                    write!(f, "{group:#?}")
                } else {
                    write!(f, "{group:?}")
                }
            }
            Self::Newline(newline) => write!(f, "{newline:?}"),
        }
    }
}

pub fn spanned<'a, O, E, F>(
    mut parser: F,
) -> impl FnMut(StrSpan<'a>) -> IResult<StrSpan<'a>, (Span, O), E>
where
    E: nom::error::ParseError<StrSpan<'a>>,
    F: Parser<StrSpan<'a>, O, E>,
{
    move |input| {
        let start = Pos {
            offset: input.location_offset(),
            line: input.location_line(),
            column: input.get_column(),
        };

        let (out_str, output) = parser.parse(input)?;
        let span = Span {
            file_name: out_str.extra.file_name.clone(),
            start,
            end: Pos {
                offset: out_str.location_offset(),
                line: out_str.location_line(),
                column: out_str.get_column(),
            },
        };
        Ok((out_str, (span, output)))
    }
}

/// Parses a block comment recursively.
/// `n` is used to specify the number of `#` in the comment block.
/// The `##...#(` have to be parsed before use.
fn block_comment(input: StrSpan) -> IResult<StrSpan, StrSpan> {
    use nom::Slice;

    let mut iter = input.char_indices();
    let n = loop {
        match iter.next() {
            Some((_, '#')) => {}
            Some((i, '(')) if 0 < i => break i,
            _ => return fail(input),
        }
    };

    let mut level = 1;
    let mut close = false;
    let mut hash_count = 0;
    for (i, ch) in iter {
        if ch == '#' {
            hash_count += 1;
            if close && hash_count == n {
                level -= 1;
                if level == 0 {
                    return Ok((input.slice(i + 1..), input.slice(n + 1..i - n)));
                }
            }
        } else {
            if ch == '(' && n <= hash_count {
                level += 1;
            }
            close = ch == ')';
            hash_count = 0;
        }
    }
    cut(fail)(input)
}

pub fn skip_whitespace(input: StrSpan) -> IResult<StrSpan, StrSpan> {
    name_span!(input("skip_whitespace"), {
        recognize(many0_count(alt((
            recognize(pair(char('\\'), satisfy(is_newline))),
            take_while1(|ch: char| ch.is_whitespace() && !is_newline(ch)),
            block_comment,
            logging(
                "basic_comment",
                recognize(pair(char('#'), take_till(is_newline))),
            ),
        ))))(input)
    })
}

pub fn parse_group(input: StrSpan) -> IResult<StrSpan, Group> {
    name_span!(input("group"), {
        spanned(|input| {
            let (input, (delim, tokens)) = pair(
                one_of("([{").map(|delim| match delim {
                    '(' => Delim::Parens,
                    '[' => Delim::Brackets,
                    '{' => Delim::Braces,
                    _ => unreachable!(),
                }),
                parse_tokens,
            )(input)?;
            Ok((char(delim.closing_char())(input)?.0, (delim, tokens)))
        })
        .map(|(span, (delim, tokens))| Group {
            delim,
            tokens: Tokens::with_span(tokens, span),
        })
        .parse(input)
    })
}

pub fn parse_token(input: StrSpan) -> IResult<StrSpan, Token> {
    name_span!(input("parse_token"), {
        alt((
            parse_literal.map(|literal| Token::Literal(literal)),
            spanned(parse_punct).map(|(span, punct)| Token::Punct(PunctToken { punct, span })),
            spanned(parse_ident).map(|(span, ident)| {
                Token::Ident(Ident {
                    ident: str::to_owned(ident.fragment()),
                    span,
                })
            }),
            spanned(sep(|| one_of(NEWLINE_CHARS), skip_whitespace))
                .map(|(span, _)| Token::Newline(Newline { span })),
            parse_group.map(|group| Token::Group(group)),
        ))(input)
    })
}

pub fn parse_tokens(input: StrSpan) -> IResult<StrSpan, Vec<Token>> {
    preceded(
        sep(|| skip_whitespace, one_of(NEWLINE_CHARS)),
        many0(terminated(parse_token, skip_whitespace)),
    )(input)
}

pub fn parse_file(
    file_name: Arc<PathBuf>,
    input: &str,
) -> Result<TokensVec, nom::Err<nom::error::Error<StrSpan>>> {
    Ok(TokensVec::new(
        terminated(parse_tokens, eof)(StrSpan::new_extra(
            input,
            StrSpanInfo {
                file_name,
                scope_name: "parse_file",
            },
        ))?
        .1,
    ))
}

pub type TokensVec = Tokens<Vec<Token>>;
pub type TokensRef<'a> = Tokens<&'a [Token]>;
pub type TokensRefMut<'a> = Tokens<&'a mut [Token]>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Tokens<T> {
    inner: T,
    span: Span,
}

impl<T: AsRef<[Token]>> Tokens<T> {
    pub fn new(tokens: T) -> Self {
        let tokens_ref = tokens.as_ref();
        let span = tokens_ref[0]
            .span()
            .join(tokens_ref.last().unwrap().span())
            .unwrap();
        Self::with_span(tokens, span)
    }
}

impl<T> Tokens<T> {
    #[inline]
    pub fn with_span(tokens: T, span: Span) -> Self {
        Self {
            inner: tokens,
            span,
        }
    }
}

impl<T> Spanned for Tokens<T> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<T> ops::Deref for Tokens<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.inner
    }
}

impl<T> ops::DerefMut for Tokens<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}

fn range_ends<R: ops::RangeBounds<usize>>(len: usize, range: R) -> (usize, usize) {
    let start = match range.start_bound() {
        Bound::Included(&x) => x,
        Bound::Excluded(&x) => x + 1,
        Bound::Unbounded => 0,
    };
    let end = match range.end_bound() {
        Bound::Included(&x) => x + 1,
        Bound::Excluded(&x) => x,
        Bound::Unbounded => len,
    };
    (start, end)
}

impl<'a> Tokens<&'a [Token]> {
    pub fn take_slice<R: ops::RangeBounds<usize>>(&self, range: R) -> Option<Self> {
        let inner = &self.inner;
        let (start, end) = range_ends(inner.len(), range);

        Some(Tokens {
            inner: inner.get(start..end)?,
            span: if start < end {
                inner[start].span().join(inner[end - 1].span()).unwrap()
            } else {
                inner[start].span().beginning()
            },
        })
    }

    pub fn take_split_first(&self) -> Option<(&'a Token, Self)> {
        let inner = self.inner.as_ref();
        let (first, rest) = inner.split_first()?;

        Some((
            first,
            Tokens {
                inner: rest,
                span: if 1 < inner.len() {
                    self.span().ending().join(inner[1].span()).unwrap()
                } else {
                    self.span().ending()
                },
            },
        ))
    }

    pub fn take_split_last(&self) -> Option<(&'a Token, Self)> {
        let inner = self.inner.as_ref();
        let (last, rest) = inner.split_last()?;

        Some((
            last,
            Tokens {
                inner: rest,
                span: if 1 < inner.len() {
                    self.span()
                        .beginning()
                        .join(inner[inner.len() - 2].span())
                        .unwrap()
                } else {
                    self.span().beginning()
                },
            },
        ))
    }

    pub fn take_split_at(&self, split: usize) -> (Self, Self) {
        let inner = self.inner.as_ref();
        let (left, right) = inner.split_at(split);

        (
            Tokens {
                inner: left,
                span: if 0 < split {
                    self.span()
                        .beginning()
                        .join(inner[split - 1].span())
                        .unwrap()
                } else {
                    self.span().beginning()
                },
            },
            Tokens {
                inner: right,
                span: if split < inner.len() {
                    self.span().ending().join(inner[split].span()).unwrap()
                } else {
                    self.span().ending()
                },
            },
        )
    }
}

impl<T: AsRef<[Token]>> Tokens<T> {
    pub fn slice<R: ops::RangeBounds<usize>>(&self, range: R) -> Option<Tokens<&[Token]>> {
        let inner = self.inner.as_ref();
        let (start, end) = range_ends(inner.len(), range);

        Some(Tokens {
            inner: &inner.get(start..end)?,
            span: if start < end {
                inner[start].span().join(inner[end - 1].span()).unwrap()
            } else {
                inner[start].span().beginning()
            },
        })
    }

    pub fn split_first(&self) -> Option<(&Token, Tokens<&[Token]>)> {
        let inner = self.inner.as_ref();
        let (first, rest) = inner.split_first()?;

        Some((
            first,
            Tokens {
                inner: rest,
                span: if 1 < inner.len() {
                    self.span().ending().join(inner[1].span()).unwrap()
                } else {
                    self.span().ending()
                },
            },
        ))
    }

    pub fn split_last(&self) -> Option<(&Token, Tokens<&[Token]>)> {
        let inner = self.inner.as_ref();
        let (last, rest) = inner.split_last()?;

        Some((
            last,
            Tokens {
                inner: rest,
                span: if 1 < inner.len() {
                    self.span()
                        .beginning()
                        .join(inner[inner.len() - 2].span())
                        .unwrap()
                } else {
                    self.span().beginning()
                },
            },
        ))
    }

    pub fn split_at(&self, split: usize) -> (Tokens<&[Token]>, Tokens<&[Token]>) {
        let inner = self.inner.as_ref();
        let (left, right) = inner.split_at(split);

        (
            Tokens {
                inner: left,
                span: if 0 < split {
                    self.span()
                        .beginning()
                        .join(inner[split - 1].span())
                        .unwrap()
                } else {
                    self.span().beginning()
                },
            },
            Tokens {
                inner: right,
                span: if split < inner.len() {
                    self.span().ending().join(inner[split].span()).unwrap()
                } else {
                    self.span().ending()
                },
            },
        )
    }
}

impl<T: AsMut<[Token]>> Tokens<T> {
    pub fn slice_mut<R: ops::RangeBounds<usize>>(
        &mut self,
        range: R,
    ) -> Option<Tokens<&mut [Token]>> {
        let inner = self.inner.as_mut();
        let (start, end) = range_ends(inner.len(), range);

        _ = inner.get(start..end)?;
        Some(Tokens {
            span: if start < end {
                inner[start].span().join(inner[end - 1].span()).unwrap()
            } else {
                inner[start].span().beginning()
            },
            inner: &mut inner[start..end],
        })
    }

    pub fn split_first_mut(&mut self) -> Option<(&mut Token, Tokens<&mut [Token]>)> {
        let inner = self.inner.as_mut();
        let (first, rest) = inner.split_first_mut()?;
        let span = if let Some(tk) = rest.first() {
            self.span.ending().join(tk.span()).unwrap()
        } else {
            self.span.ending()
        };

        Some((first, Tokens { inner: rest, span }))
    }

    pub fn split_last_mut(&mut self) -> Option<(&mut Token, Tokens<&mut [Token]>)> {
        let inner = self.inner.as_mut();
        let (last, rest) = inner.split_last_mut()?;
        let span = if let Some(tk) = rest.last() {
            self.span.beginning().join(tk.span()).unwrap()
        } else {
            self.span.beginning()
        };

        Some((last, Tokens { inner: rest, span }))
    }

    pub fn split_at_mut(&mut self, split: usize) -> (Tokens<&mut [Token]>, Tokens<&mut [Token]>) {
        let inner = self.inner.as_mut();

        let left_span = if 0 < split {
            self.span.beginning().join(inner[split - 1].span()).unwrap()
        } else {
            self.span.beginning()
        };
        let right_span = if split < inner.len() {
            self.span.ending().join(inner[split].span()).unwrap()
        } else {
            self.span.ending()
        };

        let (left, right) = inner.split_at_mut(split);

        (
            Tokens {
                inner: left,
                span: left_span,
            },
            Tokens {
                inner: right,
                span: right_span,
            },
        )
    }
}

impl<T: AsRef<[Token]>> Tokens<T> {
    pub fn as_ref(&self) -> Tokens<&[Token]> {
        Tokens {
            inner: self.inner.as_ref(),
            span: self.span.clone(),
        }
    }
}

impl<T: AsMut<[Token]>> Tokens<T> {
    pub fn as_mut(&mut self) -> Tokens<&mut [Token]> {
        Tokens {
            inner: self.inner.as_mut(),
            span: self.span.clone(),
        }
    }
}

impl<'a, T> IntoIterator for &'a Tokens<T>
where
    &'a T: IntoIterator<Item = &'a Token>,
{
    type Item = &'a Token;
    type IntoIter = <&'a T as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.inner).into_iter()
    }
}

impl<'a, T> IntoIterator for &'a mut Tokens<T>
where
    &'a mut T: IntoIterator<Item = &'a mut Token>,
{
    type Item = &'a mut Token;
    type IntoIter = <&'a mut T as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&mut self.inner).into_iter()
    }
}
