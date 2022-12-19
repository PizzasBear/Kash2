use lazy_static::lazy_static;
use regex::Regex;
use std::{borrow::Cow, fmt, fs, iter::FusedIterator, ops, path::Path, str, sync::Arc};

pub const SPECIAL_CHARS: &str = r#"#()[]{}\"#;
pub const PUNCT_CHARS: &str = r#"+-*/%,!&|^~@$<>=.,:;?'"`"#;

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
    file_name: Arc<Path>,
    start: Pos,
    end: Pos,
}

impl Span {
    pub fn file_name(&self) -> &Path {
        &self.file_name
    }
    pub fn arc_file_name(&self) -> &Arc<Path> {
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

pub type Result<T, E = Error> = std::result::Result<T, E>;
pub struct Error {
    span: Span,
    text: Cow<'static, str>,
}

impl Spanned for Error {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Newline {
    span: Span,
}

impl Spanned for Newline {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Spacing {
    Joint,
    Alone,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Punct {
    pub ch: char,
    pub spacing: Spacing,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PunctToken {
    punct: Punct,
    span: Span,
}

impl PunctToken {
    fn punct(&self) -> &Punct {
        &self.punct
    }
}

impl Spanned for PunctToken {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StrToken {
    value: String,
    span: Span,
}

impl ops::Deref for StrToken {
    type Target = str;
    fn deref(&self) -> &str {
        &self.value
    }
}

impl Spanned for StrToken {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Newline(Newline),
    Punct(PunctToken),
    Str(StrToken),
}

impl Spanned for Token {
    fn span(&self) -> &Span {
        match self {
            Self::Newline(token) => token.span(),
            Self::Punct(token) => token.span(),
            Self::Str(token) => token.span(),
        }
    }
}

#[derive(Debug, Clone)]
struct PositionedChars<'a> {
    pos: Pos,
    chars: str::Chars<'a>,
}

impl<'a> PositionedChars<'a> {
    fn new(s: &'a str) -> Self {
        Self {
            pos: Pos {
                offset: 0,
                line: 1,
                column: 1,
            },
            chars: s.chars(),
        }
    }

    const fn consume(&mut self, num_bytes: usize) {
        let new_offset = self.pos().offset + num_bytes;
        while self.pos().offset < new_offset {
            self.next().unwrap();
        }
    }

    #[inline]
    #[must_use]
    const fn as_str(&self) -> &'a str {
        self.chars.as_str()
    }

    #[inline]
    #[must_use]
    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    #[inline]
    #[must_use]
    const fn pos(&self) -> Pos {
        self.pos
    }
}

impl<'a> ops::Deref for PositionedChars<'a> {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl<'a> Iterator for PositionedChars<'a> {
    type Item = (Pos, char);

    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.chars.next()?;
        let start = self.pos;

        self.pos.offset += ch.len_utf8();
        if ch == '\n' || ch == '\r' && self.peek() != Some('\n') {
            self.pos.line += 1;
            self.pos.column = 1;
        }

        Some((start, ch))
    }
}
impl<'a> FusedIterator for PositionedChars<'a> {}

pub fn lex(path: Arc<Path>, file: &str) -> Result<Vec<Token>> {
    let comment_start = Regex::new(r"\A#+\(").unwrap();
    let comment_segment = Regex::new(r"#+\(|\)#+").unwrap();
    let string = Regex::new(r#""[^(//)]*""#).unwrap();

    let mut tokens = vec![];
    let mut chars = PositionedChars::new(file);

    'chars_loop: while let Some(ch) = chars.peek() {
        if ch.is_whitespace() {
            let (start, _) = chars.next().unwrap();

            if (ch, chars.peek()) == ('\r', Some('\n')) {
                chars.next().unwrap();
            } else if ch != '\r' && ch != '\n' {
                continue;
            };
            tokens.push(Token::Newline(Newline {
                span: Span {
                    file_name: path.clone(),
                    start,
                    end: chars.pos(),
                },
            }));
        } else if let Some(mch) = comment_start.find(chars.as_str()) {
            let mut blocks = vec![mch.as_str().len()];
            for mch in comment_segment.find_iter(chars.as_str()).skip(1) {
                let mch_len = mch.as_str().len();

                if mch_len < blocks[blocks.len() - 1] {
                    continue;
                }

                if mch.as_str().starts_with(')') {
                    let len = blocks.pop().unwrap();
                    if blocks.is_empty() {
                        chars.consume(mch.start() + len);
                        continue 'chars_loop;
                    }
                } else {
                    blocks.push(mch_len);
                }
            }
            let start = chars.pos();
            chars.consume(chars.len());
            return Err(Error {
                span: Span {
                    file_name: path,
                    start,
                    end: chars.pos(),
                },
                text: "Unclosed comment".into(),
            });
        } else if ch == '\"' {
            let (start, _) = chars.next().unwrap();
            let mut value = String::new();

            while let Some((_, ch)) = chars.next() {
                match ch {
                    '\\' => {
                        let Some((_, ch)) = chars.next() else {
                            break;
                        };
                        match ch {
                            '\n' => {}
                            'n' => value.push('\n'),
                            'r' => value.push('\r'),
                            't' => value.push('\t'),
                            '0' => value.push('\0'),
                            '\\' | '\'' | '\"' => value.push(ch),
                            'x' => 'x_scope: {
                                let Some((_, ch1)) = chars.next() else {
                                    break 'x_scope;
                                };
                                let Some((_, ch2)) = chars.next() else {
                                    break 'x_scope;
                                };
                            }
                            'u' => 'u_scope: {
                                let pos = chars.pos();
                                let Some((_, '{')) = chars.next() else {
                                    return Err(Error {
                                        span: Span { file_name: path, start: pos, end: pos },
                                        text: "Missing `{` to begin unicode escape".into()
                                    });
                                };
                                let mut unicode: u32 = 0;
                                for len in 0.. {
                                    let Some((_, ch)) = chars.next() else {
                                        return Err(Error {
                                            span: Span { file_name: path, start: chars.pos(), end: chars.pos() },
                                            text: "Missing `{` to begin unicode escape".into()
                                        });
                                    };
                                    if ch == '}' {
                                        match len {
                                            0 => return Err(Error { span: (), text: () }),
                                        }
                                        break;
                                    } else if let Some(digit) = ch.to_digit(16) {
                                        unicode = unicode << 4 | digit;
                                    } else {
                                        return Err(Error {
                                            span: Span {
                                                file_name: path,
                                                start: chars.pos(),
                                                end: chars.pos(),
                                            },
                                            text: "Missing `{` to begin unicode escape".into(),
                                        });
                                    }
                                }
                            }
                        }
                    }
                    '\"' => {
                        value.shrink_to_fit();
                        tokens.push(Token::Str(StrToken {
                            value,
                            span: Span {
                                file_name: path.clone(),
                                start,
                                end: chars.pos(),
                            },
                        }));
                        continue 'chars_loop;
                    }
                    _ => value.push(ch),
                }
            }
            todo!();
        } else if PUNCT_CHARS.contains(ch) {
            let (start, _) = chars.next().unwrap();
            tokens.push(Token::Punct(PunctToken {
                punct: Punct {
                    ch,
                    spacing: match chars.peek() {
                        Some(ch) if PUNCT_CHARS.contains(ch) => Spacing::Joint,
                        _ => Spacing::Alone,
                    },
                },
                span: Span {
                    file_name: path.clone(),
                    start,
                    end: chars.pos(),
                },
            }));
        }
    }
    Ok(tokens)
}
