pub mod lexer;
pub mod parser;

use clap::{Arg, Command};
use crossterm::style::Stylize;
use lexer::TokenTree;
use std::{fmt, fs, io, mem, ops, path::Path, ptr, str};
// use crossterm::style::Stylize;
// use std::io::{self, Write};
// use std::mem;

// artemis fowl vs johan liebert

#[derive(Default, Clone, Copy, PartialEq, Eq)]
#[repr(C, align(8))]
pub struct Ascii7 {
    len: u8,
    chars: [u8; 7],
}

impl Ascii7 {
    #[inline]
    pub fn new() -> Self {
        Self {
            len: 0,
            chars: [0; 7],
        }
    }

    #[inline]
    pub fn len(self) -> usize {
        self.len as _
    }

    pub fn push(&mut self, ch: u8) -> bool {
        if self.len() < self.chars.len() && ch.is_ascii() {
            self.chars[self.len()] = ch;
            self.len += 1;
            true
        } else {
            false
        }
    }

    pub fn pop(&mut self) -> Option<u8> {
        unsafe {
            if self.len == 0 {
                None
            } else {
                self.len -= 1;
                Some(mem::replace(
                    self.chars.get_unchecked_mut(self.len as usize),
                    0,
                ))
            }
        }
    }
}

#[derive(Clone, Copy, Hash)]
pub enum Ascii7ConversionError {
    TooLong,
    NotAscii,
}

impl fmt::Display for Ascii7ConversionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TooLong => write!(
                f,
                "Tried to convert a string longer than 7 characters to `Ascii7`"
            ),
            Self::NotAscii => write!(f, "Tried to convert a non ASCII string to `Ascii7`"),
        }
    }
}

impl fmt::Debug for Ascii7ConversionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", std::any::type_name::<Self>())?;
        match self {
            Self::TooLong => write!(f, "TooLong")?,
            Self::NotAscii => write!(f, "NotAscii")?,
        }
        write!(f, " (\"{}\")", self)?;
        Ok(())
    }
}

impl std::error::Error for Ascii7ConversionError {}

impl<'a> TryFrom<&'a [u8]> for Ascii7 {
    type Error = Ascii7ConversionError;

    fn try_from(s: &'a [u8]) -> Result<Self, Self::Error> {
        if 7 < s.len() {
            Err(Ascii7ConversionError::TooLong)
        } else if !s.is_ascii() {
            Err(Ascii7ConversionError::NotAscii)
        } else {
            let mut slf = Self::new();
            unsafe {
                ptr::copy_nonoverlapping(s.as_ptr(), slf.chars.as_mut_ptr(), s.len());
            }
            slf.len = s.len() as _;
            Ok(slf)
        }
    }
}

impl ops::Deref for Ascii7 {
    type Target = str;

    #[inline]
    fn deref(&self) -> &str {
        unsafe { str::from_utf8_unchecked(&self.chars.get_unchecked(..self.len as usize)) }
    }
}

impl fmt::Display for Ascii7 {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <str as fmt::Display>::fmt(self, f)
    }
}

impl fmt::Debug for Ascii7 {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <str as fmt::Debug>::fmt(self, f)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span<'a> {
    pub start: usize,
    pub end: usize,
    pub path: &'a Path,
}

impl<'a> Span<'a> {
    #[inline]
    pub const fn new(path: &'a Path, start: usize, end: usize) -> Self {
        Self { path, start, end }
    }

    pub fn union(self, other: Self) -> Self {
        assert_eq!(self.path, other.path);
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            ..self
        }
    }

    pub fn display(&self, lines: &[usize], s: &str) {
        println!("Error at {}", ShowSpan { span: *self, lines });
        let start_line = lines.binary_search(&self.start).unwrap_or_else(|err| err);
        let end_line = lines
            .binary_search(&(self.end - 1))
            .unwrap_or_else(|err| err);

        let start_line_start = start_line.checked_sub(1).map(|i| lines[i] + 1).unwrap_or(0);
        if start_line == end_line {
            println!(
                "{} | {}{}{}",
                start_line + 1,
                &s.get(start_line_start..self.start).unwrap(),
                s.get(self.start..self.end).unwrap().bold().red(),
                &s.get(self.end..lines[end_line]).unwrap(),
            );
        } else {
            println!(
                "{} | {}{}",
                start_line + 1,
                &s.get(start_line_start..self.start).unwrap(),
                &s.get(self.start..lines[start_line]).unwrap().bold().red(),
            );
            for line in start_line + 1..end_line {
                println!(
                    "{} | {}",
                    line + 1,
                    &s.get(lines[line - 1] + 1..lines[line])
                        .unwrap()
                        .bold()
                        .red(),
                );
            }
            println!(
                "{} | {}{}",
                end_line + 1,
                &s.get(lines[end_line - 1] + 1..self.end)
                    .unwrap()
                    .bold()
                    .red(),
                &s.get(self.end - 1..lines[end_line]).unwrap(),
            );
        }
    }
}

impl<'a> fmt::Display for Span<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:[{}-{}]", self.path.display(), self.start, self.end)
    }
}

impl<'a> fmt::Debug for Span<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Span({})", self)
    }
}

struct ShowSpan<'a> {
    span: Span<'a>,
    lines: &'a [usize],
}

impl<'a> fmt::Display for ShowSpan<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let start_line = self
            .lines
            .binary_search(&self.span.start)
            .unwrap_or_else(|err| err);
        let end_line = self
            .lines
            .binary_search(&(self.span.end - 1))
            .unwrap_or_else(|err| err);
        write!(
            f,
            "{}[{}:{}-{}:{}]",
            self.span.path.display(),
            start_line + 1,
            self.span.start + 1
                - start_line
                    .checked_sub(1)
                    .map(|i| self.lines[i] + 1)
                    .unwrap_or(0),
            end_line + 1,
            self.span.end
                - end_line
                    .checked_sub(1)
                    .map(|i| self.lines[i] + 1)
                    .unwrap_or(0),
        )
    }
}

fn main() -> io::Result<()> {
    let matches = Command::new("Kash 2")
        .version("indev")
        .about("This is a shell language with the goal of making complex actions simpler to read, yet quick to write.")
        .author("Max S.")
        .subcommand(Command::new("run")
            .about("Runs the provided script")
            .arg(Arg::new("SCRIPT")
                .help("Sets the script file to use")
                .required(true)
                .index(1)))
        .get_matches();

    match matches.subcommand() {
        Some(("run", sub_m)) => {
            let script = sub_m.value_of("SCRIPT").unwrap();
            let path = Path::new(script);
            let file = fs::read_to_string(path)
                .unwrap_or_else(|_| panic!("File `{}` not found", path.display()))
                + "\n";

            let mut tokens = TokenTree::tokenize(path, &file);
            tokens.post_process();
            tokens.tokens = tokens
                .tokens
                .into_iter()
                .filter(|token| !matches!(token, TokenTree::NewLine(_)))
                .collect();
            println!("{:#?}", tokens);
            println!("{:#?}", parser::parse_expr(&mut &*tokens.tokens));
        }
        _ => {}
    }

    Ok(())
}
