pub mod ascii;
pub mod cache_hash;
pub mod lexer;
pub mod parser;
pub mod parser2;
pub mod parser_old;
pub mod parserlib;

use clap::{Arg, Command};
use crossterm::style::Stylize;
use lexer::TokenTree;
use std::{fmt, fs, io, path::Path, str};
// use crossterm::style::Stylize;
// use std::io::{self, Write};
// use std::mem;

// artemis fowl vs johan liebert

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

    #[inline]
    pub fn begining(self) -> Self {
        Self {
            end: self.start,
            ..self
        }
    }

    #[inline]
    pub fn ending(self) -> Self {
        Self {
            start: self.end,
            ..self
        }
    }

    pub fn union(self, other: Self) -> Self {
        assert_eq!(self.path, other.path);
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            path: self.path,
        }
    }

    pub fn unite_with(&mut self, other: Self) {
        assert_eq!(self.path, other.path);
        self.start = self.start.min(other.start);
        self.end = self.start.min(other.end);
    }

    pub fn try_union(self, other: Option<Self>) -> Self {
        match other {
            Some(other) => self.union(other),
            None => self,
        }
    }

    pub fn try_unite_with(&mut self, other: Option<Self>) {
        if let Some(other) = other {
            self.unite_with(other);
        }
    }

    pub fn display(self, lines: &[usize], s: &str) {
        if self.start == self.end {
            let line = lines.binary_search(&self.start).unwrap_or_else(|err| err);
            print!(
                "{} | {}",
                line + 1,
                s.get(line.checked_sub(1).map(|i| lines[i] + 1).unwrap_or(0)..self.start)
                    .unwrap(),
            );
            let (cursor_col, _) = crossterm::cursor::position().unwrap();
            println!(
                "{}",
                s.get(self.end..lines.get(line).copied().unwrap_or(s.len()))
                    .or_else(|| s.get(self.end..))
                    .unwrap()
            );
            for _ in 0..cursor_col {
                print!(" ");
            }
            println!("{}", "^".bold().red());
        } else {
            // println!("Error at {}", ShowSpan { span: *self, lines });
            let start_line = lines.binary_search(&self.start).unwrap_or_else(|err| err);
            let end_line = lines
                .binary_search(&(self.end - 1))
                .unwrap_or_else(|err| err);

            // println!("Span::display - self: {self}");
            // println!(
            //     "Span::display - lines: {}:{} {}:{}",
            //     start_line + 1,
            //     self.start + 1 - start_line.checked_sub(1).map(|i| lines[i] + 1).unwrap_or(0),
            //     end_line + 1,
            //     self.end - lines.get(end_line).copied().unwrap_or(s.len())
            // );

            let start_line_start = start_line.checked_sub(1).map(|i| lines[i] + 1).unwrap_or(0);
            if start_line == end_line {
                println!(
                    "{} | {}{}{}",
                    start_line + 1,
                    s.get(start_line_start..self.start).unwrap(),
                    s.get(self.start..self.end).unwrap().bold().red(),
                    s.get(self.end..lines[end_line])
                        .or_else(|| s.get(self.end..))
                        .unwrap(),
                );
            } else {
                println!(
                    "{} | {}{}",
                    start_line + 1,
                    s.get(start_line_start..self.start).unwrap(),
                    s.get(self.start..lines[start_line]).unwrap().bold().red(),
                );
                for line in start_line + 1..end_line {
                    println!(
                        "{} | {}",
                        line + 1,
                        s.get(lines[line - 1] + 1..lines[line])
                            .unwrap()
                            .bold()
                            .red(),
                    );
                }
                println!(
                    "{} | {}{}",
                    end_line + 1,
                    s.get(lines[end_line - 1] + 1..self.end)
                        .unwrap()
                        .bold()
                        .red(),
                    s.get(self.end..lines[end_line])
                        .or_else(|| s.get(self.end..))
                        .unwrap(),
                );
            }
        }
    }
}

impl<'a> fmt::Display for Span<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.start == self.end {
            write!(f, "{}:{}", self.path.display(), self.start)
        } else {
            write!(f, "{}:[{}-{}]", self.path.display(), self.start, self.end)
        }
    }
}

impl<'a> fmt::Debug for Span<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Span({})", self)
    }
}

pub trait Spanned<'a> {
    fn span(&self) -> Span<'a>;
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

pub fn lines(s: &str) -> Vec<usize> {
    s.char_indices()
        .filter_map(|(i, ch)| if ch == '\n' { Some(i) } else { None })
        .collect()
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

    // match parser2::TUPLING_EXPR {
    //     parser2::Module::NamedModule { module, .. } => {
    //         println!("{:#}", module);
    //     }
    //     _ => unreachable!(),
    // }

    match matches.subcommand() {
        Some(("run", sub_m)) => {
            let script = sub_m.value_of("SCRIPT").unwrap();
            let path = Path::new(script);
            let file = fs::read_to_string(path)
                .unwrap_or_else(|_| panic!("File `{}` not found", path.display()))
                + "\n";
            let file_lines = lines(&file);
            // println!("file: {file:?}");
            // println!("file_lines: {file_lines:?}");

            let mut tokens = match TokenTree::tokenize(path, &file) {
                Ok(tokens) => tokens,
                Err(err) => {
                    for span in err.spans() {
                        span.display(&file_lines, &file);
                    }
                    panic!("{}", err);
                }
            };
            tokens.post_process();
            println!("{:#?}", tokens);
            {
                let expr = match parserlib::Parser::parse(
                    &parser::TuplingExprParser {
                        ignore_newline: false,
                    },
                    &mut parser_old::Tokens::new(&tokens),
                ) {
                    Ok(out) => out,
                    Err(err) => {
                        for span in err.spans() {
                            span.display(&file_lines, &file);
                        }
                        panic!("Failed to parse expression with error: {err}");
                    }
                };
                println!("{expr:#?}");
            }

            println!(
                "{:#?}",
                parser_old::pub_parse_expr(
                    &mut parser_old::Tokens::new(&tokens),
                    &file,
                    &file_lines
                )
            );
        }
        _ => {}
    }

    Ok(())
}
