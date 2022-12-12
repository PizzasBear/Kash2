use crate::{
    lexer::{self, Delimiter},
    parser_old::Tokens,
    Span,
};
use std::{error::Error, fmt};

pub const KEYWORDS: &[&str] = &[
    "let", "mut", "if", "else", "for", "while", "loop", "async", "await", "fn", "class", "struct",
    "trait", "impl", "false", "true", "_",
];

pub enum ParseError<'a> {
    ExpectedModule {
        module: &'static Module,
        span: Span<'a>,
    },
    Unexpected {
        name: &'static str,
        span: Span<'a>,
    },
    Expected {
        name: &'static str,
        span: Span<'a>,
    },
}

impl<'a> ParseError<'a> {
    pub fn span(&self) -> Span<'a> {
        match *self {
            Self::ExpectedModule { span, .. } => span,
            Self::Expected { span, .. } => span,
            Self::Unexpected { span, .. } => span,
        }
    }
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Unexpected { name, span } => {
                write!(f, "Unexpected {name} at '{span}'")
            }
            Self::ExpectedModule { module, span } => {
                write!(f, "Expected module {module} at '{span}'")
            }
            Self::Expected { name, span } => write!(f, "Expected {name} at '{span}'"),
        }
    }
}
impl<'a> fmt::Debug for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
impl<'a> Error for ParseError<'a> {}

#[derive(Clone, Copy)]
pub enum TokenMatch {
    Newline,
    NotKeywordIdent,
    Keyword(&'static str),
    Punct(&'static [u8]),
    Group(Delimiter, &'static Module),
    Str(Option<&'static str>),
    Int(Option<u64>),
    Float(Option<f64>),
    // Ident(Option<String>),
    // Group(lexer::Delimiter, Vec<Module>),
    // Str(Option<String>),
}

impl fmt::Debug for TokenMatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TM::")?;
        match self {
            Self::Newline => write!(f, "Newline"),
            Self::NotKeywordIdent => write!(f, "NotKeywordIdent"),
            Self::Keyword(kw) => f.debug_tuple("Keyword").field(kw).finish(),
            Self::Punct(punct) => f
                .debug_tuple("Punct")
                .field(&std::str::from_utf8(punct).unwrap())
                .finish(),
            Self::Group(delim, module) => {
                f.debug_tuple("Group").field(delim).field(module).finish()
            }
            Self::Str(s) => f.debug_tuple("Str").field(s).finish(),
            Self::Int(x) => f.debug_tuple("Int").field(x).finish(),
            Self::Float(x) => f.debug_tuple("Float").field(x).finish(),
        }
    }
}

#[derive(Clone, Copy)]
pub enum Module {
    TokenMatch(TokenMatch),
    Repeat {
        body: &'static Module,
        sep: &'static Module,
        allow_empty: bool,
        allow_trailing_sep: bool,
    },
    Optional(&'static Self),
    OneOf(&'static [Self]),
    Modules(&'static [Self]),
    NamedModule {
        name: &'static str,
        module: &'static Self,
    },
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Module::")?;
        match self {
            Self::TokenMatch(tm) => f.debug_tuple("TokenMatch").field(tm).finish(),
            Self::Optional(module) => f.debug_tuple("Optional").field(module).finish(),
            Self::OneOf(modules) => f.debug_tuple("OneOf").field(modules).finish(),
            Self::Modules(modules) => f.debug_tuple("Modules").field(modules).finish(),
            Self::NamedModule { name, .. } => f.debug_tuple("NamedModule").field(name).finish(),
            Self::Repeat {
                body,
                sep,
                allow_empty,
                allow_trailing_sep,
            } => f
                .debug_struct("Repeat")
                .field("body", body)
                .field("sep", sep)
                .field("allow_empty", allow_empty)
                .field("allow_trailing_sep", allow_trailing_sep)
                .finish(),
        }
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use fmt::Write;
        use std::borrow::Borrow;

        struct Indent<'a, 'b>(&'a mut fmt::Formatter<'b>);

        impl<'a, 'b> Borrow<fmt::Formatter<'b>> for Indent<'a, 'b> {
            fn borrow(&self) -> &fmt::Formatter<'b> {
                self.0
            }
        }

        impl<'a, 'b> Write for Indent<'a, 'b> {
            fn write_str(&mut self, s: &str) -> fmt::Result {
                for ch in s.chars() {
                    self.write_char(ch)?;
                }
                Ok(())
            }
            fn write_char(&mut self, ch: char) -> fmt::Result {
                match ch {
                    '\n' => self.0.write_str("\n    "),
                    _ => self.0.write_char(ch),
                }
            }
        }

        fn one_of_fmt<'a, F: Borrow<fmt::Formatter<'a>> + fmt::Write>(
            f: &mut F,
            slf: &'static Module,
        ) -> fmt::Result {
            match *slf {
                Module::Modules(modules) => {
                    if (*f).borrow().alternate() {
                        if let Some((head, tail)) = modules.split_first() {
                            one_of_fmt(f, head)?;
                            for module in tail.iter() {
                                writeln!(f)?;
                                one_of_fmt(f, module)?;
                            }
                        }
                    } else {
                        if let Some((head, tail)) = modules.split_first() {
                            write!(f, "{head}")?;
                            for module in tail.iter() {
                                write!(f, " {module}")?;
                            }
                        }
                    }
                    Ok(())
                }
                Module::TokenMatch(TokenMatch::Punct(punct)) => {
                    for &ch in punct.iter() {
                        match ch {
                            b'$' => write!(f, "$$")?,
                            b'|' => write!(f, "$|")?,
                            _ => write!(f, "{}", char::from_u32(ch as _).unwrap())?,
                        }
                    }
                    Ok(())
                }
                _ => {
                    if (*f).borrow().alternate() {
                        write!(f, "{slf:#}")
                    } else {
                        write!(f, "{slf}")
                    }
                }
            }
        }

        match *self {
            Self::NamedModule { name, .. } => {
                write!(f, "${{{name:?}}}")
            }
            Self::OneOf(modules) => {
                if f.alternate() {
                    if let Some((head, tail)) = modules.split_first() {
                        write!(Indent(f), "$[\n  ")?;
                        one_of_fmt(&mut Indent(f), head)?;
                        for module in tail {
                            write!(Indent(f), "\n| ")?;
                            one_of_fmt(&mut Indent(f), module)?;
                        }
                        write!(f, "\n]")
                    } else {
                        write!(f, "$[]")
                    }
                } else {
                    write!(f, "$[")?;
                    if let Some((head, tail)) = modules.split_first() {
                        one_of_fmt(f, head)?;
                        for module in tail {
                            write!(f, " | ")?;
                            one_of_fmt(f, module)?;
                        }
                    }
                    write!(f, "]")
                }
            }
            Self::Repeat {
                body,
                sep,
                allow_empty,
                allow_trailing_sep,
            } => {
                if f.alternate() {
                    write!(Indent(f), "$(\n{body:#}")?;
                    if allow_trailing_sep {
                        write!(f, "\n)[")?;
                        write!(Indent(f), "\n{sep:#}")?;
                        write!(f, "\n]")?;
                    } else {
                        write!(f, "\n)(")?;
                        write!(Indent(f), "\n{sep:#}")?;
                        write!(f, "\n)")?;
                    }
                } else {
                    write!(f, "$({body})")?;
                    if allow_trailing_sep {
                        write!(f, "[{sep}]")?;
                    } else {
                        write!(f, "({sep})")?;
                    }
                }
                if allow_empty {
                    write!(f, "*")
                } else {
                    write!(f, "+")
                }
            }
            Self::Optional(module) => {
                if f.alternate() {
                    let ident_f = &mut Indent(f);
                    write!(ident_f, "$(\n{module:#}")?;
                    write!(f, "\n)?")
                } else {
                    write!(f, "$({module})?")
                }
            }
            Self::Modules(modules) => {
                if f.alternate() {
                    if let Some((head, tail)) = modules.split_first() {
                        write!(f, "{head:#}")?;
                        for module in tail.iter() {
                            write!(f, "\n{module:#}")?;
                        }
                    }
                } else {
                    if let Some((head, tail)) = modules.split_first() {
                        write!(f, "{head}")?;
                        for module in tail.iter() {
                            write!(f, " {module}")?;
                        }
                    }
                }
                Ok(())
            }
            Self::TokenMatch(ref tm) => match tm {
                TokenMatch::NotKeywordIdent => write!(f, "${{ident}}"),
                TokenMatch::Keyword(kw) => write!(f, "{kw}"),
                TokenMatch::Str(None) => write!(f, "${{str}}"),
                TokenMatch::Str(Some(s)) => write!(f, "{s:?}"),
                TokenMatch::Int(None) => write!(f, "${{int}}"),
                TokenMatch::Int(Some(x)) => write!(f, "{x:?}"),
                TokenMatch::Float(None) => write!(f, "${{float}}"),
                TokenMatch::Float(Some(x)) => write!(f, "{x:?}"),
                TokenMatch::Group(delim, module) => {
                    if f.alternate() {
                        write!(Indent(f), "{}\n{module:#}", delim.get_open())?;
                        write!(f, "\n{}", delim.get_close())
                    } else {
                        write!(f, "{}{module}{}", delim.get_open(), delim.get_close())
                    }
                }
                TokenMatch::Newline => write!(f, "${{newline}}"),
                TokenMatch::Punct(punct) => {
                    for &ch in punct.iter() {
                        match ch {
                            b'$' => write!(f, "$$")?,
                            _ => write!(f, "{}", char::from_u32(ch as _).unwrap())?,
                        }
                    }
                    Ok(())
                }
            },
        }
    }
}

#[derive(Debug)]
pub enum ModuleValueType<'a> {
    // /// A basic matching module
    // Module(&'static Module),
    /// A match to a basic matching module
    Match,
    /// A match to modules
    Values(Vec<ModuleValue<'a>>),
    Repeat {
        body: Vec<ModuleValue<'a>>,
        sep: Vec<ModuleValue<'a>>,
    },
    Optional(Option<Box<ModuleValue<'a>>>),
    /// The value chosen from a `Module::OneOf`
    ChosenOne {
        module: &'static Module,
        value: Box<ModuleValue<'a>>,
    },
    Ident(String),
    Group(Box<ModuleValue<'a>>),
    Str(String),
    Int(u64),
    Float(f64),
}

#[derive(Debug)]
pub struct ModuleValue<'a> {
    pub ty: ModuleValueType<'a>,
    pub span: Span<'a>,
}

impl Module {
    pub fn parse<'a>(
        &'static self,
        tokens: &mut Tokens<'a, '_>,
    ) -> Result<ModuleValue<'a>, ParseError<'a>> {
        let mut inner_tokens = *tokens;
        inner_tokens.skip_whitespace(false);
        let out = match *self {
            Self::OneOf(modules) => {
                let mut iter = modules.iter();
                loop {
                    if let Some(module) = iter.next() {
                        if let Ok(value) = module.parse(&mut inner_tokens) {
                            break ModuleValue {
                                span: value.span,
                                ty: ModuleValueType::ChosenOne {
                                    module: self,
                                    value: Box::new(value),
                                },
                            };
                        }
                    } else {
                        return Err(ParseError::ExpectedModule {
                            module: self,
                            span: inner_tokens.span().begining(),
                        });
                    }
                }
            }
            Self::NamedModule { name, module } => match module.parse(&mut inner_tokens) {
                Ok(value) => value,
                Err(_err) => {
                    return Err(ParseError::Expected {
                        name,
                        span: inner_tokens.span().begining(),
                    })
                }
            },
            Self::Modules(modules) => {
                let mut values = Vec::with_capacity(modules.len());
                let mut basic_match = true;
                for module in modules {
                    let value = module.parse(&mut inner_tokens)?;
                    basic_match = basic_match && matches!(value.ty, ModuleValueType::Match);
                    values.push(value);
                }
                ModuleValue {
                    span: values
                        .first()
                        .map(|x| x.span.union(values.last().unwrap().span))
                        .unwrap_or(inner_tokens.span().begining()),
                    ty: if basic_match {
                        ModuleValueType::Match
                    } else {
                        ModuleValueType::Values(values)
                    },
                }
            }
            Self::Optional(module) => match module.parse(&mut inner_tokens) {
                Ok(value) => ModuleValue {
                    span: value.span,
                    ty: ModuleValueType::Optional(Some(Box::new(value))),
                },
                Err(_) => ModuleValue {
                    span: inner_tokens.span().begining(),
                    ty: ModuleValueType::Optional(None),
                },
            },
            Self::Repeat {
                body,
                sep,
                allow_empty,
                allow_trailing_sep,
            } => match body.parse(&mut inner_tokens) {
                Ok(first) => {
                    let mut span = first.span;
                    let mut body_values = vec![first];
                    let mut sep_values = vec![];
                    loop {
                        match sep.parse(&mut inner_tokens) {
                            Ok(value) => {
                                span.unite_with(value.span);
                                sep_values.push(value);
                            }
                            Err(_) => break,
                        }
                        match body.parse(&mut inner_tokens) {
                            Ok(value) => {
                                span.unite_with(value.span);
                                body_values.push(value);
                            }
                            Err(_) if allow_trailing_sep => break,
                            Err(err) => return Err(err),
                        }
                    }
                    ModuleValue {
                        ty: ModuleValueType::Repeat {
                            body: body_values,
                            sep: sep_values,
                        },
                        span,
                    }
                }
                Err(_) if allow_empty => ModuleValue {
                    ty: ModuleValueType::Repeat {
                        body: vec![],
                        sep: vec![],
                    },
                    span: inner_tokens.span().begining(),
                },
                Err(err) => return Err(err),
            },
            Self::TokenMatch(tm) => match tm {
                TokenMatch::Group(delim, module) => match inner_tokens.pop_first() {
                    Some(&lexer::pat::group!((ref group @ { delim: tk_delim, span, .. })))
                        if tk_delim == delim =>
                    {
                        let mut tokens = Tokens::new(group);
                        let module_value = module.parse(&mut tokens)?;
                        if let Some(token) = tokens.first() {
                            return Err(ParseError::Unexpected {
                                name: "token",
                                span: token.span().begining(),
                            });
                        } else {
                            ModuleValue {
                                ty: ModuleValueType::Group(Box::new(module_value)),
                                span,
                            }
                        }
                    }
                    _ => {
                        return Err(ParseError::ExpectedModule {
                            module: self,
                            span: inner_tokens.span().begining(),
                        })
                    }
                },
                TokenMatch::Punct(punct) => {
                    let mut mod_span = inner_tokens
                        .first()
                        .map(|x| x.span())
                        .unwrap_or(inner_tokens.span())
                        .begining();
                    for &ch in punct {
                        match inner_tokens.pop_first() {
                            Some(&lexer::pat::punct!({ ch: tk_ch, span })) if ch == tk_ch => {
                                mod_span.unite_with(span);
                            }
                            _ => {
                                return Err(ParseError::ExpectedModule {
                                    module: self,
                                    span: mod_span.begining(),
                                })
                            }
                        }
                    }
                    ModuleValue {
                        ty: ModuleValueType::Match,
                        span: mod_span,
                    }
                }
                TokenMatch::NotKeywordIdent => match inner_tokens.pop_first() {
                    Some(&lexer::pat::ident!({ ref name, span }))
                        if !KEYWORDS.contains(&name.as_str()) =>
                    {
                        ModuleValue {
                            ty: ModuleValueType::Ident(name.clone()),
                            span,
                        }
                    }
                    _ => {
                        return Err(ParseError::ExpectedModule {
                            module: self,
                            span: inner_tokens.span().begining(),
                        })
                    }
                },
                TokenMatch::Keyword(kw) => match inner_tokens.pop_first() {
                    Some(&lexer::pat::ident!({ ref name, span })) if name == kw => ModuleValue {
                        ty: ModuleValueType::Match,
                        span,
                    },
                    _ => {
                        return Err(ParseError::ExpectedModule {
                            module: self,
                            span: inner_tokens.span().begining(),
                        })
                    }
                },
                TokenMatch::Newline => match inner_tokens.pop_first() {
                    Some(&lexer::pat::newline!({ span })) => ModuleValue {
                        ty: ModuleValueType::Match,
                        span,
                    },
                    _ => {
                        return Err(ParseError::Expected {
                            name: "a newline character",
                            span: inner_tokens.span().begining(),
                        })
                    }
                },
                TokenMatch::Str(None) => match inner_tokens.pop_first() {
                    Some(&lexer::pat::str!({ ref value, span })) => ModuleValue {
                        ty: ModuleValueType::Str(value.clone()),
                        span,
                    },
                    _ => {
                        return Err(ParseError::ExpectedModule {
                            module: self,
                            span: inner_tokens.span().begining(),
                        })
                    }
                },
                TokenMatch::Str(Some(value)) => match inner_tokens.pop_first() {
                    Some(&lexer::pat::str!({ value: ref tk_value, span })) if value == tk_value => {
                        ModuleValue {
                            ty: ModuleValueType::Match,
                            span,
                        }
                    }
                    _ => {
                        return Err(ParseError::ExpectedModule {
                            module: self,
                            span: inner_tokens.span().begining(),
                        })
                    }
                },
                TokenMatch::Int(None) => match inner_tokens.pop_first() {
                    Some(&lexer::pat::int!({ value, span })) => ModuleValue {
                        ty: ModuleValueType::Int(value),
                        span,
                    },
                    _ => {
                        return Err(ParseError::ExpectedModule {
                            module: self,
                            span: inner_tokens.span().begining(),
                        })
                    }
                },
                TokenMatch::Int(Some(value)) => match inner_tokens.pop_first() {
                    Some(&lexer::pat::int!({ value: tk_value, span })) if value == tk_value => {
                        ModuleValue {
                            ty: ModuleValueType::Match,
                            span,
                        }
                    }
                    _ => {
                        return Err(ParseError::ExpectedModule {
                            module: self,
                            span: inner_tokens.span().begining(),
                        })
                    }
                },
                TokenMatch::Float(None) => match inner_tokens.pop_first() {
                    Some(&lexer::pat::float!({ value, span })) => ModuleValue {
                        ty: ModuleValueType::Float(value),
                        span,
                    },
                    _ => {
                        return Err(ParseError::ExpectedModule {
                            module: self,
                            span: inner_tokens.span().begining(),
                        })
                    }
                },
                TokenMatch::Float(Some(value)) => match inner_tokens.pop_first() {
                    Some(&lexer::pat::float!({ value: tk_value, span })) if value == tk_value => {
                        ModuleValue {
                            ty: ModuleValueType::Match,
                            span,
                        }
                    }
                    _ => {
                        return Err(ParseError::ExpectedModule {
                            module: self,
                            span: inner_tokens.span().begining(),
                        })
                    }
                },
            },
        };
        *tokens = inner_tokens;
        Ok(out)
    }
}

pub const EMPTY: Module = Module::Modules(&[]);

pub static CORE_VALUE: Module = Module::OneOf(&[
    Module::TokenMatch(TokenMatch::Group(lexer::Delimiter::Parentheses, &EMPTY)),
    Module::TokenMatch(TokenMatch::Group(
        lexer::Delimiter::Parentheses,
        &TUPLING_EXPR,
    )),
    Module::TokenMatch(TokenMatch::Str(None)),
    Module::TokenMatch(TokenMatch::Float(None)),
    Module::TokenMatch(TokenMatch::Int(None)),
    Module::TokenMatch(TokenMatch::NotKeywordIdent),
]);
pub static VALUE_PREFIXES: Module = Module::Repeat {
    body: &Module::OneOf(&[
        Module::TokenMatch(TokenMatch::Punct(b"!")),
        Module::TokenMatch(TokenMatch::Punct(b"&")),
        Module::TokenMatch(TokenMatch::Punct(b"*")),
        Module::TokenMatch(TokenMatch::Punct(b"-")),
    ]),
    sep: &EMPTY,
    allow_empty: true,
    allow_trailing_sep: true,
};
pub static VALUE_SUFFIXES: Module = Module::Repeat {
    body: &Module::OneOf(&[
        Module::TokenMatch(TokenMatch::Punct(b".!")),
        Module::TokenMatch(TokenMatch::Punct(b".&")),
        Module::TokenMatch(TokenMatch::Punct(b".*")),
        Module::TokenMatch(TokenMatch::Punct(b".-")),
    ]),
    sep: &EMPTY,
    allow_empty: true,
    allow_trailing_sep: true,
};
pub static VALUE: Module = Module::Modules(&[VALUE_PREFIXES, CORE_VALUE, VALUE_SUFFIXES]);
pub static EXPR: Module = Module::NamedModule {
    name: "expression",
    module: &Module::Repeat {
        body: &VALUE,
        sep: &Module::OneOf(&[
            Module::TokenMatch(TokenMatch::Punct(b"<<")),
            Module::TokenMatch(TokenMatch::Punct(b">>")),
            Module::TokenMatch(TokenMatch::Punct(b"&&")),
            Module::TokenMatch(TokenMatch::Punct(b"||")),
            Module::TokenMatch(TokenMatch::Punct(b"|")),
            Module::TokenMatch(TokenMatch::Punct(b"&")),
            Module::TokenMatch(TokenMatch::Punct(b"^")),
            Module::TokenMatch(TokenMatch::Punct(b"<")),
            Module::TokenMatch(TokenMatch::Punct(b">")),
            Module::TokenMatch(TokenMatch::Punct(b"-")),
            Module::TokenMatch(TokenMatch::Punct(b"+")),
            Module::TokenMatch(TokenMatch::Punct(b"*")),
            Module::TokenMatch(TokenMatch::Punct(b"%")),
            Module::TokenMatch(TokenMatch::Punct(b"/")),
            Module::TokenMatch(TokenMatch::Punct(b"==")),
            Module::TokenMatch(TokenMatch::Punct(b"<=")),
            Module::TokenMatch(TokenMatch::Punct(b">=")),
            Module::TokenMatch(TokenMatch::Punct(b"!=")),
        ]),
        allow_empty: false,
        allow_trailing_sep: false,
    },
};
pub static TUPLING_EXPR: Module = Module::NamedModule {
    name: "tupling expression",
    module: &Module::Repeat {
        body: &EXPR,
        sep: &Module::TokenMatch(TokenMatch::Punct(b",")),
        allow_empty: false,
        allow_trailing_sep: true,
    },
};

pub static ASSIGNED: Module = Module::OneOf(&[
    Module::TokenMatch(TokenMatch::NotKeywordIdent),
    Module::Modules(&[Module::TokenMatch(TokenMatch::Punct(b"*")), VALUE]),
    Module::TokenMatch(TokenMatch::Group(
        Delimiter::Brackets,
        &Module::Repeat {
            body: &ASSIGNED,
            sep: &Module::TokenMatch(TokenMatch::Punct(b",")),
            allow_empty: true,
            allow_trailing_sep: true,
        },
    )),
    Module::TokenMatch(TokenMatch::Group(
        Delimiter::Parentheses,
        &Module::Repeat {
            body: &ASSIGNED,
            sep: &Module::TokenMatch(TokenMatch::Punct(b",")),
            allow_empty: true,
            allow_trailing_sep: true,
        },
    )),
]);
pub static ASSIGN_STMT: Module = Module::Modules(&[
    Module::Repeat {
        body: &Module::Modules(&[ASSIGNED, Module::TokenMatch(TokenMatch::Punct(b"="))]),
        sep: &EMPTY,
        allow_empty: true,
        allow_trailing_sep: true,
    },
    TUPLING_EXPR,
]);

// val:
// expr: $($(prefix_op)* val $(suffix_op)*) duo_op+
