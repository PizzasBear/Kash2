use crate::lexer2::{Delim, Ident, Literal, Punct, PunctToken, Span, Spanned, Token, TokensRef};
use kash2_derive::{mamamia, one_mamamia};
use std::{borrow::Cow, error::Error as StdError, fmt};

#[derive(Debug)]
pub enum Error {
    UnexpectedToken { token: Token },
    UnexpectedEof { span: Span },
    Expected { text: Cow<'static, str>, span: Span },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken { token } => {
                write!(f, "Unexpected token {token:#?}: {}", token.span().show())
            }
            Self::UnexpectedEof { span } => {
                write!(f, "Unexpected end of file (EOF): {}", span.show())
            }
            Self::Expected { text, span } => {
                write!(f, "Expected \"{text}\": {}", span.show())
            }
        }
    }
}

impl StdError for Error {}

pub type Result<T, Err = Error> = std::result::Result<T, Err>;

pub trait Parser {
    type Type;

    fn parse<'a>(&self, tokens: TokensRef<'a>) -> Result<(Self::Type, TokensRef<'a>)>;
}

impl<T, F: Fn(TokensRef) -> Result<(T, TokensRef)>> Parser for F {
    type Type = T;

    fn parse<'a>(&self, tokens: TokensRef<'a>) -> Result<(T, TokensRef<'a>)> {
        self(tokens)
    }
}

fn punctuated<P: Parser>(
    parser: &P,
    sep: u8,
    nonempty: bool,
    allow_trailing: bool,
) -> impl '_ + Fn(TokensRef) -> Result<(Vec<P::Type>, TokensRef)> {
    move |mut tokens| {
        let mut values = vec![mamamia!(tokens {
            $(#value:(*parser)) => value,
            else [err] => return if nonempty {
                Err(err)
            } else {
                Ok((vec![], tokens))
            }
        })];

        loop {
            let Some((first, rest)) = tokens.take_split_first() else { break; };
            let Token::Punct(PunctToken { punct, .. }) = first  else { break; };
            if punct.ch != sep {
                break;
            }
            if let Ok((value, rest)) = parser.parse(rest.clone()) {
                values.push(value);
                tokens = rest;
            } else {
                if allow_trailing {
                    tokens = rest;
                }
                break;
            }
        }
        Ok((values, tokens))
    }
}

fn expect_end<P: Parser>(parser: &P) -> impl '_ + Fn(TokensRef) -> Result<(P::Type, TokensRef)> {
    move |mut tokens| {
        let value;
        (value, tokens) = parser.parse(tokens)?;
        if let Some(first) = tokens.first() {
            Err(Error::UnexpectedToken {
                token: first.clone(),
            })
        } else {
            Ok((value, tokens))
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UniOp {
    Neg,
    Pos,
    Deref,
    Ref,
}

impl UniOp {
    pub fn parse_prefix(mut tokens: TokensRef) -> Result<(Self, TokensRef)> {
        Ok((
            mamamia!(tokens {
                $(-) => Self::Neg,
                $(+) => Self::Pos,
                $(*) => Self::Deref,
                $(&) => Self::Ref,
                else _ => return Err(Error::Expected {
                    text: "a prefix operator".into(),
                    span: tokens.span().beginning(),
                })
            }),
            tokens,
        ))
    }

    pub fn parse_suffix(mut tokens: TokensRef) -> Result<(Self, TokensRef)> {
        Ok((
            mamamia!(tokens {
                $(.-) => Self::Neg,
                $(.+) => Self::Pos,
                $(.*) => Self::Deref,
                $(.&) => Self::Ref,
                else _ => return Err(Error::Expected {
                    text: "a suffix operator".into(),
                    span: tokens.span().beginning(),
                })
            }),
            tokens,
        ))
    }
}

pub enum DuoOp {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Shr,
    Shl,
    Rotr,
    Rotl,
    Xor,
    BitAnd,
    BitOr,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
}

impl DuoOp {
    fn parse(mut tokens: TokensRef) -> Result<(Self, TokensRef)> {
        Ok((
            mamamia!(tokens {
                $(*) => Self::Mul,
                $(/) => Self::Div,
                $(%) => Self::Mod,
                $(+) => Self::Add,
                $(-) => Self::Sub,
                $(<<) => Self::Shl,
                $(>>) => Self::Shr,
                $(>>>) => Self::Rotr,
                $(<<<) => Self::Rotl,
                $(^) => Self::Xor,
                $(&) => Self::BitAnd,
                $(|) => Self::BitOr,
                $(==) => Self::Eq,
                $(!=) => Self::Neq,
                $(>) => Self::Gt,
                $(<=) => Self::Le,
                $(<) => Self::Lt,
                $(>=) => Self::Ge,
                $(&&) => Self::And,
                $(||) => Self::Or,
                else _ => return Err(Error::Expected {
                    text: "a joining operator".into(),
                    span: tokens.span().beginning(),
                })
            }),
            tokens,
        ))
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Literal(Literal),
    Var(Ident),
    UniOp { op: UniOp, value: Box<Self> },
}

impl Value {
    pub fn parse_base(mut tokens: TokensRef) -> Result<(Self, TokensRef)> {
        let value = mamamia!(tokens {
            $(#literal:literal) => Self::Literal(literal.clone()),
            $(#name:ident) => Self::Var(name.clone()),
            else _ => return Err(tokens.first().map_or_else(
                || Error::UnexpectedEof {
                    span: tokens.span().clone(),
                },
                |first| Error::UnexpectedToken {
                    token: first.clone(),
                },
            ))
        });

        Ok((value, tokens))
    }

    // ($[#var:(Expr::parse)]*(,)?)

    pub fn parse(mut tokens: TokensRef) -> Result<(Self, TokensRef)> {
        // prefix uni-ops
        let mut prefix_ops = vec![];
        while let Ok((op, rest)) = UniOp::parse_prefix(tokens.clone()) {
            prefix_ops.push(op);
            tokens = rest;
        }
        let mut value;
        (value, tokens) = Self::parse_base(tokens)?;
        loop {
            value = mamamia!(tokens {
                $(#op:(UniOp::parse_suffix)) => Self::UniOp {
                    op,
                    value: Box::new(value),
                },
                else _ => break
            });
        }
        for op in prefix_ops.into_iter().rev() {
            value = Self::UniOp {
                op,
                value: Box::new(value),
            };
        }
        Ok((value, tokens))
    }
}

#[test]
fn print() {}
