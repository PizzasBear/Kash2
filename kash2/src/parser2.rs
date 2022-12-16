use crate::lexer2::{Delim, Ident, Literal, Span, Spanned, Token, TokensRef};
use kash2_derive::mamamia;
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

#[derive(Debug, Clone, Copy)]
pub enum UniOp {
    Neg,
    Pos,
    Deref,
    Ref,
}

impl UniOp {
    pub fn parse_prefix(mut tokens: TokensRef) -> Result<(Self, TokensRef)> {
        let val = mamamia!(tokens {
            $(-) => Self::Neg,
            $(+) => Self::Pos,
            $(*) => Self::Deref,
            $(&) => Self::Ref,
        })
        .map_err(|_| Error::Expected {
            text: "A prefix operator".into(),
            span: tokens.span().beginning(),
        })?;

        Ok((val, tokens))
    }

    pub fn parse_suffix(mut tokens: TokensRef) -> Result<(Self, TokensRef)> {
        let val = mamamia!(tokens {
            $(.-) => Self::Neg,
            $(.+) => Self::Pos,
            $(.*) => Self::Deref,
            $(.&) => Self::Ref,
        })
        .map_err(|_| Error::Expected {
            text: "A suffix operator".into(),
            span: tokens.span().beginning(),
        })?;

        Ok((val, tokens))
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
        let val = mamamia!(tokens {
            $(*) => Self::Mul,
            $(/) => Self::Div,
            $(%) => Self::Mod,
            $(+) => Self::Add,
            $(-) => Self::Sub,
            $(>>) => Self::Shr,
            $(<<) => Self::Shl,
            $(>>>) => Self::Rotr,
            $(<<<) => Self::Rotl,
            $(^) => Self::Xor,
            $(&) => Self::BitAnd,
            $(|) => Self::BitOr,
            $(==) => Self::Eq,
            $(!=) => Self::Neq,
            $(<) => Self::Lt,
            $(>) => Self::Gt,
            $(<=) => Self::Le,
            $(>=) => Self::Ge,
            $(&&) => Self::And,
            $(||) => Self::Or,
        })
        .map_err(|_| Error::Expected {
            text: "a joining operator".into(),
            span: tokens.span().beginning(),
        })?;

        Ok((val, tokens))
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
        })
        .map_err(|_| {
            tokens.first().map_or_else(
                || Error::UnexpectedEof {
                    span: tokens.span().clone(),
                },
                |first| Error::UnexpectedToken {
                    token: first.clone(),
                },
            )
        })?;

        Ok((value, tokens))
    }

    pub fn parse(mut tokens: TokensRef) -> Result<(Self, TokensRef)> {
        // prefix uni-ops
        let mut prefix_ops = vec![];
        while let Ok((op, rest)) = UniOp::parse_prefix(tokens.clone()) {
            prefix_ops.push(op);
            tokens = rest;
        }
        let mut value;
        (value, tokens) = Self::parse_base(tokens)?;
        while mamamia!(tokens {
            $(#op:(UniOp::parse_suffix)) => {
                value = Self::UniOp {
                    op,
                    value: Box::new(value),
                };
            },
        })
        .is_ok()
        {}
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
