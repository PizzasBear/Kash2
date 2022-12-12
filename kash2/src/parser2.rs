use crate::lexer2::{
    CharLiteral, Delim, FloatLiteral, Group, Ident, IntLiteral, Literal, Punct, Span, Spanned,
    StrLiteral, Token, TokensRef, TokensVec,
};
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

pub enum TokenMatch<'a> {
    Int,
    Float,
    Char,
    Str,
    Literal,
    AnyIdent,
    Punct(Punct),
    Ident(&'a str),
    Group(Delim, &'a [TokenMatch<'a>]),
}

pub enum MatchValue<'a> {
    Int(&'a IntLiteral),
    Float(&'a FloatLiteral),
    Char(&'a CharLiteral),
    Str(&'a StrLiteral),
    Literal(&'a Literal),
    Ident(&'a Ident),
    Punct(&'a Punct),
    IdentMatch(&'a Ident),
    Group(&'a Group, Vec<MatchValue<'a>>),
    Many(Vec<MatchValue<'a>>),
}

impl<'a> TokenMatch<'a> {
    pub fn parse<'b>(&self, mut tokens: TokensRef<'b>) -> Result<(MatchValue<'b>, TokensRef<'b>)> {
        let first;
        (first, tokens) = tokens
            .take_split_first()
            .ok_or_else(|| Error::UnexpectedEof {
                span: tokens.span().clone(),
            })?;
        Ok((
            match (self, first) {
                (Self::Int, Token::Literal(Literal::Int(lit))) => MatchValue::Int(lit),
                (Self::Float, Token::Literal(Literal::Float(lit))) => MatchValue::Float(lit),
                (Self::Char, Token::Literal(Literal::Char(lit))) => MatchValue::Char(lit),
                (Self::Literal, Token::Literal(lit)) => MatchValue::Literal(lit),
                (Self::AnyIdent, Token::Ident(ident)) => MatchValue::Ident(ident),
                (Self::Punct(match_punct), Token::Punct(punct))
                    if match_punct.ch == punct.punct.ch
                        && punct.spacing.does_match(&match_punct.spacing) =>
                {
                    MatchValue::Punct(punct)
                }
                (&Self::Ident(match_ident), Token::Ident(ident)) if match_ident == **ident => {
                    MatchValue::IdentMatch(ident)
                }
                (
                    Self::Group(match_delim, match_tokens),
                    Token::Group(group @ Group { delim, tokens }),
                ) if match_delim == delim => {
                    let (out, tail) = Self::multiparse(&match_tokens, tokens.as_ref())?;
                    if !out.is_empty() {
                        return Err(Error::UnexpectedToken {
                            token: tokens[match_tokens.len()].clone(),
                        });
                    }
                    MatchValue::Group(group, out)
                }
                _ => {
                    return Err(Error::UnexpectedToken {
                        token: first.clone(),
                    })
                }
            },
            tokens,
        ))
        // num channel x y z
    }

    pub fn multiparse<'b>(
        tk_matches: &[Self],
        mut tokens: TokensRef<'b>,
    ) -> Result<(Vec<MatchValue<'b>>, TokensRef<'b>)> {
        let mut result = Vec::with_capacity(tk_matches.len());
        for tk_match in tk_matches {
            let value;
            (value, tokens) = tk_match.parse(tokens)?;
            result.push(value);
        }
        Ok((result, tokens))
    }
}

pub enum UniOp {
    Neg,
    Pos,
    Deref,
    Ref,
}

impl UniOp {
    pub fn parse_prefix(mut tokens: TokensRef) -> Result<(Self, TokensRef)> {
        let first;
        (first, tokens) = tokens
            .take_split_first()
            .ok_or_else(|| Error::UnexpectedEof {
                span: tokens.span().clone(),
            })?;

        todo!()
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Literal(Literal),
    Var(Ident),
}

impl Value {
    pub fn parse_base(mut tokens: TokensRef) -> Result<(Self, TokensRef)> {
        let first;
        (first, tokens) = tokens
            .take_split_first()
            .ok_or_else(|| Error::UnexpectedEof {
                span: tokens.span().clone(),
            })?;

        let value = match first {
            Token::Literal(lit) => Value::Literal(lit.clone()),
            Token::Ident(ident) => Value::Var(ident.clone()),
            Token::Group(_group) => todo!(),
            _ => {
                return Err(Error::UnexpectedToken {
                    token: first.clone(),
                })
            }
        };

        Ok((value, tokens))
    }

    pub fn parse(tokens: TokensRef) -> Result<(Self, TokensRef)> {
        // prefix uni-ops
        let (first, tokens) = tokens.split_first().ok_or_else(|| Error::UnexpectedEof {
            span: tokens.span().clone(),
        })?;

        todo!()
    }
}

#[test]
fn print() {}
