#![allow(dead_code)]

use proc_macro2::{Delimiter, Ident, Punct, Spacing, Span, TokenStream};
use quote::{quote, ToTokens};
use std::ops;
use syn::{
    braced, bracketed,
    ext::IdentExt,
    parenthesized,
    parse::{Parse, ParseStream},
    parse_macro_input, token, LitChar, LitFloat, LitInt, LitStr, Token,
};

struct Lexer2;
impl ToTokens for Lexer2 {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(quote!(crate::lexer2))
    }
}

struct Parser2;
impl ToTokens for Parser2 {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(quote!(crate::parser2))
    }
}

#[derive(Debug, Clone)]
enum PunctMatch {
    Simple(Punct),
    Escaped { dollar: Token![$], punct: Punct },
}

impl ops::Deref for PunctMatch {
    type Target = Punct;

    fn deref(&self) -> &Punct {
        match self {
            Self::Simple(punct) => punct,
            Self::Escaped { punct, .. } => punct,
        }
    }
}

impl ops::DerefMut for PunctMatch {
    fn deref_mut(&mut self) -> &mut Punct {
        match self {
            Self::Simple(punct) => punct,
            Self::Escaped { punct, .. } => punct,
        }
    }
}

impl Parse for PunctMatch {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![#]) || input.peek(Token![@]) {
            Err(input.error("unescaped punct"))
        } else if input.cursor().punct().map_or(false, |(x, cursor)| {
            x.as_char() == '$' && cursor.punct().is_some()
        }) {
            Ok(PunctMatch::Escaped {
                dollar: input.parse().unwrap(),
                punct: input.parse().unwrap(),
            })
        } else {
            Ok(PunctMatch::Simple(input.parse()?))
        }
    }
}

// #[derive(Debug, Clone)]
// struct PunctsMatch {
//     pub puncts: Vec<PunctMatch>,
// }
//
// impl Parse for PunctsMatch {
//     fn parse(input: ParseStream) -> syn::Result<Self> {
//         let mut puncts = vec![PunctMatch::parse(input)?];
//         if puncts[0].spacing() == Spacing::Joint {
//             while let Ok(punct) = PunctMatch::parse(input) {
//                 let spacing = punct.spacing();
//                 puncts.push(punct);
//
//                 if spacing == Spacing::Alone {
//                     break;
//                 }
//             }
//         }
//         Ok(Self { puncts })
//     }
// }

#[inline]
fn check_spacing(check: Spacing, input: Spacing) -> bool {
    check == Spacing::Alone || input == Spacing::Joint
}

#[derive(Debug, Clone)]
struct IntMatch {
    literal: LitInt,
    num: u64,
}

impl Parse for IntMatch {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let literal: LitInt = input.parse()?;
        let num = literal.base10_parse()?;
        Ok(Self { literal, num })
    }
}

#[derive(Debug, Clone)]
struct FloatMatch {
    literal: LitFloat,
    num: f64,
}

impl Parse for FloatMatch {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let literal: LitFloat = input.parse()?;
        let num = literal.base10_parse()?;
        Ok(Self { literal, num })
    }
}

#[derive(Debug, Clone)]
struct StrMatch {
    literal: LitStr,
    s: String,
}

impl Parse for StrMatch {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let literal: LitStr = input.parse()?;
        let s = literal.value();
        Ok(Self { literal, s })
    }
}

#[derive(Debug, Clone)]
struct CharMatch {
    literal: LitChar,
    ch: char,
}

impl Parse for CharMatch {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let literal: LitChar = input.parse()?;
        let ch = literal.value();
        Ok(Self { literal, ch })
    }
}

#[derive(Debug, Clone)]
struct NewlineMatch {
    dollar: Token![$],
    n_ident: Ident,
}

impl Parse for NewlineMatch {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            dollar: input.parse()?,
            n_ident: input.step(|cursor| match cursor.ident() {
                Some((ident, rest)) if &ident.to_string() == "n" => Ok((ident, rest)),
                _ => Err(cursor.error("expected the identifier `n`")),
            })?,
        })
    }
}

#[derive(Debug, Clone)]
struct IdentMatch {
    ident: Ident,
}

impl Parse for IdentMatch {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            ident: Ident::parse_any(input)?.unraw(),
        })
    }
}

#[derive(Debug, Clone)]
enum MatchLit {
    Ident(IdentMatch),
    Punct(PunctMatch),
    Int(IntMatch),
    Float(FloatMatch),
    Str(StrMatch),
    Char(CharMatch),
    Newline(NewlineMatch),
}

impl MatchLit {
    fn pattern(&self) -> TokenStream {
        quote! { () }
    }
}

impl Parse for MatchLit {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(syn::Ident) {
            Ok(Self::Ident(input.parse()?))
        } else if input.peek(LitInt) {
            Ok(Self::Int(input.parse()?))
        } else if input.peek(LitFloat) {
            Ok(Self::Float(input.parse()?))
        } else if input.peek(LitStr) {
            Ok(Self::Str(input.parse()?))
        } else if input.peek(LitChar) {
            Ok(Self::Char(input.parse()?))
        } else if NewlineMatch::parse(&input.fork()).is_ok() {
            Ok(Self::Newline(input.parse()?))
        } else if PunctMatch::parse(&input.fork()).is_ok() {
            Ok(Self::Punct(input.parse()?))
        } else {
            Err(input.error("expected a literal token match"))
        }
    }
}

impl ToTokens for MatchLit {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Int(IntMatch { literal, .. }) => {
                let text = format!("the integer {literal}");
                tokens.extend(quote! {
                    if let (
                        #Lexer2::Token::Literal(#Lexer2::Literal::Int(#Lexer2::IntLiteral {
                            value: #literal,
                            ..
                        })),
                        rest,
                    ) = tokens
                        .take_split_first()
                        .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                            span: tokens.span().beginning(),
                        })?
                    {
                        tokens = rest;
                    } else {
                        return Err(#Parser2::Error::Expected {
                            text: #text.into(),
                            span: tokens.span().beginning(),
                        });
                    }
                });
            }
            Self::Float(FloatMatch { literal, .. }) => {
                let text = format!("the floating point number {literal}");
                tokens.extend(quote! {
                    if let (
                        #Lexer2::Token::Literal(#Lexer2::Literal::Float(#Lexer2::FloatLiteral {
                            value: #literal,
                            ..
                        })),
                        rest,
                    ) = tokens
                        .take_split_first()
                        .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                            span: tokens.span().beginning(),
                        })?
                    {
                        tokens = rest;
                    } else {
                        return Err(#Parser2::Error::Expected {
                            text: #text.into(),
                            span: tokens.span().beginning(),
                        });
                    }
                });
            }
            Self::Char(CharMatch { literal, ch }) => {
                let text = format!("the character {ch:?}");
                tokens.extend(quote! {
                    if let (
                        #Lexer2::Token::Literal(#Lexer2::Literal::Char(#Lexer2::CharLiteral {
                            value: #literal,
                            ..
                        })),
                        rest,
                    ) = tokens
                        .take_split_first()
                        .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                            span: tokens.span().beginning(),
                        })?
                    {
                        tokens = rest;
                    } else {
                        return Err(#Parser2::Error::Expected {
                            text: #text.into(),
                            span: tokens.span().beginning(),
                        });
                    }
                });
            }
            Self::Str(StrMatch { literal, s }) => {
                let text = format!("the string {s:?}");
                tokens.extend(quote! {
                    match tokens
                        .take_split_first()
                        .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                            span: tokens.span().beginning(),
                        })? {
                        (
                            #Lexer2::Token::Literal(#Lexer2::Literal::String(
                                #Lexer2::StrLiteral { value, .. },
                            )),
                            rest,
                        ) if value == #literal => {
                            tokens = rest;
                        }
                        _ => {
                            return Err(#Parser2::Error::Expected {
                                text: #text.into(),
                                span: tokens.span().beginning(),
                            })
                        }
                    }
                });
            }
            Self::Ident(IdentMatch { ident }) => {
                let text = format!("the ident `{ident}`");
                let ident_text = ident.to_string();
                tokens.extend(quote! {
                    match tokens
                        .take_split_first()
                        .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                            span: tokens.span().beginning(),
                        })? {
                        (#Lexer2::Token::Ident(#Lexer2::Ident { ident, .. }), rest)
                            if ident == #ident_text =>
                        {
                            tokens = rest;
                        }
                        _ => {
                            return Err(#Parser2::Error::Expected {
                                text: #text.into(),
                                span: tokens.span().beginning(),
                            })
                        }
                    }
                });
            }
            Self::Punct(PunctMatch::Simple(punct) | PunctMatch::Escaped { punct, .. }) => {
                let ch = syn::LitByte::new(punct.as_char() as _, Span::call_site());
                let text = format!(
                    "the punct `{}`{}",
                    punct.as_char(),
                    if punct.spacing() == Spacing::Joint {
                        " that's joint with another punct"
                    } else {
                        ""
                    },
                );

                let spacing = match punct.spacing() {
                    Spacing::Alone => quote!(..),
                    Spacing::Joint => quote!(spacing: #Lexer2::PunctSpacing::Joint),
                };

                tokens.extend(quote! {
                    if let (
                        #Lexer2::Token::Punct(#Lexer2::PunctToken {
                            punct: #Lexer2::Punct { ch: #ch, #spacing },
                            ..
                        }),
                        rest,
                    ) = tokens
                        .take_split_first()
                        .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                            span: tokens.span().beginning(),
                        })?
                    {
                        tokens = rest;
                    } else {
                        return Err(#Parser2::Error::Expected {
                            text: #text.into(),
                            span: tokens.span().beginning(),
                        });
                    }
                });
            }
            Self::Newline(_) => tokens.extend(quote! {
                if let (#Lexer2::Token::Newline(_), rest) =
                    tokens
                        .take_split_first()
                        .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                            span: tokens.span().beginning(),
                        })?
                {
                    tokens = rest;
                } else {
                    return Err(#Parser2::Error::Expected {
                        text: "a newline".into(),
                        span: tokens.span().beginning(),
                    });
                }
            }),
        }
    }
}

#[derive(Debug, Clone)]
enum NamedType {
    Ident(Ident),
    Punct(Ident),
    Literal(Ident),
    Int(Ident),
    Float(Ident),
    Str(Ident),
    Char(Ident),
    ParenGroup(Ident, token::Paren),
    BracketGroup(Ident, token::Bracket),
    BraceGroup(Ident, token::Brace),
    Expr(syn::ExprParen),
}

impl Parse for NamedType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(syn::Ident::peek_any) {
            input.step(|cursor| {
                cursor
                    .ident()
                    .and_then(|(ident, rest)| match ident.to_string().as_str() {
                        "ident" => Some((Self::Ident(ident), rest)),
                        "punct" => Some((Self::Punct(ident), rest)),
                        "literal" => Some((Self::Literal(ident), rest)),
                        "int" => Some((Self::Int(ident), rest)),
                        "float" => Some((Self::Float(ident), rest)),
                        "str" => Some((Self::Str(ident), rest)),
                        "char" => Some((Self::Char(ident), rest)),
                        "group" => {
                            if let Some((inside, span, rest)) = rest.group(Delimiter::Parenthesis) {
                                if inside.eof() {
                                    Some((
                                        Self::ParenGroup(ident, syn::token::Paren { span }),
                                        rest,
                                    ))
                                } else {
                                    None
                                }
                            } else if let Some((inside, span, rest)) =
                                rest.group(Delimiter::Bracket)
                            {
                                if inside.eof() {
                                    Some((
                                        Self::BracketGroup(ident, syn::token::Bracket { span }),
                                        rest,
                                    ))
                                } else {
                                    None
                                }
                            } else if let Some((inside, span, rest)) =
                                rest.group(Delimiter::Parenthesis)
                            {
                                if inside.eof() {
                                    Some((
                                        Self::BraceGroup(ident, syn::token::Brace { span }),
                                        rest,
                                    ))
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        }
                        _ => None,
                    })
                    .ok_or_else(|| {
                        cursor.error(format_args!(
                            "expected a named type (one of [{}] or a parenthesised expresion)",
                            &[
                                "`ident`",
                                "`punct`",
                                "`literal`",
                                "`int`",
                                "`float`",
                                "`str`",
                                "`char`",
                                "`group()`",
                                "`group[]`",
                                "`group{}`",
                            ]
                            .join(", "),
                        ))
                    })
            })
        } else if lookahead.peek(syn::token::Paren) {
            Ok(Self::Expr(input.parse()?))
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Debug, Clone)]
struct NamedMatch {
    hash: Token![#],
    pat: syn::Pat,
    colon: Token![:],
    ty: NamedType,
}

impl NamedMatch {
    fn pattern(&self) -> TokenStream {
        self.pat.to_token_stream()
    }
}

impl Parse for NamedMatch {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            hash: input.parse()?,
            pat: input.parse()?,
            colon: input.parse()?,
            ty: input.parse()?,
        })
    }
}

impl ToTokens for NamedMatch {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match &self.ty {
            NamedType::Ident(_) => tokens.extend(quote! {
                if let (#Lexer2::Token::Ident(ident), rest) =
                    tokens
                        .take_split_first()
                        .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                            span: tokens.span().beginning(),
                        })? {
                    tokens = rest;
                    ident
                } else {
                    return Err(#Parser2::Error::Expected {
                        text: "an identifier".into(),
                        span: tokens.span().beginning(),
                    });
                }
            }),
            NamedType::Literal(_) => tokens.extend(quote! {
                if let (#Lexer2::Token::Literal(literal), rest) =
                    tokens
                        .take_split_first()
                        .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                            span: tokens.span().beginning(),
                        })? {
                    tokens = rest;
                    literal
                } else {
                    return Err(#Parser2::Error::Expected {
                        text: "a literal".into(),
                        span: tokens.span().beginning(),
                    });
                }
            }),
            NamedType::Int(_) => tokens.extend(quote! {
                if let (#Lexer2::Token::Literal(#Lexer2::Literal::Int(n)), rest) =
                    tokens
                        .take_split_first()
                        .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                            span: tokens.span().beginning(),
                        })? {
                    tokens = rest;
                    n
                } else {
                    return Err(#Parser2::Error::Expected {
                        text: "an integer".into(),
                        span: tokens.span().beginning(),
                    });
                }
            }),
            NamedType::Float(_) => tokens.extend(quote! {
                if let (#Lexer2::Token::Literal(#Lexer2::Literal::Float(x)), rest) =
                    tokens
                        .take_split_first()
                        .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                            span: tokens.span().beginning(),
                        })? {
                    tokens = rest;
                    x
                } else {
                    return Err(#Parser2::Error::Expected {
                        text: "a floating point number".into(),
                        span: tokens.span().beginning(),
                    });
                }
            }),
            NamedType::Char(_) => tokens.extend(quote! {
                if let (#Lexer2::Token::Literal(#Lexer2::Literal::Char(ch)), rest) =
                    tokens
                        .take_split_first()
                        .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                            span: tokens.span().beginning(),
                        })? {
                    tokens = rest;
                    ch
                } else {
                    return Err(#Parser2::Error::Expected {
                        text: "a character literal".into(),
                        span: tokens.span().beginning(),
                    });
                }
            }),
            NamedType::Str(_) => tokens.extend(quote! {
                if let (#Lexer2::Token::Literal(#Lexer2::Literal::Str(s)), rest) =
                    tokens
                        .take_split_first()
                        .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                            span: tokens.span().beginning(),
                        })? {
                    tokens = rest;
                    s
                } else {
                    return Err(#Parser2::Error::Expected {
                        text: "a string literal".into(),
                        span: tokens.span().beginning(),
                    });
                }
            }),
            NamedType::Punct(_) => tokens.extend(quote! {
                if let (#Lexer2::Token::Punct(punct), rest) =
                    tokens
                        .take_split_first()
                        .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                            span: tokens.span().beginning(),
                        })? {
                    tokens = rest;
                    punct
                } else {
                    return Err(#Parser2::Error::Expected {
                        text: "a punctuation".into(),
                        span: tokens.span().beginning(),
                    });
                }
            }),
            NamedType::ParenGroup(_, _) => tokens.extend(quote! {
                if let (
                    #Lexer2::Token::Group(
                        group @ #Lexer2::Group {
                            delim: #Lexer2::Delim::Parens,
                            ..
                        },
                    ),
                    rest,
                ) = tokens
                    .take_split_first()
                    .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                        span: tokens.span().beginning(),
                    })?
                {
                    tokens = rest;
                    group
                } else {
                    return Err(#Parser2::Error::Expected {
                        text: "a parenthesis group".into(),
                        span: tokens.span().beginning(),
                    });
                }
            }),
            NamedType::BracketGroup(_, _) => tokens.extend(quote! {
                if let (
                    #Lexer2::Token::Group(
                        group @ #Lexer2::Group {
                            delim: #Lexer2::Delim::Brackets,
                            ..
                        },
                    ),
                    rest,
                ) = tokens
                    .take_split_first()
                    .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                        span: tokens.span().beginning(),
                    })?
                {
                    tokens = rest;
                    group
                } else {
                    return Err(#Parser2::Error::Expected {
                        text: "a bracket group".into(),
                        span: tokens.span().beginning(),
                    });
                }
            }),
            NamedType::BraceGroup(_, _) => tokens.extend(quote! {
                if let (
                    #Lexer2::Token::Group(
                        group @ #Lexer2::Group {
                            delim: #Lexer2::Delim::Braces,
                            ..
                        },
                    ),
                    rest,
                ) = tokens
                    .take_split_first()
                    .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                        span: tokens.span().beginning(),
                    })?
                {
                    tokens = rest;
                    group
                } else {
                    return Err(#Parser2::Error::Expected {
                        text: "a brace group".into(),
                        span: tokens.span().beginning(),
                    });
                }
            }),
            NamedType::Expr(expr) => tokens.extend(quote! {
                {
                    let __val;
                    (__val, tokens) = {
                        let __val = &mut ();
                        ::std::mem::drop(__val);
                        #expr(tokens)?
                    };

                    __val
                }
            }),
        }
    }
}

#[derive(Debug, Clone)]
struct SpannedMatch {
    content: Box<Match>,
    at: Token![@],
    pat: syn::Pat,
    semi: Token![;],
}

impl SpannedMatch {
    fn pattern(&self) -> TokenStream {
        let content = self.content.pattern();
        let pat = &self.pat;
        quote! {
            (#content, #pat)
        }
    }
}

impl ToTokens for SpannedMatch {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let content = &self.content;
        tokens.extend(quote! { {
            let __tokens_stack = tokens.clone();
            (
                {
                    let __tokens_stack = &mut ();
                    ::std::mem::drop(__tokens_stack);

                    #content
                },
                match __tokens_stack.first() {
                    Some(first_token) => first_token
                        .span()
                        .join(__tokens_stack[__tokens_stack.len() - tokens.len() - 1].span())
                        .unwrap(),
                    None => tokens.span().beginning(),
                },
            )
        } });
    }
}

#[derive(Debug, Clone)]
struct VectorMatch {
    dollar: Token![$],
    paren_token: token::Paren,
    content: Vec<Match>,
}

impl VectorMatch {
    fn pattern(&self) -> TokenStream {
        let content = self.content.iter().map(Match::pattern);
        quote! {
            ( #(#content,)* )
        }
    }
}

impl Parse for VectorMatch {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            dollar: input.parse()?,
            paren_token: parenthesized!(content in input),
            content: {
                let mut vector = vec![];
                while !content.is_empty() {
                    vector.push(content.parse()?);
                }
                vector
            },
        })
    }
}

impl ToTokens for VectorMatch {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let content = &self.content;
        tokens.extend(quote! {
            (
                #(#content,)*
            )
        });
    }
}

enum MatchGroup {
    Paren {
        paren: token::Paren,
        content: Vec<Match>,
    },
    Bracket {
        bracket: token::Bracket,
        content: Vec<Match>,
    },
    Brace {
        brace: token::Brace,
        content: Vec<Match>,
    },
}

impl MatchGroup {
    fn content(&self) -> &[Match] {
        match self {
            Self::Paren { content, .. } => &content,
            Self::Bracket { content, .. } => &content,
            Self::Brace { content, .. } => &content,
        }
    }

    fn pattern(&self) -> TokenStream {
        let content = self.content().iter().map(Match::pattern);
        quote! {
            ( #(#content,)* )
        }
    }
}

impl Parse for MatchGroup {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(token::Paren) {
            let content;
            Ok(Self::Paren {
                paren: parenthesized!(content in input),
                content: {
                    let mut vector = vec![];
                    while !content.is_empty() {
                        vector.push(content.parse()?);
                    }
                    vector
                },
            })
        } else if lookahead.peek(token::Bracket) {
            let content;
            Ok(Self::Bracket {
                bracket: bracketed!(content in input),
                content: {
                    let mut vector = vec![];
                    while !content.is_empty() {
                        vector.push(content.parse()?);
                    }
                    vector
                },
            })
        } else if lookahead.peek(token::Brace) {
            let content;
            Ok(Self::Brace {
                brace: braced!(content in input),
                content: {
                    let mut vector = vec![];
                    while !content.is_empty() {
                        vector.push(content.parse()?);
                    }
                    vector
                },
            })
        } else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for MatchGroup {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let content = self.content();
        let delim: syn::Path = match self {
            Self::Paren { .. } => syn::parse_quote! {
                #Lexer2::Delim::Parens
            },
            Self::Bracket { .. } => syn::parse_quote! {
                #Lexer2::Delim::Brackets
            },
            Self::Brace { .. } => syn::parse_quote! {
                #Lexer2::Delim::Braces
            },
        };
        tokens.extend(quote! {
            if let (
                #Lexer2::Token::Group(
                    #Lexer2::Group {
                        delim: #delim,
                        tokens: __group_tokens,
                    },
                ),
                __rest,
            ) = tokens
                .take_split_first()
                .ok_or_else(|| #Parser2::Error::UnexpectedEof {
                    span: tokens.span().beginning(),
                })?
            {
                let x = {
                    let mut tokens = __group_tokens;
                    let x = ( #(#content,)* );
                    if let Some(token) = tokens.first() {
                        return Err(#Parser2::Error::UnexpectedToken { token });
                    } else {
                        x
                    }
                };
                tokens = __rest;
                x
            } else {
                return Err(#Parser2::Error::Expected {
                    text: "a parenthesis group".into(),
                    span: tokens.span().beginning(),
                });
            }
        });
    }
}

#[derive(Debug, Clone)]
enum Match {
    MatchLit(MatchLit),
    NamedMatch(NamedMatch),
    Spanned(SpannedMatch),
    Vector(VectorMatch),
}

impl Match {
    fn pattern(&self) -> TokenStream {
        match self {
            Self::MatchLit(m) => m.pattern(),
            Self::Spanned(m) => m.pattern(),
            Self::Vector(m) => m.pattern(),
            Self::NamedMatch(m) => m.pattern(),
        }
    }
}

impl Parse for Match {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let slf = if input.peek(Token![#]) {
            Self::NamedMatch(input.parse()?)
        } else if input.peek(Token![$]) {
            if input.peek2(syn::token::Paren) {
                Self::Vector(input.parse()?)
            } else {
                Self::MatchLit(input.parse()?)
            }
        } else {
            Self::MatchLit(input.parse()?)
        };
        if input.peek(Token![@]) {
            Ok(Self::Spanned(SpannedMatch {
                content: Box::new(slf),
                at: input.parse()?,
                pat: input.parse()?,
                semi: input.parse()?,
            }))
        } else {
            Ok(slf)
        }
    }
}

impl ToTokens for Match {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::MatchLit(m) => m.to_tokens(tokens),
            Self::Spanned(m) => m.to_tokens(tokens),
            Self::Vector(m) => m.to_tokens(tokens),
            Self::NamedMatch(m) => m.to_tokens(tokens),
        }
    }
}

#[derive(Debug)]
struct Mamamia {
    tokens_ident: Ident,
    brace: token::Brace,
    exprs: Vec<(VectorMatch, Token![=>], syn::Expr, Token![,])>,
    else_arm: Option<(Token![else], syn::Pat, Token![=>], syn::Expr)>,
}

impl Parse for Mamamia {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            tokens_ident: input.step(|cursor| {
                cursor
                    .ident()
                    .and_then(|(ident, rest)| match ident.to_string().as_str() {
                        "tokens" => Some((ident, rest)),
                        _ => None,
                    })
                    .ok_or_else(|| {
                        cursor.error(
                            "expected a named type (one of {:?} or a parenthesised expresion)",
                        )
                    })
            })?,
            brace: braced!(content in input),
            exprs: {
                let mut exprs = vec![];

                while content.peek(Token![$]) {
                    exprs.push((
                        content.parse()?,
                        content.parse()?,
                        content.parse()?,
                        content.parse()?,
                    ));
                }

                exprs
            },
            else_arm: if content.is_empty() {
                None
            } else {
                Some((
                    content.parse()?,
                    content.parse()?,
                    content.parse()?,
                    content.parse()?,
                ))
            },
        })
    }
}

impl ToTokens for Mamamia {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        // struct Mamamia {
        //     tokens_ident: Ident,
        //     comma: Token![,],
        //     exprs: Vec<(VectorMatch, Token![=>], syn::Expr, Option<Token![,]>)>,
        //     else_arm: Option<(Token![else], syn::Pat, Token![=>], syn::Expr)>,
        // }
        let exprs = self.exprs.iter().map(|(vmatch, arrow, expr, comma)| {
            let pattern = vmatch.pattern();
            quote! {
                match (|| -> crate::parser2::Result<_> {
                    let __var;
                    (__var, tokens) = {
                        let __var = &mut ();
                        ::std::mem::drop(__var);

                        let mut tokens = tokens.clone();
                        (#vmatch, tokens)
                    };
                    Ok(__var)
                })() {
                    Ok(#pattern) #arrow break '__mamamia_block Ok(#expr) #comma
                    Err(err) => err
                }
            }
        });
        let result = quote! {
            '__mamamia_block: {
                use #Lexer2::Spanned;
                Err([ #(#exprs,)* ])
            }
        };
        if let Some((_else_token, pat, arrow, expr)) = &self.else_arm {
            tokens.extend(quote! {
                match #result {
                    Ok(x) => x,
                    Err(#pat) #arrow #expr,
                }
            });
        } else {
            tokens.extend(result)
        }
    }
}

macro_rules! show {
    ($name:expr, $expr:expr) => {{
        let (name, x) = ($name, $expr);
        println!("----------");
        println!("{name}");
        println!("{x}");
        x
    }};
}

#[proc_macro]
pub fn mamamia(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as Mamamia);

    proc_macro::TokenStream::from(show!("Output:", input.into_token_stream()))
}
