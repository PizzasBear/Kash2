use crate::lexer::{self, TokenTree};
use crate::{lines, Span};
use std::{error::Error, fmt, marker::PhantomData};

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum DuoOpType {
    Pow = 0x00,
    Mul = 0x10,
    Div = 0x11,
    Mod = 0x12,
    Add = 0x20,
    Sub = 0x21,
    Shr = 0x30,
    Shl = 0x31,
    BitXor = 0x40,
    BitAnd = 0x50,
    BitOr = 0x60,
    Lt = 0x70,
    Gt = 0x71,
    Le = 0x72,
    Ge = 0x73,
    Eq = 0x74,
    Neq = 0x75,
    And = 0x80,
    Or = 0x90,
}

impl DuoOpType {
    #[inline]
    const fn level(self) -> u8 {
        self as u8 & 0xf0
    }

    #[allow(dead_code)]
    #[inline]
    fn try_from(s: &str) -> Option<Self> {
        Self::try_from_buf(s.as_bytes())
    }

    fn try_from_buf(s: &[u8]) -> Option<Self> {
        match s {
            b"**" => Some(Self::Pow),
            b"*" => Some(Self::Mul),
            b"/" => Some(Self::Div),
            b"%" => Some(Self::Mod),
            b"+" => Some(Self::Add),
            b"-" => Some(Self::Sub),
            b">>" => Some(Self::Shr),
            b"<<" => Some(Self::Shl),
            b"^" => Some(Self::BitXor),
            b"&" => Some(Self::BitAnd),
            b"|" => Some(Self::BitOr),
            b"<" => Some(Self::Lt),
            b">" => Some(Self::Gt),
            b"<=" => Some(Self::Le),
            b">=" => Some(Self::Ge),
            b"==" => Some(Self::Eq),
            b"!=" => Some(Self::Neq),
            b"&&" => Some(Self::And),
            b"||" => Some(Self::Or),
            _ => None,
        }
    }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum UniOpType {
    Neg,
    Pos,
    Not,
    Ref,
    Deref,
}

impl UniOpType {
    #[inline]
    fn try_prefix(s: &str) -> Option<Self> {
        Self::try_prefix_buf(s.as_bytes())
    }

    fn try_prefix_buf(s: &[u8]) -> Option<Self> {
        match s {
            b"-" => Some(Self::Neg),
            b"+" => Some(Self::Pos),
            b"!" => Some(Self::Not),
            b"&" => Some(Self::Ref),
            b"*" => Some(Self::Deref),
            _ => None,
        }
    }

    #[allow(dead_code)]
    #[inline]
    fn try_suffix(s: &str) -> Option<Self> {
        Self::try_suffix_buf(s.as_bytes())
    }

    fn try_suffix_buf(s: &[u8]) -> Option<Self> {
        match s {
            b".-" => Some(Self::Neg),
            b".+" => Some(Self::Pos),
            b".!" => Some(Self::Not),
            b".&" => Some(Self::Ref),
            b".*" => Some(Self::Deref),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct DuoOp<'a> {
    pub left_expr: Box<Expr<'a>>,
    pub op: DuoOpType,
    pub right_expr: Box<Expr<'a>>,
    pub op_span: Span<'a>,
    pub total_span: Span<'a>,
}

#[derive(Debug)]
pub struct UniOp<'a> {
    pub op: UniOpType,
    pub expr: Box<Expr<'a>>,
    pub op_span: Span<'a>,
    pub total_span: Span<'a>,
}

#[derive(Debug)]
pub struct PropAccess<'a> {
    pub expr: Box<Expr<'a>>,
    pub prop: String,
    pub prop_span: Span<'a>,
    pub total_span: Span<'a>,
}

#[derive(Debug)]
pub struct TupleAccess<'a> {
    pub expr: Box<Expr<'a>>,
    pub element: u64,
    pub element_span: Span<'a>,
    pub total_span: Span<'a>,
}

#[derive(Debug)]
pub struct FnCall<'a> {
    pub expr: Box<Expr<'a>>,
    pub args: Vec<Expr<'a>>,
    pub args_span: Span<'a>,
    pub total_span: Span<'a>,
}

#[derive(Debug)]
pub struct Var<'a> {
    pub name: String,
    pub span: Span<'a>,
}

#[derive(Debug)]
pub struct Int<'a> {
    pub value: i64,
    pub span: Span<'a>,
}

#[derive(Debug)]
pub struct Float<'a> {
    pub value: f64,
    pub span: Span<'a>,
}

#[derive(Debug)]
pub struct Str<'a> {
    pub value: String,
    pub span: Span<'a>,
}

#[derive(Debug)]
pub struct Bool<'a> {
    pub value: bool,
    pub span: Span<'a>,
}

#[derive(Debug)]
pub struct List<'a> {
    pub values: Vec<Expr<'a>>,
    pub span: Span<'a>,
}

#[derive(Debug)]
pub struct Tuple<'a> {
    pub values: Vec<Expr<'a>>,
    pub span: Span<'a>,
}

#[derive(Debug)]
pub enum Expr<'a> {
    DuoOp(DuoOp<'a>),
    UniOp(UniOp<'a>),
    Var(Var<'a>),
    Int(Int<'a>),
    Float(Float<'a>),
    Str(Str<'a>),
    Bool(Bool<'a>),
    List(List<'a>),
    Tuple(Tuple<'a>),
    PropAccess(PropAccess<'a>),
    TupleAccess(TupleAccess<'a>),
    FnCall(FnCall<'a>),
}

impl<'a> Expr<'a> {
    pub fn span(&self) -> Span<'a> {
        match self {
            Self::DuoOp(duo_op) => duo_op.total_span,
            Self::UniOp(uni_op) => uni_op.total_span,
            Self::Var(var) => var.span,
            Self::Int(int) => int.span,
            Self::Float(float) => float.span,
            Self::Str(s) => s.span,
            Self::Bool(b) => b.span,
            Self::List(list) => list.span,
            Self::Tuple(tuple) => tuple.span,
            Self::PropAccess(prop_access) => prop_access.total_span,
            Self::TupleAccess(tuple_access) => tuple_access.total_span,
            Self::FnCall(fn_call) => fn_call.total_span,
        }
    }
}

#[derive(Debug)]
pub struct Assign<'a> {
    pub var: Var<'a>,
    pub ty: Option<Expr<'a>>,
    pub value: Expr<'a>,
    pub span: PhantomData<Span<'a>>,
}

#[derive(Debug)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
    Assign(Assign<'a>),
}

pub enum Module<'a> {
    _PlaceHolder(Span<'a>),
}

// Reserved puncts: $ : , . ; # ~ =
// Op puncts: - + * / % ** & | && || ^ == != < > <= >=
// Uni op punts: -- !
// left: ~ ` ? \
//
// a = Span(path, start + i + 1, start + char_indices.next())
// $ cargo run $()

enum SepSignal {
    LastCorrect,
    Correct,
    // LastIncorrect,
    Incorrect,
}

fn sep_puncts<TryConvert: FnMut(&[u8]) -> SepSignal>(
    mut punct_s: &[u8],
    mut f: TryConvert,
) -> &[u8] {
    'outer_loop: while !punct_s.is_empty() {
        let mut s = punct_s;
        loop {
            if s.is_empty() {
                break 'outer_loop;
            } else {
                match f(s) {
                    SepSignal::LastCorrect => {
                        punct_s = &punct_s[s.len()..];
                        break 'outer_loop;
                    }
                    // SepSignal::LastIncorrect => {
                    //     break 'outer_loop;
                    // }
                    SepSignal::Correct => break,
                    SepSignal::Incorrect => s = &s[..s.len() - 1],
                }
            }
        }
        punct_s = &punct_s[s.len()..];
    }
    punct_s
}

#[derive(Debug)]
struct LeftoverPunct<'a, 'b> {
    punct: &'b [u8],
    span: Span<'a>,
}

#[derive(Debug)]
pub enum ParseError<'a> {
    UnknownPrefixOp { op: String, span: Span<'a> },
    UnexpectedPunct { punct: String, span: Span<'a> },
    Expected { name: &'static str, span: Span<'a> },
    NegativeTupleAccess { span: Span<'a> },
}

impl<'a> ParseError<'a> {
    pub fn span(&self) -> Span<'a> {
        match *self {
            Self::UnknownPrefixOp { span, .. } => span,
            Self::UnexpectedPunct { span, .. } => span,
            Self::Expected { span, .. } => span,
            Self::NegativeTupleAccess { span, .. } => span,
        }
    }
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::UnknownPrefixOp { ref op, span } => {
                write!(f, "Unknown prefix operator `{op}` at '{span}'")
            }
            Self::UnexpectedPunct { ref punct, span } => {
                write!(f, "Unexpected punctuation `{punct}` at '{span}'")
            }
            Self::Expected { ref name, span } => {
                write!(f, "Expected {name} at '{span}'")
            }
            Self::NegativeTupleAccess { span } => {
                write!(f, "Found negative tuple access at '{span}'")
            }
        }
    }
}

impl<'a> Error for ParseError<'a> {}

/// Parses a value:
/// {prefix ops}{value}{suffix ops}
/// # Example
/// Turn this Kash2 value:
/// ```
/// -f(2).friend
/// ```
/// Into this Kash2 AST:
/// ```rust
/// UniOp {
///     ty: Neg,
///     expr: ParamAccess {
///         expr: FnCall {
///             expr: Var { name: "f" },
///             args: List {
///                 exprs: [Int { value: 2 }],
///             },
///         },
///         param: "friend",
///     },
/// }
/// ```
fn parse_val<'a, 'b>(
    tokens: &mut &'b [TokenTree<'a>],
    leftover_punct: Option<LeftoverPunct<'a, 'b>>,
) -> Result<(Expr<'a>, Option<LeftoverPunct<'a, 'b>>), ParseError<'a>> {
    let tokens_span = tokens[0].span().union(tokens.last().unwrap().span());
    let initial_tokens = *tokens;
    let mut prefix_ops = vec![];

    // Convert `leftover_puncts` into `prefix_ops`
    if let Some(LeftoverPunct { punct, mut span }) = leftover_punct {
        let leftover_punct_s = sep_puncts(punct, |s| {
            let current_span = Span {
                end: span.start + s.len(),
                ..span
            };
            span.start = current_span.end;
            if let Some(ty) = UniOpType::try_prefix_buf(s) {
                prefix_ops.push((ty, current_span));
                SepSignal::Correct
            } else {
                SepSignal::Incorrect
            }
        });
        if !leftover_punct_s.is_empty() {
            *tokens = initial_tokens;
            return Err(ParseError::UnknownPrefixOp {
                op: String::from_utf8(leftover_punct_s.to_vec()).unwrap(),
                span: Span {
                    start: span.start + punct.len() - leftover_punct_s.len(),
                    ..span
                },
            });
        }
    }
    // Make as many tokens as possible into `prefix_ops`
    loop {
        if let Some(token) = tokens.first() {
            match *token {
                TokenTree::Ident(lexer::Ident { ref name, span }) => {
                    if let Some(ty) = UniOpType::try_prefix(name) {
                        prefix_ops.push((ty, span));
                    } else {
                        break;
                    }
                }
                TokenTree::Punct7(lexer::Punct7 { mut span, .. })
                | TokenTree::PunctString(lexer::PunctString { mut span, .. }) => {
                    let punct = match token {
                        TokenTree::Punct7(lexer::Punct7 { punct, .. }) => punct.as_bytes(),
                        TokenTree::PunctString(lexer::PunctString { punct, .. }) => {
                            punct.as_bytes()
                        }
                        _ => unreachable!(),
                    };
                    let leftover_punct_s = sep_puncts(punct, |s| {
                        let current_span = Span {
                            end: span.start + s.len(),
                            ..span
                        };
                        span.start = current_span.end;
                        if let Some(ty) = UniOpType::try_prefix_buf(s) {
                            prefix_ops.push((ty, current_span));
                            SepSignal::Correct
                        } else {
                            SepSignal::Incorrect
                        }
                    });
                    if !leftover_punct_s.is_empty() {
                        *tokens = initial_tokens;
                        return Err(ParseError::UnknownPrefixOp {
                            op: String::from_utf8(leftover_punct_s.to_owned()).unwrap(),
                            span: Span {
                                start: span.start + punct.len() - leftover_punct_s.len(),
                                ..span
                            },
                        });
                    }
                }
                _ => break,
            }
            *tokens = &tokens[1..];
        } else {
            return Err(ParseError::Expected {
                name: "a value",
                span: Span {
                    start: tokens_span.end,
                    ..tokens_span
                },
            });
        }
    }

    // Get the value, pretty obvious
    let mut value = Some(match *tokens.first().unwrap() {
        TokenTree::Ident(lexer::Ident { ref name, span }) => match &**name {
            "false" => Expr::Bool(Bool { value: false, span }),
            "true" => Expr::Bool(Bool { value: true, span }),
            _ => Expr::Var(Var {
                name: name.clone(),
                span,
            }),
        },
        TokenTree::IntLiteral(lexer::IntLiteral { value, span }) => Expr::Int(Int { value, span }),
        TokenTree::FloatLiteral(lexer::FloatLiteral { value, span }) => {
            Expr::Float(Float { value, span })
        }
        TokenTree::StrLiteral(lexer::StrLiteral { ref value, span }) => Expr::Str(Str {
            value: value.clone(),
            span,
        }),
        TokenTree::Group(lexer::Group {
            delim: lexer::Delimiter::Parentheses,
            ref tokens,
            span,
        }) => {
            // let mut ref_tokens = &**tokens;
            // let (expr, leftover_punct) = parse_expr(&mut ref_tokens, None);
            // assert!(leftover_punct.is_none());
            // assert!(ref_tokens.is_empty());
            let (mut exprs, has_comma) = parse_expr_list(tokens)?;
            if has_comma {
                Expr::Tuple(Tuple {
                    values: exprs,
                    span,
                })
            } else if let Some(expr) = exprs.pop() {
                debug_assert!(exprs.is_empty());
                expr
            } else {
                Expr::Tuple(Tuple {
                    values: vec![],
                    span,
                })
            }
        }
        TokenTree::Group(lexer::Group {
            delim: lexer::Delimiter::Brackets,
            ref tokens,
            span,
        }) => Expr::List(List {
            values: parse_expr_list(tokens)?.0,
            span,
        }),
        ref token => {
            let token_span = token.span();
            return Err(ParseError::Expected {
                name: "a value",
                span: Span {
                    end: token_span.start,
                    ..token_span
                },
            });
        }
    });
    *tokens = &tokens[1..];

    // Make as many tokens as possible into suffix ops
    let mut leftover_punct = None;
    loop {
        if let Some(token) = tokens.first() {
            match *token {
                TokenTree::Punct7(lexer::Punct7 {
                    span: mut punct_span,
                    ..
                })
                | TokenTree::PunctString(lexer::PunctString {
                    span: mut punct_span,
                    ..
                }) => {
                    let punct = match token {
                        TokenTree::Punct7(lexer::Punct7 { punct, .. }) => punct.as_bytes(),
                        TokenTree::PunctString(lexer::PunctString { punct, .. }) => {
                            punct.as_bytes()
                        }
                        _ => unreachable!(),
                    };

                    match sep_puncts(&punct[..punct.len()], |s| {
                        let current_span = Span {
                            end: punct_span.start + s.len(),
                            ..punct_span
                        };
                        punct_span.start = current_span.end;
                        if let Some(ty) = UniOpType::try_suffix_buf(s) {
                            let expr = value.take().unwrap();
                            let total_span = expr.span().union(current_span);
                            value = Some(Expr::UniOp(UniOp {
                                op: ty,
                                expr: Box::new(expr),
                                op_span: current_span,
                                total_span,
                            }));
                            SepSignal::Correct
                        } else {
                            SepSignal::Incorrect
                        }
                    }) {
                        b"" => {}
                        b"." => {
                            *tokens = &tokens[1..];
                            match tokens[0] {
                                TokenTree::Ident(lexer::Ident {
                                    ref name,
                                    span: prop_span,
                                }) => {
                                    let expr = value.take().unwrap();
                                    let total_span = expr.span().union(prop_span);
                                    value = Some(Expr::PropAccess(PropAccess {
                                        expr: Box::new(expr),
                                        prop: name.clone(),
                                        prop_span: punct_span.union(prop_span),
                                        total_span,
                                    }));
                                }
                                TokenTree::IntLiteral(lexer::IntLiteral {
                                    value: element,
                                    span: element_span,
                                }) => {
                                    if element < 0 {
                                        return Err(ParseError::NegativeTupleAccess {
                                            span: element_span,
                                        });
                                    }
                                    let expr = value.take().unwrap();
                                    let total_span = expr.span().union(element_span);
                                    value = Some(Expr::TupleAccess(TupleAccess {
                                        expr: Box::new(expr),
                                        element: element as _,
                                        element_span: punct_span.union(element_span),
                                        total_span,
                                    }));
                                }
                                _ => todo!(),
                            }
                        }
                        leftover_punct_s => {
                            if leftover_punct_s.len() < punct.len() {
                                leftover_punct = Some(LeftoverPunct {
                                    punct: leftover_punct_s,
                                    span: Span {
                                        start: punct_span.start + punct.len()
                                            - leftover_punct_s.len(),
                                        ..punct_span
                                    },
                                });
                                *tokens = &tokens[1..];
                            }
                            break;
                        }
                    }
                }
                TokenTree::Group(lexer::Group {
                    delim: lexer::Delimiter::Parentheses,
                    ref tokens,
                    span,
                }) => {
                    let expr = value.take().unwrap();
                    let total_span = expr.span().union(span);
                    value = Some(Expr::FnCall(FnCall {
                        expr: Box::new(expr),
                        args: parse_expr_list(tokens)?.0,
                        args_span: span,
                        total_span,
                    }));
                }
                _ => break,
            }
            *tokens = &tokens[1..];
        } else {
            break;
        }
    }

    // Apply prefix ops
    for (ty, op_span) in prefix_ops.into_iter().rev() {
        let expr = value.take().unwrap();
        let total_span = expr.span().union(op_span);
        value = Some(Expr::UniOp(UniOp {
            op: ty,
            expr: Box::new(expr),
            op_span,
            total_span,
        }))
    }

    Ok((value.unwrap(), leftover_punct))
}

pub fn pub_parse_expr<'a>(tokens: &mut &[TokenTree<'a>], file: &str) -> Expr<'a> {
    let (expr, leftover_punct) = match parse_expr(tokens, None) {
        Ok(out) => out,
        Err(err) => {
            err.span().display(&lines(file), file);
            panic!("Failed to parse expression with error: {err}");
        }
    };
    assert!(leftover_punct.is_none());
    expr
}

fn parse_expr<'a, 'b>(
    tokens: &mut &'b [TokenTree<'a>],
    mut leftover_punct: Option<LeftoverPunct<'a, 'b>>,
) -> Result<(Expr<'a>, Option<LeftoverPunct<'a, 'b>>), ParseError<'a>> {
    let value;
    (value, leftover_punct) = parse_val(tokens, leftover_punct.take())?;
    let mut values = vec![value];
    let mut ops: Vec<(DuoOpType, Span<'a>)> = vec![];
    while !tokens.is_empty() {
        let op;
        let op_span;
        if let Some(LeftoverPunct { punct, span }) = leftover_punct.take() {
            let mut maybe_op = None;
            let leftover_punct_s = sep_puncts(punct, |s| {
                maybe_op = DuoOpType::try_from_buf(s);
                if maybe_op.is_some() {
                    SepSignal::LastCorrect
                } else {
                    SepSignal::Incorrect
                }
            });
            op_span = Span {
                end: span.start + punct.len() - leftover_punct_s.len(),
                ..span
            };
            if let Some(maybe_op) = maybe_op {
                op = maybe_op;
            } else {
                leftover_punct = Some(LeftoverPunct { punct, span });
                break;
            }
            if !leftover_punct_s.is_empty() {
                leftover_punct = Some(LeftoverPunct {
                    punct: leftover_punct_s,
                    span: Span {
                        start: op_span.end,
                        ..span
                    },
                });
            }
        } else {
            match tokens[0] {
                TokenTree::Punct7(lexer::Punct7 { span, .. })
                | TokenTree::PunctString(lexer::PunctString { span, .. }) => {
                    let punct = match &tokens[0] {
                        TokenTree::Punct7(lexer::Punct7 { punct, .. }) => punct.as_bytes(),
                        TokenTree::PunctString(lexer::PunctString { punct, .. }) => {
                            punct.as_bytes()
                        }
                        _ => unreachable!(),
                    };
                    let mut maybe_op = None;
                    let leftover_punct_s = sep_puncts(punct, |s| {
                        maybe_op = DuoOpType::try_from_buf(s);
                        if maybe_op.is_some() {
                            SepSignal::LastCorrect
                        } else {
                            SepSignal::Incorrect
                        }
                    });
                    op_span = Span {
                        end: span.start + punct.len() - leftover_punct_s.len(),
                        ..span
                    };
                    if let Some(maybe_op) = maybe_op {
                        op = maybe_op;
                    } else {
                        break;
                    }
                    if !leftover_punct_s.is_empty() {
                        leftover_punct = Some(LeftoverPunct {
                            punct: leftover_punct_s,
                            span: Span {
                                start: op_span.end,
                                ..span
                            },
                        });
                    }
                }
                _ => break,
            }
            *tokens = &tokens[1..];
        };
        // println!("Add op: `{:?}`", current_op);
        while let Some(&(last_op, last_op_span)) = ops.last() {
            if last_op.level() <= op.level() {
                ops.pop().unwrap();
                let right_expr = Box::new(values.pop().unwrap());
                let left_expr = Box::new(values.pop().unwrap());
                let total_span = left_expr.span().union(right_expr.span());
                // println!("resolve op: {last_op:?}");
                values.push(Expr::DuoOp(DuoOp {
                    op: last_op,
                    left_expr,
                    right_expr,
                    op_span: last_op_span,
                    total_span,
                }));
                // println!("new value: {:#?}", values.last().unwrap());
            } else {
                break;
            }
        }
        ops.push((op, op_span));

        let value;
        (value, leftover_punct) = parse_val(tokens, leftover_punct.take())?;
        // println!("Add value: `{:?}`", value);
        values.push(value);
    }
    while let Some((op, op_span)) = ops.pop() {
        let right_expr = Box::new(values.pop().unwrap());
        let left_expr = Box::new(values.pop().unwrap());
        let total_span = left_expr.span().union(right_expr.span());
        // println!("resolve op: {op:?}");
        values.push(Expr::DuoOp(DuoOp {
            op,
            left_expr,
            right_expr,
            op_span,
            total_span,
        }));
        // println!("new value: {:#?}", values.last().unwrap());
    }

    debug_assert_eq!(values.len(), 1);
    Ok((values.pop().unwrap(), leftover_punct))
}

fn _parse_tupling_expr<'a, 'b>(
    _tokens: &mut &'b [TokenTree<'a>],
    mut _leftover_punct: Option<LeftoverPunct<'a, 'b>>,
) -> (Expr<'a>, Option<LeftoverPunct<'a, 'b>>) {
    todo!()
}

/// Takes a list of tokens and parses them into a list of expressions.
///
/// # Returns
/// Returns a vector of the expressions and a bool specifying whether the list contains a comma.
pub fn parse_expr_list<'a>(
    mut tokens: &[TokenTree<'a>],
) -> Result<(Vec<Expr<'a>>, bool), ParseError<'a>> {
    let mut exprs = vec![];

    let mut has_comma = false;
    let mut leftover_punct = None;
    while !tokens.is_empty() {
        let expr;
        (expr, leftover_punct) = parse_expr(&mut tokens, leftover_punct)?;
        exprs.push(expr);

        if tokens.is_empty() {
            break;
        }

        let LeftoverPunct { punct, span } = if let Some(leftover_punct) = leftover_punct.take() {
            leftover_punct
        } else {
            let leftover_punct = match tokens[0] {
                TokenTree::Punct7(lexer::Punct7 { ref punct, span }) => LeftoverPunct {
                    punct: punct.as_bytes(),
                    span,
                },
                TokenTree::PunctString(lexer::PunctString { ref punct, span }) => LeftoverPunct {
                    punct: punct.as_bytes(),
                    span,
                },
                ref token => {
                    let token_span = token.span();
                    return Err(ParseError::Expected {
                        name: "`,`",
                        span: Span {
                            end: token_span.start,
                            ..token_span
                        },
                    });
                }
            };
            tokens = &tokens[1..];
            leftover_punct
        };
        if punct[0] != b',' {
            return Err(ParseError::Expected {
                name: "`,`",
                span: Span {
                    end: span.start,
                    ..span
                },
            });
        }
        if 1 < punct.len() {
            leftover_punct = Some(LeftoverPunct {
                punct: &punct[1..],
                span: Span {
                    start: span.start + 1,
                    ..span
                },
            });
        }
        has_comma = true;
    }
    if let Some(LeftoverPunct { punct, span }) = leftover_punct {
        return Err(ParseError::UnexpectedPunct {
            punct: String::from_utf8(punct.to_vec()).unwrap(),
            span,
        });
    }

    Ok((exprs, has_comma))
}

pub fn parse_stmt<'a>(_tokens: &mut &[TokenTree<'a>]) -> Stmt<'a> {
    todo!()
}

pub fn parse_module<'a>(mut _tokens: &[TokenTree<'a>]) -> Module<'a> {
    todo!();
}
