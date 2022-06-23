use crate::lexer::{self, TokenTree};
use crate::Span;
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

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
enum DuoOpLevelType {
    LeftToRight,
    #[allow(dead_code)]
    RightToLeft,
    RequireParens,
}

impl DuoOpType {
    #[inline]
    const fn level(self) -> u8 {
        self as u8 & 0xf0
    }

    #[inline]
    fn level_type(self) -> DuoOpLevelType {
        use DuoOpLevelType::*;
        unsafe {
            *[
                LeftToRight,   // 0: **
                LeftToRight,   // 1: * /
                LeftToRight,   // 2: + -
                LeftToRight,   // 3: >> <<
                LeftToRight,   // 4: ^
                LeftToRight,   // 5: &
                LeftToRight,   // 6: |
                RequireParens, // 7: < > <= >= == !=
                LeftToRight,   // 8: &&
                LeftToRight,   // 9: ||
                LeftToRight,   // a:
                LeftToRight,   // b:
                LeftToRight,   // c:
                LeftToRight,   // d:
                LeftToRight,   // e:
                LeftToRight,   // f:
            ]
            .get_unchecked(self as usize >> 4)
        }
    }

    #[allow(dead_code)]
    #[inline]
    fn try_from_str(s: &str) -> Option<Self> {
        Self::try_from_buf(s.as_bytes())
    }

    #[inline]
    const fn max_len() -> usize {
        2
    }

    fn try_from_tokens(tokens: &[TokenTree]) -> Option<Self> {
        match tokens {
            lexer::pat::puncts![b'*', b'*'] => Some(Self::Pow),
            lexer::pat::puncts![b'*'] => Some(Self::Mul),
            lexer::pat::puncts![b'/'] => Some(Self::Div),
            lexer::pat::puncts![b'%'] => Some(Self::Mod),
            lexer::pat::puncts![b'+'] => Some(Self::Add),
            lexer::pat::puncts![b'-'] => Some(Self::Sub),
            lexer::pat::puncts![b'>', b'>'] => Some(Self::Shr),
            lexer::pat::puncts![b'<', b'<'] => Some(Self::Shl),
            lexer::pat::puncts![b'^'] => Some(Self::BitXor),
            lexer::pat::puncts![b'&'] => Some(Self::BitAnd),
            lexer::pat::puncts![b'|'] => Some(Self::BitOr),
            lexer::pat::puncts![b'<'] => Some(Self::Lt),
            lexer::pat::puncts![b'>'] => Some(Self::Gt),
            lexer::pat::puncts![b'<', b'='] => Some(Self::Le),
            lexer::pat::puncts![b'>', b'='] => Some(Self::Ge),
            lexer::pat::puncts![b'=', b'='] => Some(Self::Eq),
            lexer::pat::puncts![b'!', b'='] => Some(Self::Neq),
            lexer::pat::puncts![b'&', b'&'] => Some(Self::And),
            lexer::pat::puncts![b'|', b'|'] => Some(Self::Or),
            _ => None,
        }
    }

    const fn try_from_buf(s: &[u8]) -> Option<Self> {
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
    const fn max_num_tokens() -> usize {
        2
    }

    #[inline]
    fn try_prefix_str(s: &str) -> Option<Self> {
        Self::try_prefix_buf(s.as_bytes())
    }

    fn try_prefix_tokens(tokens: &[TokenTree]) -> Option<Self> {
        match tokens {
            lexer::pat::puncts![b'-'] => Some(Self::Neg),
            lexer::pat::puncts![b'+'] => Some(Self::Pos),
            lexer::pat::puncts![b'!'] => Some(Self::Not),
            lexer::pat::puncts![b'&'] => Some(Self::Ref),
            lexer::pat::puncts![b'*'] => Some(Self::Deref),
            _ => None,
        }
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
    fn try_suffix_str(s: &str) -> Option<Self> {
        Self::try_suffix_buf(s.as_bytes())
    }

    fn try_suffix_tokens(tokens: &[TokenTree]) -> Option<Self> {
        match tokens {
            lexer::pat::puncts![b'.', b'-'] => Some(Self::Neg),
            lexer::pat::puncts![b'.', b'+'] => Some(Self::Pos),
            lexer::pat::puncts![b'.', b'!'] => Some(Self::Not),
            lexer::pat::puncts![b'.', b'&'] => Some(Self::Ref),
            lexer::pat::puncts![b'.', b'*'] => Some(Self::Deref),
            _ => None,
        }
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
pub struct SimpleIf<'a> {
    pub cond: Expr<'a>,
    pub then_block: Expr<'a>,
}

#[derive(Debug)]
pub struct If<'a> {
    pub ifs: Vec<SimpleIf<'a>>,
    pub else_block: Option<Box<Expr<'a>>>,
    pub span: Span<'a>,
}

#[derive(Debug)]
pub struct Return<'a> {
    pub span: Span<'a>,
    pub expr: Box<Expr<'a>>,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>,
    pub span: Span<'a>,
}

pub enum Expr<'a> {
    Block(Block<'a>),
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

    If(If<'a>),
    Return(Return<'a>),
}

impl<'a> fmt::Debug for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Expr::")?;
        match self {
            Self::Block(x) => fmt::Debug::fmt(x, f),
            Self::UniOp(x) => fmt::Debug::fmt(x, f),
            Self::DuoOp(x) => fmt::Debug::fmt(x, f),
            Self::Var(x) => fmt::Debug::fmt(x, f),
            Self::Int(x) => fmt::Debug::fmt(x, f),
            Self::Float(x) => fmt::Debug::fmt(x, f),
            Self::Str(x) => fmt::Debug::fmt(x, f),
            Self::Bool(x) => fmt::Debug::fmt(x, f),
            Self::List(x) => fmt::Debug::fmt(x, f),
            Self::Tuple(x) => fmt::Debug::fmt(x, f),
            Self::PropAccess(x) => fmt::Debug::fmt(x, f),
            Self::TupleAccess(x) => fmt::Debug::fmt(x, f),
            Self::FnCall(x) => fmt::Debug::fmt(x, f),
            Self::If(x) => fmt::Debug::fmt(x, f),
            Self::Return(x) => fmt::Debug::fmt(x, f),
        }
    }
}

impl<'a> Expr<'a> {
    pub fn span(&self) -> Span<'a> {
        match self {
            Self::Block(block) => block.span,
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
            Self::If(if_expr) => if_expr.span,
            Self::Return(return_expr) => return_expr.span,
        }
    }
}

#[derive(Debug)]
pub struct Assign<'a> {
    pub assigned: Vec<Expr<'a>>,
    pub value: Expr<'a>,
    pub span: Span<'a>,
}

pub enum Stmt<'a> {
    // Expr(Expr<'a>),
    Assign(Assign<'a>),
}

impl<'a> fmt::Debug for Stmt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Stmt::")?;
        match self {
            Self::Assign(x) => fmt::Debug::fmt(x, f),
        }
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SepSignal {
    FinalCorrect,
    Correct,
    // FinalIncorrect,
    Incorrect,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SepOut {
    NoTokens,
    NoMatch,
    FinalCorrect,
    // FinalIncorrect,
}

fn result_sep<'a, Err, TryConvert: FnMut(&[TokenTree<'a>]) -> Result<SepSignal, Err>>(
    tokens: &mut &[TokenTree<'a>],
    max_len: usize,
    mut f: TryConvert,
) -> Result<SepOut, Err> {
    while !tokens.is_empty() {
        let mut sub_tokens = &tokens[..max_len.min(tokens.len())];
        loop {
            if sub_tokens.is_empty() {
                return Ok(SepOut::NoMatch);
            } else {
                match f(sub_tokens)? {
                    SepSignal::FinalCorrect => {
                        *tokens = &tokens[sub_tokens.len()..];
                        return Ok(SepOut::FinalCorrect);
                    }
                    // SepSignal::LastIncorrect => {
                    //     return Ok(true);
                    //     break 'outer_loop;
                    // }
                    SepSignal::Correct => break,
                    SepSignal::Incorrect => sub_tokens = &sub_tokens[..sub_tokens.len() - 1],
                }
            }
        }
        *tokens = &tokens[sub_tokens.len()..];
    }
    Ok(SepOut::NoTokens)
}
#[inline]
fn sep<'a, TryConvert: FnMut(&[TokenTree<'a>]) -> SepSignal>(
    tokens: &mut &[TokenTree<'a>],
    max_len: usize,
    mut f: TryConvert,
) {
    result_sep::<(), _>(tokens, max_len, |tokens| Ok(f(tokens))).unwrap();
}

#[derive(Debug)]
pub enum ParseError<'a> {
    UnknownPrefixOp { op: String, span: Span<'a> },
    UnexpectedPunct { punct: String, span: Span<'a> },
    Expected { name: &'static str, span: Span<'a> },
    NegativeTupleAccess { span: Span<'a> },
    ChainedParenthesisOnly { span1: Span<'a>, span2: Span<'a> },
    InvalidLeftHandOfAssignment { span: Span<'a> },
}

impl<'a> ParseError<'a> {
    pub fn spans(&self) -> Vec<Span<'a>> {
        match *self {
            Self::UnknownPrefixOp { span, .. } => vec![span],
            Self::UnexpectedPunct { span, .. } => vec![span],
            Self::Expected { span, .. } => vec![span],
            Self::NegativeTupleAccess { span, .. } => vec![span],
            Self::ChainedParenthesisOnly { span1, span2 } => vec![span1, span2],
            Self::InvalidLeftHandOfAssignment { span } => vec![span],
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
            Self::ChainedParenthesisOnly { span1, span2 } => {
                write!(f, "Found chained operators that can't be chained at '{span1}' and '{span2}', add parentheses to fix it")
            }
            Self::InvalidLeftHandOfAssignment { span } => {
                write!(f, "Invalid left-hand side of assignment at '{span}'")
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
fn parse_val<'a, 'b>(tokens: &mut &'b [TokenTree<'a>]) -> Result<Expr<'a>, ParseError<'a>> {
    let mut inner_tokens = *tokens;
    // let tokens_span = tokens[0].span().union(tokens.last().unwrap().span());
    let mut prefix_ops = vec![];

    // Make as many tokens as possible into `prefix_ops`
    sep(
        &mut inner_tokens,
        UniOpType::max_num_tokens(),
        |tokens| match tokens {
            lexer::pat::puncts![END] => SepSignal::Correct,
            tokens => {
                if let Some(ty) = UniOpType::try_prefix_tokens(tokens) {
                    prefix_ops.push((ty, tokens[0].span().union(tokens[tokens.len() - 1].span())));
                    SepSignal::Correct
                } else {
                    SepSignal::Incorrect
                }
            }
        },
    );
    let value_token;
    (value_token, inner_tokens) =
        inner_tokens
            .split_first()
            .ok_or_else(|| ParseError::Expected {
                name: "a value",
                span: prefix_ops
                    .last()
                    .expect("No tokens were passed to `parser::parse_val`")
                    .1
                    .ending(),
            })?;
    // Get the value, pretty obvious
    let mut value = Some(match *value_token {
        lexer::pat::ident!({ ref name, span }) => match name.as_str() {
            "false" => Expr::Bool(Bool { value: false, span }),
            "true" => Expr::Bool(Bool { value: true, span }),
            "if" => {
                let mut simple_ifs = vec![];
                loop {
                    let cond = parse_expr(&mut inner_tokens)?;
                    let then_token;
                    (then_token, inner_tokens) =
                        inner_tokens
                            .split_first()
                            .ok_or_else(|| ParseError::Expected {
                                name: "a 'then block' for the `if` expression",
                                span: cond.span().ending(),
                            })?;
                    let then_block;
                    then_block = match *then_token {
                        lexer::pat::punct!(b':') => parse_expr(&mut inner_tokens)?,
                        lexer::pat::group!({
                            delim: lexer::Delimiter::Braces,
                            ref tokens,
                            span,
                        }) => Expr::Block(Block {
                            stmts: parse_block(tokens)?,
                            span,
                        }),
                        _ => {
                            return Err(ParseError::Expected {
                                name: "a 'then block' for the `if` expression",
                                span: cond.span().ending(),
                            });
                        }
                    };
                    simple_ifs.push(SimpleIf { cond, then_block });
                    match inner_tokens.first() {
                        Some(lexer::pat::ident!({ ref name, .. })) if name == "elif" => {
                            inner_tokens = &inner_tokens[1..];
                        }
                        _ => break,
                    }
                }
                let else_block;
                match inner_tokens.split_first() {
                    Some((lexer::pat::ident!({ ref name, .. }), new_tokens)) if name == "else" => {
                        inner_tokens = new_tokens;
                        let else_token;
                        (else_token, inner_tokens) =
                            inner_tokens
                                .split_first()
                                .ok_or_else(|| ParseError::Expected {
                                    name: "an 'else block' for the `if-else` expression",
                                    span: simple_ifs.last().unwrap().then_block.span().ending(),
                                })?;
                        else_block = Some(match *else_token {
                            lexer::pat::punct!(b':') => parse_expr(&mut inner_tokens)?,
                            lexer::pat::group!({
                                delim: lexer::Delimiter::Braces,
                                ref tokens,
                                span,
                            }) => Expr::Block(Block {
                                stmts: parse_block(tokens)?,
                                span,
                            }),
                            _ => {
                                return Err(ParseError::Expected {
                                    name: "an 'else block' for the `if-else` expression",
                                    span: else_token.span().begining(),
                                });
                            }
                        });
                    }
                    _ => {
                        else_block = None;
                    }
                }

                Expr::If(If {
                    span: span.union(
                        else_block
                            .as_ref()
                            .map(Expr::span)
                            .unwrap_or(simple_ifs.last().unwrap().then_block.span()),
                    ),
                    ifs: simple_ifs,
                    else_block: else_block.map(Box::new),
                })
            }
            "return" => {
                let expr = parse_tupling_expr(&mut inner_tokens)?;
                Expr::Return(Return {
                    span: span.union(expr.span()),
                    expr: Box::new(expr),
                })
            }
            _ => Expr::Var(Var {
                name: name.clone(),
                span,
            }),
        },
        lexer::pat::int!({ value, span }) => Expr::Int(Int { value, span }),
        lexer::pat::float!({ value, span }) => Expr::Float(Float { value, span }),
        lexer::pat::str!({ ref value, span }) => Expr::Str(Str {
            value: value.clone(),
            span,
        }),
        lexer::pat::group!({
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
        lexer::pat::group!({
            delim: lexer::Delimiter::Brackets,
            ref tokens,
            span,
        }) => Expr::List(List {
            values: parse_expr_list(tokens)?.0,
            span,
        }),
        lexer::pat::group!({
            delim: lexer::Delimiter::Braces,
            ref tokens,
            span,
        }) => Expr::Block(Block {
            stmts: parse_block(tokens)?,
            span,
        }),
        ref token => {
            return Err(ParseError::Expected {
                name: "a value",
                span: token.span().begining(),
            });
        }
    });

    // Make as many tokens as possible into suffix ops
    result_sep(
        &mut inner_tokens,
        UniOpType::max_num_tokens().max(3),
        |tokens| {
            Ok(match tokens {
                lexer::pat::puncts![END] => SepSignal::Correct,
                lexer::pat::tokens![
                    punct: { ch: b'.', span: punct_span },
                    punct: END,
                    ident: { ref name, span: prop_span },
                ] => {
                    let expr = value.take().unwrap();
                    let total_span = expr.span().union(prop_span);
                    value = Some(Expr::PropAccess(PropAccess {
                        expr: Box::new(expr),
                        prop: name.clone(),
                        prop_span: punct_span.union(prop_span),
                        total_span,
                    }));
                    SepSignal::Correct
                }
                lexer::pat::tokens![
                    punct: { ch: b'.', span: punct_span },
                    punct: END,
                    int: { value: element, span: element_span }
                ] => {
                    if element < 0 {
                        return Err(ParseError::NegativeTupleAccess { span: element_span });
                    } else {
                        let expr = value.take().unwrap();
                        let total_span = expr.span().union(element_span);
                        value = Some(Expr::TupleAccess(TupleAccess {
                            expr: Box::new(expr),
                            element: element as _,
                            element_span: punct_span.union(element_span),
                            total_span,
                        }));
                        SepSignal::Correct
                    }
                }
                lexer::pat::tokens![group: {
                    delim: lexer::Delimiter::Parentheses,
                    ref tokens,
                    span,
                }] => {
                    let expr = value.take().unwrap();
                    let total_span = expr.span().union(span);
                    value = Some(Expr::FnCall(FnCall {
                        expr: Box::new(expr),
                        args: parse_expr_list(tokens)?.0,
                        args_span: span,
                        total_span,
                    }));
                    SepSignal::Correct
                }
                tokens => {
                    if let Some(ty) = UniOpType::try_suffix_tokens(tokens) {
                        let current_span = tokens[0].span().union(tokens[tokens.len() - 1].span());
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
                }
            })
        },
    )?;

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

    *tokens = inner_tokens;
    Ok(value.unwrap())
}

pub fn pub_parse_expr<'a>(
    tokens: &mut &[TokenTree<'a>],
    file: &str,
    file_lines: &[usize],
) -> Expr<'a> {
    let expr = match parse_tupling_expr(tokens) {
        Ok(out) => out,
        Err(err) => {
            for span in err.spans() {
                span.display(file_lines, file);
            }
            panic!("Failed to parse expression with error: {err}");
        }
    };
    expr
}

fn parse_expr<'a>(tokens: &mut &[TokenTree<'a>]) -> Result<Expr<'a>, ParseError<'a>> {
    let mut inner_tokens = *tokens;

    let value = parse_val(&mut inner_tokens)?;
    let mut values = vec![value];
    let mut ops: Vec<(DuoOpType, Span<'a>)> = vec![];
    while !inner_tokens.is_empty() {
        let mut op_x = None;
        result_sep(
            &mut inner_tokens,
            DuoOpType::max_len(),
            |tokens| match tokens {
                lexer::pat::puncts![END] => Ok(SepSignal::Correct),
                tokens => {
                    if let Some(op_type) = DuoOpType::try_from_tokens(tokens) {
                        op_x = Some((
                            op_type,
                            tokens[0].span().union(tokens.last().unwrap().span()),
                        ));
                        Ok(SepSignal::FinalCorrect)
                    } else {
                        Ok(SepSignal::Incorrect)
                    }
                }
            },
        )?;
        let (op, op_span) = match op_x {
            Some(x) => x,
            None => break,
        };
        while let Some(&(last_op, last_op_span)) = ops.last() {
            if last_op.level() < op.level()
                || (last_op.level() == op.level() && op.level_type() == DuoOpLevelType::LeftToRight)
            {
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
            } else if last_op.level() == op.level()
                && op.level_type() == DuoOpLevelType::RequireParens
            {
                return Err(ParseError::ChainedParenthesisOnly {
                    span1: last_op_span,
                    span2: op_span,
                });
            } else {
                break;
            }
        }
        ops.push((op, op_span));

        // println!("Add value: `{:?}`", value);
        values.push(parse_val(&mut inner_tokens)?);
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

    *tokens = inner_tokens;
    Ok(values.pop().unwrap())
}

fn parse_tupling_expr<'a>(tokens: &mut &[TokenTree<'a>]) -> Result<Expr<'a>, ParseError<'a>> {
    let mut inner_tokens = *tokens;

    let mut exprs = vec![parse_expr(&mut inner_tokens)?];
    let mut has_comma = false;
    while !inner_tokens.is_empty() {
        match inner_tokens.first() {
            Some(lexer::pat::punct!(b',')) => {
                inner_tokens = &inner_tokens[1..];
                has_comma = true;
            }
            _ => {
                break;
            }
        };

        match parse_expr(&mut inner_tokens) {
            Ok(expr) => exprs.push(expr),
            Err(_) => break,
        }
    }

    *tokens = inner_tokens;
    if has_comma {
        Ok(Expr::Tuple(Tuple {
            span: exprs[0].span().union(exprs.last().unwrap().span()),
            values: exprs,
        }))
    } else {
        assert_eq!(exprs.len(), 1);
        Ok(exprs.pop().unwrap())
    }
}

/// Takes a list of tokens and parses them into a list of expressions.
///
/// # Returns
/// Returns a vector of the expressions and a bool specifying whether the list contains a comma.
fn parse_expr_list<'a>(
    mut tokens: &[TokenTree<'a>],
) -> Result<(Vec<Expr<'a>>, bool), ParseError<'a>> {
    let mut exprs = vec![];

    let mut has_comma = false;
    while !tokens.is_empty() {
        let expr = parse_expr(&mut tokens)?;
        exprs.push(expr);

        if tokens.is_empty() {
            break;
        }

        match tokens[0] {
            lexer::pat::punct![b','] => {
                tokens = &tokens[1..];
                has_comma = true;
            }
            ref token => {
                return Err(ParseError::Expected {
                    name: "`,`",
                    span: token.span().begining(),
                });
            }
        };
    }

    Ok((exprs, has_comma))
}

pub fn parse_assign_stmt<'a>(tokens: &mut &[TokenTree<'a>]) -> Result<Assign<'a>, ParseError<'a>> {
    let mut inner_tokens = *tokens;
    fn check(expr: &Expr) -> bool {
        match *expr {
            Expr::Var(_) => true,
            Expr::PropAccess(_) => true,
            Expr::TupleAccess(_) => true,
            Expr::FnCall(_) => true,
            Expr::UniOp(UniOp {
                op: UniOpType::Deref,
                ..
            }) => true,
            Expr::Tuple(Tuple { ref values, .. }) => values.iter().all(check),
            Expr::List(List { ref values, .. }) => values.iter().all(check),
            _ => false,
        }
    }

    let mut assigned = vec![];
    loop {
        let expr = parse_tupling_expr(&mut inner_tokens)?;
        if result_sep(&mut inner_tokens, 2, |tokens| match tokens {
            lexer::pat::puncts![END] => Ok(SepSignal::Correct),
            lexer::pat::puncts![b'=', END] | lexer::pat::puncts![b'='] => {
                Ok(SepSignal::FinalCorrect)
            }
            _ => Ok(SepSignal::Incorrect),
        })? == SepOut::FinalCorrect
        {
            if check(&expr) {
                assigned.push(expr);
            } else {
                return Err(ParseError::InvalidLeftHandOfAssignment { span: expr.span() });
            }
        } else {
            assigned.push(expr);
            break;
        }
    }
    let mut span = assigned.first().unwrap().span();
    if result_sep(&mut inner_tokens, 1, |tokens| match tokens {
        lexer::pat::puncts![END] => Ok(SepSignal::Correct),
        lexer::pat::new_lines![{ span: nl_span }] => {
            span = span.union(nl_span);
            Ok(SepSignal::FinalCorrect)
        }
        _ => Ok(SepSignal::Incorrect),
    })? == SepOut::FinalCorrect
    {
        *tokens = inner_tokens;
        Ok(Assign {
            span,
            value: assigned.pop().unwrap(),
            assigned,
        })
    } else {
        Err(ParseError::Expected {
            name: "a new line",
            span: span.ending(),
        })
    }
}

pub fn parse_stmt<'a>(tokens: &mut &[TokenTree<'a>]) -> Result<Stmt<'a>, ParseError<'a>> {
    Ok(Stmt::Assign(parse_assign_stmt(tokens)?))
}

pub fn parse_block<'a>(mut tokens: &[TokenTree<'a>]) -> Result<Vec<Stmt<'a>>, ParseError<'a>> {
    let mut stmts = vec![];
    while let Some(lexer::pat::punct!(END) | lexer::pat::new_line!()) = tokens.first() {
        tokens = &tokens[1..];
    }
    while !tokens.is_empty() {
        stmts.push(parse_stmt(&mut tokens)?);
        while let Some(lexer::pat::punct!(END) | lexer::pat::new_line!()) = tokens.first() {
            tokens = &tokens[1..];
        }
    }
    Ok(stmts)
}

pub fn parse_module<'a>(mut _tokens: &[TokenTree<'a>]) -> Module<'a> {
    todo!();
}
