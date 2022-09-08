use crate::lexer::{self, TokenTree};
use crate::{Span, Spanned};
use std::{error::Error, fmt, mem, ops, slice};

const KEYWORDS: &[&str] = &[
    "true", "false", "if", "else", "for", "while", "loop", "return", "fn", "let", "mut", "_",
];

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
    // Pipe   = 0x00,
    // Pow    = 0x10,
    // Mul    = 0x20,
    // Div    = 0x21,
    // Mod    = 0x22,
    // Add    = 0x30,
    // Sub    = 0x31,
    // Shr    = 0x40,
    // Shl    = 0x41,
    // BitXor = 0x50,
    // BitAnd = 0x60,
    // BitOr  = 0x70,
    // Lt     = 0x80,
    // Gt     = 0x81,
    // Le     = 0x82,
    // Ge     = 0x83,
    // Eq     = 0x84,
    // Neq    = 0x85,
    // And    = 0x90,
    // Or     = 0xA0,
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

    // fn try_prefix_buf(s: &[u8]) -> Option<Self> {
    //     match s {
    //         b"-" => Some(Self::Neg),
    //         b"+" => Some(Self::Pos),
    //         b"!" => Some(Self::Not),
    //         b"&" => Some(Self::Ref),
    //         b"*" => Some(Self::Deref),
    //         _ => None,
    //     }
    // }

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
    pub value: u64,
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
pub struct If<'a> {
    pub cond: Box<Expr<'a>>,
    pub then_block: Box<Expr<'a>>,
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

#[derive(Debug)]
pub struct LabeledExpr<'a> {
    pub label: String,
    pub expr: Box<Expr<'a>>,
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

    LabeledExpr(LabeledExpr<'a>),
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
            Self::LabeledExpr(x) => fmt::Debug::fmt(x, f),
        }
    }
}

impl<'a> Spanned<'a> for Expr<'a> {
    fn span(&self) -> Span<'a> {
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
            Self::LabeledExpr(labeled_expr) => labeled_expr.span,
        }
    }
}

#[derive(Debug)]
pub struct Assign<'a> {
    pub assigned: Vec<Expr<'a>>,
    pub value: Expr<'a>,
    pub span: Span<'a>,
}

#[derive(Debug)]
pub enum UnpackedVars<'a> {
    Var {
        span: Span<'a>,
        mutable: bool,
        name: String,
    },
    Ref {
        span: Span<'a>,
        mutable: bool,
        var: Box<Self>,
    },
    Tuple {
        span: Span<'a>,
        vars: Vec<Self>,
    },
    List {
        span: Span<'a>,
        vars: Vec<Self>,
    },
}

impl<'a> UnpackedVars<'a> {
    fn span(&self) -> Span<'a> {
        match *self {
            Self::Var { span, .. } => span,
            Self::Ref { span, .. } => span,
            Self::Tuple { span, .. } => span,
            Self::List { span, .. } => span,
        }
    }
}

#[derive(Debug)]
pub struct TupleTy<'a> {
    pub elements: Vec<Ty<'a>>,
    pub span: Span<'a>,
}

pub enum Ty<'a> {
    Auto(Span<'a>),
    Tuple(TupleTy<'a>),
}

impl<'a> Ty<'a> {
    pub fn span(&self) -> Span<'a> {
        match self {
            Self::Auto(span) => *span,
            Self::Tuple(x) => x.span,
        }
    }
}

impl<'a> fmt::Debug for Ty<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Type::")?;
        match self {
            Self::Auto(span) => f.debug_tuple("Auto").field(span).finish(),
            Self::Tuple(x) => fmt::Debug::fmt(x, f),
        }
    }
}

#[derive(Debug)]
pub struct Let<'a> {
    pub vars: UnpackedVars<'a>,
    pub ty: Ty<'a>,
    pub expr: Expr<'a>,
    pub span: Span<'a>,
}

pub enum Stmt<'a> {
    Assign(Assign<'a>),
    Let(Let<'a>),
}

impl<'a> fmt::Debug for Stmt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Stmt::")?;
        match self {
            Self::Assign(x) => fmt::Debug::fmt(x, f),
            Self::Let(x) => fmt::Debug::fmt(x, f),
        }
    }
}

pub enum Module<'a> {
    _PlaceHolder(Span<'a>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SepSignal<T = ()> {
    FinalCorrect(T),
    Correct,
    // FinalIncorrect(T),
    Incorrect,
}

// struct ChainedFunc<'a, 'b, Out, Data, E>(
//     fn(
//         &mut Data,
//         Tokens<'a, 'b>,
//         &mut Tokens<'a, 'b>,
//     ) -> Result<ChainSepSignal<'a, 'b, Out, Data, E>, E>,
// );
//
// #[derive(Clone, Copy)]
// pub enum ChainSepSignal<'a, 'b, Out, Data, E> {
//     FinalCorrect(Out),
//     Correct,
//     ChainFinalCorrect(ChainedFunc<'a, 'b, Out, Data, E>),
//     ChainCorrect(ChainedFunc<'a, 'b, (), Data, E>),
//     Incorrect,
// }
//
// impl<'a, 'b, Out, Data, E> Clone for ChainedFunc<'a, 'b, Out, Data, E> {
//     fn clone(&self) -> Self {
//         Self(self.0)
//     }
// }
// impl<'a, 'b, Out, Data, E> Copy for ChainedFunc<'a, 'b, Out, Data, E> {}
// impl<'a, 'b, Out: Clone, Data, E> ChainSepSignal<'a, 'b, Out, Data, E> {
//     fn clone(&self) -> Self {
//         match *self {
//             Self::FinalCorrect(ref out) => Self::FinalCorrect(out.clone()),
//             Self::Correct => Self::Correct,
//             Self::ChainFinalCorrect(f) => Self::ChainFinalCorrect(f),
//             Self::ChainCorrect(f) => Self::ChainCorrect(f),
//             Self::Incorrect => Self::Incorrect,
//         }
//     }
// }
// impl<'a, 'b, Out: Copy, Data, E> ChainSepSignal<'a, 'b, Out, Data, E> {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SepOut<T = ()> {
    NoTokens,
    NoMatch,
    FinalCorrect(T),
    // FinalIncorrect(T),
}

impl<T> SepOut<T> {
    /// FinalCorrect(v) => Ok(v), _ => Err(err())
    #[inline]
    pub fn ok_or_else<E, F: FnOnce() -> E>(self, err: F) -> Result<T, E> {
        match self {
            Self::FinalCorrect(x) => Ok(x),
            _ => Err(err()),
        }
    }

    /// FinalCorrect(v) => Ok(v), _ => Err(err)
    #[inline]
    pub fn ok_or<E>(self, err: E) -> Result<T, E> {
        match self {
            Self::FinalCorrect(x) => Ok(x),
            _ => Err(err),
        }
    }

    /// FinalCorrect(v) => Some(v), _ => None
    #[inline]
    pub fn some(self) -> Option<T> {
        match self {
            Self::FinalCorrect(x) => Some(x),
            _ => None,
        }
    }

    /// FinalCorrect(v) => v, _ => x
    #[inline]
    pub fn unwrap_or(self, x: T) -> T {
        match self {
            Self::FinalCorrect(x) => x,
            _ => x,
        }
    }

    /// FinalCorrect(v) => v, _ => f()
    #[inline]
    pub fn unwrap_or_else<F: FnOnce() -> T>(self, f: F) -> T {
        match self {
            Self::FinalCorrect(x) => x,
            _ => f(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Tokens<'a, 'b> {
    span: Span<'a>,
    tokens: &'b [TokenTree<'a>],
}

impl<'a, 'b> Tokens<'a, 'b> {
    pub fn new(group: &'b lexer::Group<'a>) -> Self {
        Self {
            span: match group.delim {
                lexer::Delimiter::None => group.span,
                _ => Span::new(group.span.path, group.span.start + 1, group.span.end - 1),
            },
            tokens: &group.tokens,
        }
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.tokens.len()
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    #[inline]
    pub const fn first(&self) -> Option<&'b TokenTree<'a>> {
        self.tokens.first()
    }

    #[inline]
    pub fn pop_first(&mut self) -> Option<&'b TokenTree<'a>> {
        let first = self.first()?;
        *self = self.get(1..);
        Some(first)
    }

    #[inline]
    pub fn split_first(&self) -> Option<(&'b TokenTree<'a>, Self)> {
        Some((self.first()?, self.get(1..)))
    }

    #[inline]
    pub const fn last(&self) -> Option<&'b TokenTree<'a>> {
        self.tokens.last()
    }

    #[inline]
    pub fn pop_last(&mut self) -> Option<&'b TokenTree<'a>> {
        let last = self.last()?;
        *self = self.get(..self.len() - 1);
        Some(last)
    }

    #[inline]
    pub fn split_last(&self) -> Option<(&'b TokenTree<'a>, Self)> {
        Some((self.last()?, self.get(..self.len() - 1)))
    }

    #[inline]
    pub const fn span(&self) -> Span<'a> {
        self.span
    }

    #[inline]
    pub const fn free_slice(&self) -> &'b [TokenTree<'a>] {
        self.tokens
    }

    #[inline]
    pub const fn slice(&self) -> &[TokenTree<'a>] {
        self.tokens
    }

    pub fn try_get<I>(&self, idx: I) -> Option<Self>
    where
        I: ops::RangeBounds<usize> + slice::SliceIndex<[TokenTree<'a>], Output = [TokenTree<'a>]>,
    {
        // macro_rules! ctry {
        //     ($x:expr) => {
        //         (match $x {
        //             Some(x) => x,
        //             None => return None,
        //         })
        //     };
        // }
        let start = {
            let i = match idx.start_bound() {
                ops::Bound::Unbounded => 0,
                ops::Bound::Included(&x) => x,
                ops::Bound::Excluded(&x) => x + 1,
            };
            match i {
                0 => self.span().start,
                i => self.tokens.get(i)?.span().start,
            }
        };
        let end = {
            let i = match idx.end_bound() {
                ops::Bound::Unbounded => self.len(),
                ops::Bound::Included(&x) => x + 1,
                ops::Bound::Excluded(&x) => x,
            };
            if i == self.len() {
                self.span().end
            } else {
                match i.checked_sub(1) {
                    Some(i) => self.tokens.get(i)?.span().end,
                    None => 0,
                }
            }
        }
        .max(start);
        Some(Self {
            tokens: self.tokens.get(idx)?,
            span: Span::new(self.span().path, start, end),
        })
    }

    pub fn get<I>(&self, idx: I) -> Self
    where
        I: ops::RangeBounds<usize>
            + slice::SliceIndex<[TokenTree<'a>], Output = [TokenTree<'a>]>
            + std::fmt::Debug,
    {
        let start = {
            let i = match idx.start_bound() {
                ops::Bound::Unbounded => 0,
                ops::Bound::Included(&x) => x,
                ops::Bound::Excluded(&x) => x + 1,
            };
            match i {
                0 => self.span().start,
                i if i == self.len() => self.span().end,
                i => self.tokens[i].span().start,
            }
        };
        let end = {
            let i = match idx.end_bound() {
                ops::Bound::Unbounded => self.len(),
                ops::Bound::Included(&x) => x + 1,
                ops::Bound::Excluded(&x) => x,
            };
            if i == self.len() {
                self.span().end
            } else {
                match i.checked_sub(1) {
                    Some(i) => self.tokens[i].span().end,
                    None => 0,
                }
            }
        }
        .max(start);
        Self {
            tokens: &self.tokens[idx],
            span: Span::new(self.span().path, start, end),
        }
    }

    // pub fn chain_sep<T, D, E>(
    //     &mut self,
    //     min_len: usize,
    //     max_len: usize,
    //     skip_newline: bool,
    //     mut data: D,
    //     mut try_convert: ChainedFunc<'a, 'b, T, D, E>,
    // ) -> Result<SepOut<T>, E> {
    //     let mut fstack: Vec<ChainedFunc<(), D, E>> = vec![];
    //     'sep_loop: loop {
    //         self.skip_whitespace(skip_newline);
    //         if self.is_empty() {
    //             break;
    //         }
    //         for len in (min_len..=max_len.min(self.len())).rev() {
    //             let sub_tokens = self.get(..len);
    //             let mut next_tokens = self.get(len..);
    //             let mut is_correct = true;
    //             if let Some(f) = fstack.last_mut() {
    //                 let signal = f.0(&mut data, sub_tokens, &mut next_tokens)?;
    //                 match signal {
    //                     ChainSepSignal::Correct => {}
    //                     ChainSepSignal::FinalCorrect(()) => {
    //                         fstack.pop();
    //                     }
    //                     ChainSepSignal::ChainCorrect(g) => {
    //                         fstack.push(g);
    //                     }
    //                     ChainSepSignal::ChainFinalCorrect(g) => {
    //                         *f = g;
    //                     }
    //                     ChainSepSignal::Incorrect => {
    //                         is_correct = false;
    //                     }
    //                 }
    //             } else {
    //                 let signal = try_convert.0(&mut data, sub_tokens, &mut next_tokens)?;
    //                 match try_convert.0(&mut data, sub_tokens, &mut next_tokens)? {
    //                     ChainSepSignal::Correct => {}
    //                     ChainSepSignal::FinalCorrect(out) => {
    //                         *self = next_tokens;
    //                         return Ok(SepOut::FinalCorrect(out));
    //                     }
    //                     ChainSepSignal::ChainCorrect(f) => {
    //                         fstack.push(f);
    //                     }
    //                     ChainSepSignal::ChainFinalCorrect(f) => {
    //                         try_convert = f;
    //                     }
    //                     ChainSepSignal::Incorrect => {
    //                         is_correct = false;
    //                     }
    //                 }
    //             }
    //             if is_correct {
    //                 *self = next_tokens;
    //                 continue 'sep_loop;
    //             }
    //         }
    //         return Ok(SepOut::NoMatch);
    //     }
    //     Ok(SepOut::NoTokens)
    // }

    pub fn full_sep<T, E, F: FnMut(Self, &mut Self) -> Result<SepSignal<T>, E>>(
        &mut self,
        min_len: usize,
        max_len: usize,
        skip_newline: bool,
        mut try_convert: F,
    ) -> Result<SepOut<T>, E> {
        'sep_loop: loop {
            self.skip_whitespace(skip_newline);
            if self.is_empty() {
                break;
            }
            for len in (min_len..=max_len.min(self.len())).rev() {
                let sub_tokens = self.get(..len);
                let mut next_tokens = self.get(len..);
                match try_convert(sub_tokens, &mut next_tokens)? {
                    SepSignal::FinalCorrect(out) => {
                        *self = next_tokens;
                        return Ok(SepOut::FinalCorrect(out));
                    }
                    // SepSignal::LastIncorrect => {
                    //     return Ok(true);
                    //     break 'sep_loop;
                    // }
                    SepSignal::Correct => {
                        *self = next_tokens;
                        continue 'sep_loop;
                    }
                    SepSignal::Incorrect => {}
                }
            }
            return Ok(SepOut::NoMatch);
        }
        Ok(SepOut::NoTokens)
    }

    #[inline]
    pub fn result_sep<T, E, F: FnMut(Self) -> Result<SepSignal<T>, E>>(
        &mut self,
        max_len: usize,
        skip_newline: bool,
        mut try_convert: F,
    ) -> Result<SepOut<T>, E> {
        self.full_sep(1, max_len, skip_newline, |tokens, _| try_convert(tokens))
    }

    #[inline]
    pub fn sep<T, F: FnMut(Self) -> SepSignal<T>>(
        &mut self,
        max_len: usize,
        skip_newline: bool,
        mut try_convert: F,
    ) -> SepOut<T> {
        self.result_sep::<T, std::convert::Infallible, _>(max_len, skip_newline, |tokens| {
            Ok(try_convert(tokens))
        })
        .unwrap()
    }

    pub fn skip_whitespace(&mut self, skip_newline: bool) {
        if let Some(lexer::pat::punct!(END)) = self.first() {
            self.pop_first();
        }
        if skip_newline && matches!(self.first(), Some(lexer::pat::newline!())) {
            self.pop_first();
        }
    }
}

// Reserved puncts: $ : , . ; # ~ =
// Op puncts: - + * / % ** & | && || ^ == != < > <= >=
// Uni op punts: -- !
// left: ~ ` ? \
//
// a = Span(path, start + i + 1, start + char_indices.next())
// $ cargo run $()

// fn result_sep<'a, Err, TryConvert: FnMut(&[TokenTree<'a>]) -> Result<SepSignal, Err>>(
//     tokens: &mut &[TokenTree<'a>],
//     max_len: usize,
//     mut f: TryConvert,
// ) -> Result<SepOut, Err> {
//     while !tokens.is_empty() {
//         let mut sub_tokens = &tokens[..max_len.min(tokens.len())];
//         loop {
//             if sub_tokens.is_empty() {
//                 return Ok(SepOut::NoMatch);
//             } else {
//                 match f(sub_tokens)? {
//                     SepSignal::FinalCorrect => {
//                         *tokens = &tokens[sub_tokens.len()..];
//                         return Ok(SepOut::FinalCorrect);
//                     }
//                     // SepSignal::LastIncorrect => {
//                     //     return Ok(true);
//                     //     break 'outer_loop;
//                     // }
//                     SepSignal::Correct => break,
//                     SepSignal::Incorrect => sub_tokens = &sub_tokens[..sub_tokens.len() - 1],
//                 }
//             }
//         }
//         *tokens = &tokens[sub_tokens.len()..];
//     }
//     Ok(SepOut::NoTokens)
// }
//
// #[inline]
// fn sep<'a, TryConvert: FnMut(&[TokenTree<'a>]) -> SepSignal>(
//     tokens: &mut &[TokenTree<'a>],
//     max_len: usize,
//     mut f: TryConvert,
// ) -> SepOut {
//     result_sep::<std::convert::Infallible, _>(tokens, max_len, |tokens| Ok(f(tokens))).unwrap()
// }

#[derive(Debug)]
pub enum ParseError<'a> {
    UnknownPrefixOp { op: String, span: Span<'a> },
    UnexpectedPunct { punct: String, span: Span<'a> },
    Expected { name: &'static str, span: Span<'a> },
    NegativeTupleAccess { span: Span<'a> },
    ChainedParenthesisOnly { span1: Span<'a>, span2: Span<'a> },
    InvalidLeftHandOfAssignment { span: Span<'a> },
    UnexpectedKeyword { kw: &'static str, span: Span<'a> },
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
            Self::UnexpectedKeyword { span, .. } => vec![span],
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
            Self::UnexpectedKeyword { kw, span } => {
                write!(f, "Unexpected keyword `{kw}` at '{span}'")
            }
        }
    }
}

impl<'a> Error for ParseError<'a> {}

fn parse_do_block<'a>(
    tokens: &mut Tokens<'a, '_>,
    ignore_newlines: bool,
) -> Result<Expr<'a>, ParseError<'a>> {
    let mut inner_tokens = *tokens;
    let out = inner_tokens
        .full_sep(1, 3, ignore_newlines, |tokens, next_tokens| {
            match tokens.slice() {
                // lexer::pat::puncts![{ ch: b':', span }]
                // | lexer::pat::tokens![punct: { ch: b':', span }, punct: END, newline] => {
                //     let assign_stmt = parse_assign_stmt(next_tokens, false)?;
                //     Ok(SepSignal::FinalCorrect(Expr::Block(Block {
                //         span: span.union(assign_stmt.span),
                //         stmts: vec![Stmt::Assign(assign_stmt)],
                //     })))
                // }
                lexer::pat::puncts![b':']
                | lexer::pat::tokens![punct: b':', punct: END, newline] => Ok(
                    SepSignal::FinalCorrect(parse_expr(next_tokens, ignore_newlines)?),
                ),
                lexer::pat::groups![(ref group @ {
                    delim: lexer::Delimiter::Braces, span, ..
                })] => Ok(SepSignal::FinalCorrect(Expr::Block(Block {
                    stmts: parse_block(Tokens::new(group))?,
                    span,
                }))),
                _ => Ok(SepSignal::Incorrect),
            }
        })?
        .ok_or_else(|| ParseError::Expected {
            name: "a do/then block",
            span: tokens.span().begining(),
        })?;

    *tokens = inner_tokens;
    Ok(out)
}

const EXPECTED_BLOCKLIKE_EXPR: &'static str =
    "a blocklike expression (if, while, for, or just a block { ... })";
fn parse_blocklike_expr<'a>(
    tokens: &mut Tokens<'a, '_>,
    ignore_newlines: bool,
    allow_label: bool,
) -> Result<Expr<'a>, ParseError<'a>> {
    let mut inner_tokens = *tokens;
    let block_info = if allow_label {
        inner_tokens
            .full_sep(2, 2, ignore_newlines, |tokens, _next_tokens| {
                match tokens.slice() {
                    lexer::pat::tokens![ident: { ref name, span }, punct: b':'] => {
                        if let Some(kw) = KEYWORDS.iter().find(|s| s == &name) {
                            Err(ParseError::UnexpectedKeyword { kw, span })
                        } else {
                            Ok(SepSignal::FinalCorrect((name.clone(), span)))
                        }
                    }
                    _ => Ok(SepSignal::Incorrect),
                }
            })?
            .some()
    } else {
        None
    };
    let expr = inner_tokens
        .full_sep(1, 1, ignore_newlines, |tokens, next_tokens| {
            match tokens.slice() {
                lexer::pat::idents![{ ref name, span }] if name == "if" => {
                    let mut if_expr = If {
                        cond: Box::new(parse_expr(next_tokens, ignore_newlines)?),
                        then_block: Box::new(parse_do_block(next_tokens, ignore_newlines)?),
                        else_block: next_tokens
                            .full_sep(1, 2, ignore_newlines, |tokens, next_tokens| {
                                match tokens.slice() {
                                    lexer::pat::idents![{ ref name, .. }]
                                    | lexer::pat::tokens![newline, ident: { ref name, .. }]
                                        if name == "else" =>
                                    {
                                        let else_block = parse_blocklike_expr(
                                            next_tokens,
                                            ignore_newlines,
                                            false,
                                        )
                                        .or_else(|_| {
                                            parse_do_block(next_tokens, ignore_newlines)
                                        })?;
                                        Ok(SepSignal::FinalCorrect(Box::new(else_block)))
                                    }
                                    _ => Ok(SepSignal::Incorrect),
                                }
                            })?
                            .some(),
                        span,
                    };
                    if_expr.span.unite_with(match if_expr.else_block {
                        Some(ref expr) => expr.span(),
                        None => if_expr.then_block.span(),
                    });
                    Ok(SepSignal::FinalCorrect(Expr::If(if_expr)))
                }
                lexer::pat::groups![(ref group @ {
                    delim: lexer::Delimiter::Braces,
                    span,
                    ..
                })] => Ok(SepSignal::FinalCorrect(Expr::Block(Block {
                    stmts: parse_block(Tokens::new(group))?,
                    span,
                }))),
                _ => Ok(SepSignal::Incorrect),
            }
        })?
        .ok_or_else(|| ParseError::Expected {
            name: EXPECTED_BLOCKLIKE_EXPR,
            span: inner_tokens.span(),
        })?;

    *tokens = inner_tokens;
    Ok(match block_info {
        Some((label, span)) => Expr::LabeledExpr(LabeledExpr {
            span: expr.span().union(span),
            expr: Box::new(expr),
            label,
        }),
        None => expr,
    })
}

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
fn parse_val<'a>(
    tokens: &mut Tokens<'a, '_>,
    ignore_newlines: bool,
) -> Result<Expr<'a>, ParseError<'a>> {
    let mut inner_tokens = *tokens;
    // let tokens_span = tokens[0].span().union(tokens.last().unwrap().span());
    let mut prefix_ops = vec![];

    // Make as many tokens as possible into `prefix_ops`
    inner_tokens.sep::<(), _>(UniOpType::max_num_tokens(), ignore_newlines, |tokens| {
        match UniOpType::try_prefix_tokens(tokens.slice()) {
            Some(ty) => {
                prefix_ops.push((ty, tokens.span()));
                SepSignal::Correct
            }
            None => SepSignal::Incorrect,
        }
    });
    // Get the value, pretty obvious
    let mut value = Some(
        parse_blocklike_expr(&mut inner_tokens, ignore_newlines, true).or_else(
            |blocklike_err| {
                let value_token = inner_tokens
                    .pop_first()
                    .ok_or_else(|| ParseError::Expected {
                        name: "a value",
                        span: tokens.span().ending(),
                    })?;
                Ok(match *value_token {
                    lexer::pat::ident!({ ref name, span }) if name == "true" => {
                        Expr::Bool(Bool { value: true, span })
                    }
                    lexer::pat::ident!({ ref name, span }) if name == "false" => {
                        Expr::Bool(Bool { value: false, span })
                    }
                    lexer::pat::ident!({ ref name, span }) if name == "return" => {
                        let expr = parse_tupling_expr(&mut inner_tokens, ignore_newlines)?;
                        Expr::Return(Return {
                            span: span.union(expr.span()),
                            expr: Box::new(expr),
                        })
                    }
                    lexer::pat::ident!({ ref name, span }) => {
                        if let Some(&kw) = KEYWORDS.iter().find(|&kw| kw == name) {
                            return Err(ParseError::UnexpectedKeyword { kw, span });
                        } else {
                            Expr::Var(Var {
                                name: name.clone(),
                                span,
                            })
                        }
                    }
                    lexer::pat::int!({ value, span }) => Expr::Int(Int { value, span }),
                    lexer::pat::float!({ value, span }) => Expr::Float(Float { value, span }),
                    lexer::pat::str!({ ref value, span }) => Expr::Str(Str {
                        value: value.clone(),
                        span,
                    }),
                    lexer::pat::group!((ref group @ {
                        delim: lexer::Delimiter::Parentheses,
                        span,
                        ..
                    })) => {
                        let (mut exprs, has_comma) =
                            parse_list(Tokens::new(group), |tokens| parse_expr(tokens, true))?;
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
                    lexer::pat::group!((ref group @ {
                        delim: lexer::Delimiter::Brackets,
                        span,
                        ..
                    })) => Expr::List(List {
                        values: parse_list(Tokens::new(group), |tokens| parse_expr(tokens, true))?
                            .0,
                        span,
                    }),
                    // lexer::pat::group!((ref group @ {
                    //     delim: lexer::Delimiter::Braces,
                    //     span,
                    //     ..
                    // })) => Expr::Block(Block {
                    //     stmts: parse_block(Tokens::new(group))?,
                    //     span,
                    // }),
                    _ if !matches!(
                        blocklike_err,
                        ParseError::Expected {
                            name: EXPECTED_BLOCKLIKE_EXPR,
                            ..
                        },
                    ) =>
                    {
                        return Err(blocklike_err);
                    }
                    ref token => {
                        return Err(ParseError::Expected {
                            name: "a value",
                            span: token.span().begining(),
                        });
                    }
                })
            },
        )?,
    );

    // Make as many tokens as possible into suffix ops
    inner_tokens.result_sep::<(), _, _>(
        UniOpType::max_num_tokens().max(3),
        ignore_newlines,
        |tokens| {
            Ok(match tokens.slice() {
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
                    // if element < 0 {
                    //     return Err(ParseError::NegativeTupleAccess { span: element_span });
                    // } else {
                    let expr = value.take().unwrap();
                    let total_span = expr.span().union(element_span);
                    value = Some(Expr::TupleAccess(TupleAccess {
                        expr: Box::new(expr),
                        element: element as _,
                        element_span: punct_span.union(element_span),
                        total_span,
                    }));
                    SepSignal::Correct
                    // }
                }
                lexer::pat::groups![(ref group @ {
                    delim: lexer::Delimiter::Parentheses,
                    span,
                    ..
                })] => {
                    let expr = value.take().unwrap();
                    let total_span = expr.span().union(span);
                    value = Some(Expr::FnCall(FnCall {
                        expr: Box::new(expr),
                        args: parse_list(Tokens::new(group), |tokens| parse_expr(tokens, true))?.0,
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
    tokens: &mut Tokens<'a, '_>,
    file: &str,
    file_lines: &[usize],
) -> Expr<'a> {
    let expr = match parse_tupling_expr(tokens, false) {
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

fn parse_expr<'a>(
    tokens: &mut Tokens<'a, '_>,
    ignore_newlines: bool,
) -> Result<Expr<'a>, ParseError<'a>> {
    let mut inner_tokens = *tokens;

    let value = parse_val(&mut inner_tokens, ignore_newlines)?;
    let mut values = vec![value];
    let mut ops: Vec<(DuoOpType, Span<'a>)> = vec![];
    while !inner_tokens.is_empty() {
        let op_x = inner_tokens.result_sep(DuoOpType::max_len(), ignore_newlines, |tokens| {
            if let Some(op_type) = DuoOpType::try_from_tokens(tokens.slice()) {
                Ok(SepSignal::FinalCorrect((
                    op_type,
                    tokens
                        .first()
                        .unwrap()
                        .span()
                        .union(tokens.last().unwrap().span()),
                )))
            } else {
                Ok(SepSignal::Incorrect)
            }
        })?;
        let (op, op_span) = match op_x {
            SepOut::FinalCorrect(x) => x,
            _ => break,
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
        values.push(parse_val(&mut inner_tokens, ignore_newlines)?);
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

fn parse_tupling_expr<'a>(
    tokens: &mut Tokens<'a, '_>,
    ignore_newlines: bool,
) -> Result<Expr<'a>, ParseError<'a>> {
    let mut inner_tokens = *tokens;

    let mut exprs = vec![parse_expr(&mut inner_tokens, ignore_newlines)?];
    let mut has_comma = false;
    inner_tokens.full_sep(1, 1, ignore_newlines, |tokens, next_tokens| {
        match tokens.slice() {
            lexer::pat::puncts![b','] => {
                has_comma = true;
                match parse_expr(next_tokens, ignore_newlines) {
                    Ok(expr) => {
                        exprs.push(expr);
                        Ok(SepSignal::Correct)
                    }
                    Err(_) => Ok(SepSignal::FinalCorrect(())),
                }
            }
            _ => Ok(SepSignal::Incorrect),
        }
    })?;

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
/// # Note
/// This function ignores newlines
///
/// # Returns
/// Returns a vector of the expressions and a bool specifying whether the list contains a comma.
fn parse_list<'a, T, F: FnMut(&mut Tokens<'a, '_>) -> Result<T, ParseError<'a>>>(
    mut tokens: Tokens<'a, '_>,
    mut f: F,
) -> Result<(Vec<T>, bool), ParseError<'a>> {
    let mut values = vec![];

    let mut needs_f = true;
    let mut has_comma = false;
    tokens.full_sep::<(), _, _>(0, 1, true, |tokens, next_tokens| match tokens.slice() {
        lexer::pat::puncts![b','] if !needs_f => {
            has_comma = true;
            needs_f = true;
            Ok(SepSignal::Correct)
        }
        &[] if needs_f => {
            values.push(f(next_tokens)?);
            needs_f = false;
            Ok(SepSignal::Correct)
        }
        _ => Ok(SepSignal::Incorrect),
    })?;
    has_comma |= values.is_empty();

    Ok((values, has_comma))
    // if tokens.is_empty() {
    //     Ok((vec![], false))
    // } else {
    //     let mut has_comma = false;
    //     let mut values = vec![f(&mut tokens)?];

    //     tokens.result_sep::<(), _, _>(1, true, |tokens, next_tokens| match tokens.slice() {
    //         lexer::pat::puncts![b','] => {
    //             has_comma = true;
    //             if !next_tokens.is_empty() {
    //                 let val = f(&mut tokens)?;
    //                 values.push(val);
    //             }
    //             Ok(SepSignal::Correct)
    //         }
    //         &[ref token] => Err(ParseError::Expected {
    //             name: "`,`",
    //             span: token.span().begining(),
    //         }),
    //         _ => unreachable!(),
    //     })?;

    //     Ok((values, has_comma))
    // }
}

pub fn parse_ty<'a>(
    tokens: &mut Tokens<'a, '_>,
    ignore_newlines: bool,
) -> Result<Ty<'a>, ParseError<'a>> {
    // TODO: implement full type parsing
    let mut inner_tokens = *tokens;
    let out = inner_tokens
        .result_sep(1, ignore_newlines, |tokens| match tokens.slice() {
            lexer::pat::idents![{ ref name, span }] if name == "_" => {
                Ok(SepSignal::FinalCorrect(Ty::Auto(span)))
            }
            lexer::pat::groups![(ref group @ { delim: lexer::Delimiter::Parentheses, span, .. })] =>
            {
                let tokens = Tokens::new(group);
                let (mut types, has_comma) = parse_list(tokens, |tokens| parse_ty(tokens, true))?;
                if has_comma || types.is_empty() {
                    Ok(SepSignal::FinalCorrect(Ty::Tuple(TupleTy {
                        elements: types,
                        span,
                    })))
                } else {
                    assert_eq!(types.len(), 1);
                    Ok(SepSignal::FinalCorrect(types.pop().unwrap()))
                }
            }
            _ => Ok(SepSignal::Incorrect),
        })?
        .ok_or_else(|| ParseError::Expected {
            name: "a type",
            span: inner_tokens.span().begining(),
        })?;
    *tokens = inner_tokens;
    Ok(out)
}

pub fn parse_let_stmt<'a>(
    let_kw: &lexer::Ident<'a>,
    tokens: &mut Tokens<'a, '_>,
) -> Result<Let<'a>, ParseError<'a>> {
    assert_eq!(let_kw.name, "let");

    let mut inner_tokens = *tokens;

    let mut has_comma = false;
    let mut vars = vec![parse_unpacked_vars(&mut inner_tokens, false)?];
    let mut types = vec![
        match inner_tokens.full_sep(1, 1, false, |tokens, next_tokens| match tokens.slice() {
            lexer::pat::puncts![b':'] => {
                let ty = parse_ty(next_tokens, false)?;
                Ok(SepSignal::FinalCorrect(ty))
            }
            _ => Ok(SepSignal::Incorrect),
        })? {
            SepOut::FinalCorrect(ty) => ty,
            _ => Ty::Auto(vars[0].span()),
        },
    ];
    inner_tokens
        .full_sep(1, 3, false, |tokens, next_tokens| match tokens.slice() {
            lexer::pat::puncts![b',', END, b'='] | lexer::pat::puncts![b',', b'='] => {
                has_comma = true;
                Ok(SepSignal::FinalCorrect(()))
            }
            lexer::pat::puncts![b'='] => Ok(SepSignal::FinalCorrect(())),
            lexer::pat::puncts![b','] => {
                has_comma = true;
                vars.push(parse_unpacked_vars(next_tokens, false)?);
                match next_tokens.full_sep(1, 1, false, |tokens, next_tokens| {
                    match tokens.slice() {
                        lexer::pat::puncts![b':'] => {
                            let ty = parse_ty(next_tokens, false)?;
                            Ok(SepSignal::FinalCorrect(ty))
                        }
                        _ => Ok(SepSignal::Incorrect),
                    }
                })? {
                    SepOut::FinalCorrect(ty) => types.push(ty),
                    _ => types.push(Ty::Auto(vars[vars.len() - 1].span())),
                }
                Ok(SepSignal::Correct)
            }
            _ => Ok(SepSignal::Incorrect),
        })?
        .ok_or_else(|| ParseError::Expected {
            name: "an equal sign '=' followed by an expression",
            span: inner_tokens.span().begining(),
        })?;
    let mut expr = Some(parse_tupling_expr(&mut inner_tokens, false)?);
    let out = inner_tokens
        .sep(1, false, |tokens| match tokens.slice() {
            lexer::pat::puncts![{ ch: b';', span }] | lexer::pat::newlines![{ span }] => {
                if has_comma {
                    let span = vars[0].span().union(types[types.len() - 1].span());
                    SepSignal::FinalCorrect(Let {
                        vars: UnpackedVars::Tuple {
                            span,
                            vars: mem::take(&mut vars),
                        },
                        ty: Ty::Tuple(TupleTy {
                            span,
                            elements: mem::take(&mut types),
                        }),
                        expr: expr.take().unwrap(),
                        span: span.union(let_kw.span),
                    })
                } else {
                    assert_eq!(vars.len(), 1);
                    assert_eq!(types.len(), 1);
                    SepSignal::FinalCorrect(Let {
                        vars: vars.pop().unwrap(),
                        ty: types.pop().unwrap(),
                        expr: expr.take().unwrap(),
                        span: span.union(let_kw.span),
                    })
                }
            }
            _ => SepSignal::Incorrect,
        })
        .ok_or_else(|| ParseError::Expected {
            name: "line ending ('\\n' or ';')",
            span: inner_tokens.span().begining(),
        })?;
    *tokens = inner_tokens;
    Ok(out)
}

pub fn parse_assign_stmt<'a>(
    tokens: &mut Tokens<'a, '_>,
    check_newline: bool,
) -> Result<Assign<'a>, ParseError<'a>> {
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
            Expr::UniOp(UniOp {
                op: UniOpType::Ref,
                ref expr,
                ..
            }) => check(expr),
            Expr::Tuple(Tuple { ref values, .. }) | Expr::List(List { ref values, .. }) => {
                values.iter().all(check)
            }
            _ => false,
        }
    }

    let mut inner_tokens = *tokens;
    let mut assigned = vec![parse_tupling_expr(&mut inner_tokens, false)?];
    let out = inner_tokens.full_sep(1, 1, false, |tokens, next_tokens| match tokens.slice() {
        lexer::pat::puncts![b'='] => {
            let last = assigned.last().unwrap();
            if check(last) {
                assigned.push(parse_tupling_expr(next_tokens, false)?);
                Ok(SepSignal::Correct)
            } else {
                Err(ParseError::InvalidLeftHandOfAssignment { span: last.span() })
            }
        }
        lexer::pat::puncts![{ ch: b';', span }] | lexer::pat::newlines![{ span }]
            if check_newline =>
        {
            Ok(SepSignal::FinalCorrect(span))
        }
        _ => Ok(SepSignal::Incorrect),
    })?;
    let span = assigned[0].span().union(if check_newline {
        out.ok_or_else(|| ParseError::Expected {
            name: "line ending ('\\n' or ';')",
            span: inner_tokens.span().begining(),
        })?
    } else {
        assigned.last().unwrap().span()
    });
    *tokens = inner_tokens;
    Ok(Assign {
        span,
        value: assigned.pop().unwrap(),
        assigned: mem::take(&mut assigned),
    })
}

fn parse_unpacked_vars<'a>(
    tokens: &mut Tokens<'a, '_>,
    ignore_newlines: bool,
) -> Result<UnpackedVars<'a>, ParseError<'a>> {
    // # TYPES
    // ({uvars}, {uvars}, ...)
    // [{uvars}, {uvars}, ...]
    // &{uvars}
    // mut {ident}
    // {ident}
    fn rec<'a>(
        tokens: &mut Tokens<'a, '_>,
        ignore_newlines: bool,
    ) -> Result<UnpackedVars<'a>, ParseError<'a>> {
        tokens
            .full_sep(0, 3, ignore_newlines, |tokens, next_tokens| {
                match tokens.slice() {
                    lexer::pat::tokens![
                        punct: { ch: b'&', span },
                        punct: END,
                        ident: { ref name, .. },
                    ] if name == "mut" => {
                        let var = Box::new(rec(next_tokens, ignore_newlines)?);
                        Ok(SepSignal::FinalCorrect(UnpackedVars::Ref {
                            span: span.union(var.span()),
                            mutable: true,
                            var,
                        }))
                    }
                    lexer::pat::puncts![{ ch: b'&', span }] => {
                        let var = Box::new(rec(next_tokens, ignore_newlines)?);
                        Ok(SepSignal::FinalCorrect(UnpackedVars::Ref {
                            span: span.union(var.span()),
                            mutable: false,
                            var,
                        }))
                    }
                    lexer::pat::idents![{ name: ref mut_name, span: mut_span }, { ref name, span }]
                        if mut_name == "mut" =>
                    {
                        if let Some(&kw) = KEYWORDS.iter().find(|&kw| kw == name) {
                            Err(ParseError::UnexpectedKeyword { kw, span })
                        } else {
                            Ok(SepSignal::FinalCorrect(UnpackedVars::Var {
                                mutable: true,
                                name: name.clone(),
                                span: mut_span.union(span),
                            }))
                        }
                    }
                    lexer::pat::idents![{ ref name, span }] => {
                        if let Some(&kw) = KEYWORDS.iter().find(|&kw| kw == name) {
                            Err(ParseError::UnexpectedKeyword { kw, span })
                        } else {
                            Ok(SepSignal::FinalCorrect(UnpackedVars::Var {
                                mutable: false,
                                name: name.clone(),
                                span,
                            }))
                        }
                    }
                    lexer::pat::groups![(ref group @ {
                        delim: lexer::Delimiter::Parentheses,
                        span,
                        ..
                    })] => {
                        let mut vars = vec![];
                        let mut need_vars = true;
                        let mut has_comma = false;
                        let mut tokens = Tokens::new(group);
                        match tokens.full_sep::<(), _, _>(0, 1, true, |tokens, next_tokens| {
                            match tokens.slice() {
                                lexer::pat::puncts![b','] if !need_vars => {
                                    has_comma = true;
                                    need_vars = true;
                                    Ok(SepSignal::Correct)
                                }
                                &[] if need_vars => {
                                    vars.push(rec(next_tokens, true)?);
                                    need_vars = false;
                                    Ok(SepSignal::Correct)
                                }
                                _ => Ok(SepSignal::Incorrect),
                            }
                        })? {
                            SepOut::NoTokens => {
                                Ok(SepSignal::FinalCorrect(if has_comma || vars.is_empty() {
                                    UnpackedVars::Tuple { vars, span }
                                } else {
                                    assert_eq!(vars.len(), 1);
                                    vars.pop().unwrap()
                                }))
                            }
                            _ => Err(ParseError::Expected {
                                name: "a comma (`,`)",
                                span: tokens.span().begining(),
                            }),
                        }
                        // let mut tokens = Tokens::new(group);
                        // let mut vars = vec![];
                        // let mut has_comma = false;
                        // while !tokens.is_empty() {
                        //     vars.push(rec(&mut tokens, true)?);
                        //     tokens.result_sep(1, true, |tokens| match tokens.slice() {
                        //         lexer::pat::puncts![b','] => {
                        //             has_comma = true;
                        //             Ok(SepSignal::FinalCorrect(()))
                        //         }
                        //         _ => Err(ParseError::Expected {
                        //             name: "a comma (`,`)",
                        //             span: tokens.span().begining(),
                        //         }),
                        //     })?;
                        // }
                        // Ok(SepSignal::FinalCorrect(if has_comma {
                        //     UnpackedVars::Tuple { vars, span }
                        // } else {
                        //     assert_eq!(vars.len(), 1);
                        //     vars.pop().unwrap()
                        // }))
                    }
                    lexer::pat::groups![(ref group @ {
                        delim: lexer::Delimiter::Brackets,
                        span,
                        ..
                    })] => {
                        let mut vars = vec![];
                        let mut need_vars = true;
                        let mut tokens = Tokens::new(group);
                        match tokens.full_sep::<(), _, _>(0, 1, true, |tokens, next_tokens| {
                            match tokens.slice() {
                                lexer::pat::puncts![b','] if !need_vars => {
                                    need_vars = true;
                                    Ok(SepSignal::Correct)
                                }
                                &[] if need_vars => {
                                    vars.push(rec(next_tokens, true)?);
                                    need_vars = false;
                                    Ok(SepSignal::Correct)
                                }
                                _ => Ok(SepSignal::Incorrect),
                            }
                        })? {
                            SepOut::NoTokens => {
                                Ok(SepSignal::FinalCorrect(UnpackedVars::List { vars, span }))
                            }
                            _ => Err(ParseError::Expected {
                                name: "a comma (`,`)",
                                span: tokens.span().begining(),
                            }),
                        }
                    }
                    _ => Ok(SepSignal::Incorrect),
                }
            })?
            .ok_or_else(|| ParseError::Expected {
                name: "unpacked Vars",
                span: tokens.span().begining(),
            })
    }

    let mut inner_tokens = *tokens;
    let value = rec(&mut inner_tokens, ignore_newlines)?;
    *tokens = inner_tokens;
    Ok(value)
}

pub fn parse_stmt<'a>(tokens: &mut Tokens<'a, '_>) -> Result<Stmt<'a>, ParseError<'a>> {
    let mut inner_tokens = *tokens;
    let out = match inner_tokens.first() {
        Some(lexer::pat::ident!((ref kw @ { ref name, .. }))) => match name.as_str() {
            "let" => {
                inner_tokens.pop_first();
                Stmt::Let(parse_let_stmt(kw, &mut inner_tokens)?)
            }
            _ => Stmt::Assign(parse_assign_stmt(&mut inner_tokens, true)?),
        },
        _ => Stmt::Assign(parse_assign_stmt(&mut inner_tokens, true)?),
    };
    *tokens = inner_tokens;
    Ok(out)
}

pub fn parse_block<'a>(mut tokens: Tokens<'a, '_>) -> Result<Vec<Stmt<'a>>, ParseError<'a>> {
    let mut stmts = vec![];
    tokens.skip_whitespace(true);
    while !tokens.is_empty() {
        stmts.push(parse_stmt(&mut tokens)?);
        tokens.skip_whitespace(true);
    }
    Ok(stmts)
}

pub fn parse_module<'a>(mut _tokens: &[TokenTree<'a>]) -> Module<'a> {
    todo!();
}

// TODO: Make devouring functions return their stop error
