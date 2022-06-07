use crate::lexer::{self, TokenTree};
use crate::Span;
use std::marker::PhantomData;
use std::mem;

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

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum UniOpType {
    Neg,
    Pos,
    Not,
    Ref,
    Deref,
    PropAccess(String),
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
    pub ty: DuoOpType,
    pub left_expr: Box<Expr<'a>>,
    pub right_expr: Box<Expr<'a>>,
    pub span: PhantomData<Span<'a>>,
}

#[derive(Debug)]
pub struct UniOp<'a> {
    pub ty: UniOpType,
    pub expr: Box<Expr<'a>>,
    pub span: PhantomData<Span<'a>>,
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
pub enum Expr<'a> {
    DuoOp(DuoOp<'a>),
    UniOp(UniOp<'a>),
    Var(Var<'a>),
    Int(Int<'a>),
    Float(Float<'a>),
    Str(Str<'a>),
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

fn parse_val<'a>(
    tokens: &mut &[TokenTree<'a>],
    mut prefix_ops: Vec<UniOpType>,
) -> (Expr<'a>, Option<DuoOpType>, Vec<UniOpType>) {
    loop {
        if let Some(token) = tokens.first() {
            match *token {
                TokenTree::Ident(lexer::Ident { ref name, .. }) => {
                    if let Some(ty) = UniOpType::try_prefix(name) {
                        prefix_ops.push(ty);
                    } else {
                        break;
                    }
                }
                TokenTree::Punct7(_) | TokenTree::PunctString(_) => {
                    let punct = match token {
                        TokenTree::Punct7(lexer::Punct7 { punct, .. }) => punct.as_bytes(),
                        TokenTree::PunctString(lexer::PunctString { punct, .. }) => {
                            punct.as_bytes()
                        }
                        _ => unreachable!(),
                    };
                    assert_eq!(
                        sep_puncts(punct, |s| {
                            if let Some(ty) = UniOpType::try_prefix_buf(s) {
                                prefix_ops.push(ty);
                                SepSignal::Correct
                            } else {
                                SepSignal::Incorrect
                            }
                        }),
                        b"",
                    );
                }
                _ => break,
            }
            *tokens = &tokens[1..];
        } else {
            panic!("Failed to find value")
        }
    }

    let mut value = Some(match *tokens.first().unwrap() {
        TokenTree::Ident(lexer::Ident { ref name, span }) => {
            ();
            Expr::Var(Var {
                name: name.clone(),
                span,
            })
        }
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
            ..
        }) => parse_expr(&mut &**tokens),
        _ => todo!(),
    });
    *tokens = &tokens[1..];

    let mut next_op = None;
    let mut next_prefix_ops = vec![];
    loop {
        if let Some(token) = tokens.first() {
            match *token {
                TokenTree::Punct7(_) | TokenTree::PunctString(_) => {
                    let punct = match token {
                        TokenTree::Punct7(lexer::Punct7 { punct, .. }) => punct.as_bytes(),
                        TokenTree::PunctString(lexer::PunctString { punct, .. }) => {
                            punct.as_bytes()
                        }
                        _ => unreachable!(),
                    };

                    match sep_puncts(&punct[..punct.len()], |s| {
                        if let Some(ty) = UniOpType::try_suffix_buf(s) {
                            value = Some(Expr::UniOp(UniOp {
                                ty,
                                expr: Box::new(value.take().unwrap()),
                                span: PhantomData,
                            }));
                            SepSignal::Correct
                        } else {
                            SepSignal::Incorrect
                        }
                    }) {
                        b"" => {}
                        b"." => {
                            *tokens = &tokens[1..];
                            match &tokens[0] {
                                TokenTree::Ident(lexer::Ident { name, .. }) => {
                                    value = Some(Expr::UniOp(UniOp {
                                        ty: UniOpType::PropAccess(name.clone()),
                                        expr: Box::new(value.take().unwrap()),
                                        span: PhantomData,
                                    }));
                                }
                                _ => todo!(),
                            }
                        }
                        mut puncts => {
                            puncts = sep_puncts(puncts, |s| {
                                if let Some(ty) = DuoOpType::try_from_buf(s) {
                                    next_op = Some(ty);
                                    SepSignal::LastCorrect
                                } else {
                                    SepSignal::Incorrect
                                }
                            });
                            assert_eq!(
                                sep_puncts(puncts, |s| {
                                    if let Some(ty) = UniOpType::try_prefix_buf(s) {
                                        next_prefix_ops.push(ty);
                                        SepSignal::LastCorrect
                                    } else {
                                        SepSignal::Incorrect
                                    }
                                }),
                                b"",
                            );
                            *tokens = &tokens[1..];
                            break;
                        }
                    }
                }
                // TODO: Add function call
                _ => break,
            }
            *tokens = &tokens[1..];
        } else {
            break;
        }
    }

    for ty in prefix_ops.into_iter().rev() {
        value = Some(Expr::UniOp(UniOp {
            ty,
            expr: Box::new(value.take().unwrap()),
            span: PhantomData,
        }))
    }

    (value.unwrap(), next_op, next_prefix_ops)
}

pub fn parse_expr<'a>(tokens: &mut &[TokenTree<'a>]) -> Expr<'a> {
    let (value, mut op, mut prefix_ops) = parse_val(tokens, vec![]);
    let mut values = vec![value];
    let mut ops: Vec<DuoOpType> = vec![];
    while !tokens.is_empty() {
        let current_op;
        if let Some(op) = op.take() {
            current_op = op;
        } else {
            match tokens[0] {
                TokenTree::Punct7(_) | TokenTree::PunctString(_) => {
                    let punct = match &tokens[0] {
                        TokenTree::Punct7(lexer::Punct7 { punct, .. }) => punct.as_bytes(),
                        TokenTree::PunctString(lexer::PunctString { punct, .. }) => {
                            punct.as_bytes()
                        }
                        _ => unreachable!(),
                    };
                    let punct = sep_puncts(punct, |s| {
                        op = DuoOpType::try_from_buf(s);
                        if op.is_some() {
                            SepSignal::LastCorrect
                        } else {
                            SepSignal::Incorrect
                        }
                    });
                    current_op = op.unwrap();
                    assert_eq!(
                        sep_puncts(punct, |s| {
                            if let Some(ty) = UniOpType::try_prefix_buf(s) {
                                prefix_ops.push(ty);
                                SepSignal::Correct
                            } else {
                                SepSignal::Incorrect
                            }
                        }),
                        b""
                    )
                }
                _ => break,
            }
            *tokens = &tokens[1..];
        };
        // println!("Add op: `{:?}`", current_op);
        while let Some(&last_op) = ops.last() {
            if last_op.level() <= current_op.level() {
                let right_expr = Box::new(values.pop().unwrap());
                let left_expr = Box::new(values.pop().unwrap());
                // println!("resolve op: {last_op:?}");
                values.push(Expr::DuoOp(DuoOp {
                    ty: ops.pop().unwrap(),
                    left_expr,
                    right_expr,
                    span: PhantomData,
                }));
                // println!("new value: {:#?}", values.last().unwrap());
            } else {
                break;
            }
        }
        ops.push(current_op);

        let value;
        (value, op, prefix_ops) = parse_val(tokens, mem::take(&mut prefix_ops));
        // println!("Add value: `{:?}`", value);
        values.push(value);
    }
    assert!(op.is_none());
    assert!(prefix_ops.is_empty());
    while let Some(op) = ops.pop() {
        let right_expr = Box::new(values.pop().unwrap());
        let left_expr = Box::new(values.pop().unwrap());
        // println!("resolve op: {op:?}");
        values.push(Expr::DuoOp(DuoOp {
            ty: op,
            left_expr,
            right_expr,
            span: PhantomData,
        }));
        // println!("new value: {:#?}", values.last().unwrap());
    }

    assert_eq!(values.len(), 1);
    values.pop().unwrap()
}

pub fn parse_module<'a>(tokens: &[TokenTree<'a>]) -> Module<'a> {
    for _token in tokens {}

    todo!();
}
