use crate::{lexer::Delimiter, parser_old::Tokens, parserlib::*, Span, Spanned};
use std::fmt;

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum DuoOpType {
    // Pipe   = 0x00,
    Pow = 0x10,
    Mul = 0x20,
    Div = 0x21,
    Mod = 0x22,
    Add = 0x30,
    Sub = 0x31,
    Shr = 0x40,
    Shl = 0x41,
    BitXor = 0x50,
    BitAnd = 0x60,
    BitOr = 0x70,
    Lt = 0x80,
    Gt = 0x81,
    Le = 0x82,
    Ge = 0x83,
    Eq = 0x84,
    Neq = 0x85,
    And = 0x90,
    Or = 0xA0,
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
                LeftToRight,   // 0:
                LeftToRight,   // 1: **
                LeftToRight,   // 2: * /
                LeftToRight,   // 3: + -
                LeftToRight,   // 4: >> <<
                LeftToRight,   // 5: ^
                LeftToRight,   // 6: &
                LeftToRight,   // 7: |
                RequireParens, // 8: < > <= >= == !=
                LeftToRight,   // 9: &&
                LeftToRight,   // a: ||
                LeftToRight,   // b:
                LeftToRight,   // c:
                LeftToRight,   // d:
                LeftToRight,   // e:
                LeftToRight,   // f:
            ]
            .get_unchecked(self as usize >> 4)
        }
    }
}

pub struct DuoOpParser {
    pub ignore_newline: bool,
}
pub struct DuoOpValue<'a> {
    pub ty: DuoOpType,
    pub span: Span<'a>,
}
impl<'a> Spanned<'a> for DuoOpValue<'a> {
    #[inline]
    fn span(&self) -> Span<'a> {
        self.span
    }
}
impl<'a> Parser<'a> for DuoOpParser {
    type Output = DuoOpValue<'a>;
    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let val = Chained2(
            MatchParser::SkipWhitespace {
                skip_newline: self.ignore_newline,
            },
            OneOf {
                name: "a duo operator",
                parsers: &[
                    (DuoOpType::Pow, MatchParser::Punct(b"**")),
                    (DuoOpType::Mul, MatchParser::Punct(b"*")),
                    (DuoOpType::Div, MatchParser::Punct(b"/")),
                    (DuoOpType::Mod, MatchParser::Punct(b"%")),
                    (DuoOpType::Add, MatchParser::Punct(b"+")),
                    (DuoOpType::Sub, MatchParser::Punct(b"-")),
                    (DuoOpType::Shr, MatchParser::Punct(b">>")),
                    (DuoOpType::Shl, MatchParser::Punct(b"<<")),
                    (DuoOpType::BitXor, MatchParser::Punct(b"^")),
                    (DuoOpType::BitAnd, MatchParser::Punct(b"&")),
                    (DuoOpType::BitOr, MatchParser::Punct(b"|")),
                    (DuoOpType::Lt, MatchParser::Punct(b"<")),
                    (DuoOpType::Gt, MatchParser::Punct(b">")),
                    (DuoOpType::Le, MatchParser::Punct(b"<=")),
                    (DuoOpType::Ge, MatchParser::Punct(b">=")),
                    (DuoOpType::Eq, MatchParser::Punct(b"==")),
                    (DuoOpType::Neq, MatchParser::Punct(b"!=")),
                    (DuoOpType::And, MatchParser::Punct(b"&&")),
                    (DuoOpType::Or, MatchParser::Punct(b"||")),
                ],
            },
        )
        .parse(tokens)?;
        Ok(DuoOpValue {
            ty: val.1.id,
            span: val.span(),
        })
    }
}

#[derive(Debug, Clone)]
pub enum UniOpType<'a> {
    Neg,
    Pos,
    Not,
    Ref,
    Deref,
    PropAccess(String),
    TupleAccess(u64),
    FuncCall(Vec<Expr<'a>>),
    MethodCall { name: String, args: Vec<Expr<'a>> },
}

pub struct PrefixParser {
    pub ignore_newline: bool,
}
pub struct SuffixParser {
    pub ignore_newline: bool,
}
pub struct UniOpValue<'a> {
    pub ty: UniOpType<'a>,
    pub span: Span<'a>,
}
impl<'a> Spanned<'a> for UniOpValue<'a> {
    #[inline]
    fn span(&self) -> Span<'a> {
        self.span
    }
}
impl<'a> Parser<'a> for PrefixParser {
    type Output = UniOpValue<'a>;
    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let skip_whitespace = MatchParser::SkipWhitespace {
            skip_newline: self.ignore_newline,
        };
        let value = Chained2(
            skip_whitespace,
            OneOf {
                name: "a prefix operator",
                parsers: &[
                    (UniOpType::Neg, MatchParser::Punct(b"-")),
                    (UniOpType::Pos, MatchParser::Punct(b"+")),
                    (UniOpType::Not, MatchParser::Punct(b"!")),
                    (UniOpType::Ref, MatchParser::Punct(b"&")),
                    (UniOpType::Deref, MatchParser::Punct(b"*")),
                ],
            },
        )
        .parse(tokens)?;
        Ok(UniOpValue {
            span: value.1.span(),
            ty: value.1.id,
        })
    }
}
impl<'a> Parser<'a> for SuffixParser {
    type Output = UniOpValue<'a>;
    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let skip_whitespace = MatchParser::SkipWhitespace {
            skip_newline: self.ignore_newline,
        };
        let value = Chained2(
            skip_whitespace,
            OneOf5 {
                name: "a prefix operator",
                parsers: (
                    OneOf {
                        name: "a suffix operator",
                        parsers: &[
                            (UniOpType::Neg, MatchParser::Punct(b".-")),
                            (UniOpType::Pos, MatchParser::Punct(b".+")),
                            (UniOpType::Not, MatchParser::Punct(b".!")),
                            (UniOpType::Ref, MatchParser::Punct(b".&")),
                            (UniOpType::Deref, MatchParser::Punct(b".*")),
                        ],
                    },
                    Chained5(
                        MatchParser::Punct(b"."),
                        skip_whitespace,
                        IdentParser,
                        skip_whitespace,
                        GroupParser {
                            delim: Delimiter::Parentheses,
                            body: ListParser {
                                ignore_newline: self.ignore_newline,
                                allow_empty: true,
                                parser: ExprParser {
                                    ignore_newline: self.ignore_newline,
                                },
                            },
                        },
                    ),
                    Chained3(MatchParser::Punct(b"."), skip_whitespace, IdentParser),
                    Chained3(MatchParser::Punct(b"."), skip_whitespace, IntParser),
                    GroupParser {
                        delim: Delimiter::Parentheses,
                        body: ListParser {
                            ignore_newline: self.ignore_newline,
                            allow_empty: true,
                            parser: ExprParser {
                                ignore_newline: self.ignore_newline,
                            },
                        },
                    },
                ),
            },
        )
        .parse(tokens)?;
        Ok(UniOpValue {
            span: value.span(),
            ty: match value.1 {
                OneOfValue5::T0(value) => value.id,
                OneOfValue5::T1(value) => UniOpType::MethodCall {
                    name: value.2.name,
                    args: value.4.body.elements,
                },
                OneOfValue5::T2(value) => UniOpType::PropAccess(value.2.name),
                OneOfValue5::T3(value) => UniOpType::TupleAccess(value.2.value),
                OneOfValue5::T4(value) => UniOpType::FuncCall(value.body.elements),
            },
        })
    }
}

pub struct BoolParser;
pub struct BoolValue<'a> {
    pub value: bool,
    pub span: Span<'a>,
}
impl<'a> Spanned<'a> for BoolValue<'a> {
    #[inline]
    fn span(&self) -> Span<'a> {
        self.span
    }
}
impl<'a> Parser<'a> for BoolParser {
    type Output = BoolValue<'a>;
    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let value = OneOf {
            name: "a boolean",
            parsers: &[
                (true, MatchParser::Keyword("true")),
                (false, MatchParser::Keyword("false")),
            ],
        }
        .parse(tokens)?;
        Ok(BoolValue {
            value: value.id,
            span: value.span(),
        })
    }
}

#[derive(Debug, Clone)]
pub struct DuoOp<'a> {
    pub left_expr: Box<Expr<'a>>,
    pub op: DuoOpType,
    pub right_expr: Box<Expr<'a>>,
    pub op_span: Span<'a>,
    pub total_span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct UniOp<'a> {
    pub op: UniOpType<'a>,
    pub expr: Box<Expr<'a>>,
    pub op_span: Span<'a>,
    pub total_span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct FnCall<'a> {
    pub expr: Box<Expr<'a>>,
    pub args: Vec<Expr<'a>>,
    pub args_span: Span<'a>,
    pub total_span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct Var<'a> {
    pub name: String,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct Int<'a> {
    pub value: u64,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct Float<'a> {
    pub value: f64,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct Str<'a> {
    pub value: String,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct Bool<'a> {
    pub value: bool,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct List<'a> {
    pub values: Vec<Expr<'a>>,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct Tuple<'a> {
    pub values: Vec<Expr<'a>>,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct If<'a> {
    pub cond: Box<Expr<'a>>,
    pub then_block: Box<Expr<'a>>,
    pub else_block: Option<Box<Expr<'a>>>,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct Return<'a> {
    pub span: Span<'a>,
    pub expr: Box<Expr<'a>>,
}

// #[derive(Debug)]
// pub struct Block<'a> {
//     pub stmts: Vec<Stmt<'a>>,
//     pub span: Span<'a>,
// }

#[derive(Debug, Clone)]
pub struct LabeledExpr<'a> {
    pub label: String,
    pub expr: Box<Expr<'a>>,
    pub span: Span<'a>,
}

#[derive(Clone)]
pub enum Expr<'a> {
    // Block(Block<'a>),
    DuoOp(DuoOp<'a>),
    UniOp(UniOp<'a>),

    Var(Var<'a>),
    Int(Int<'a>),
    Float(Float<'a>),
    Str(Str<'a>),
    Bool(Bool<'a>),
    List(List<'a>),
    Tuple(Tuple<'a>),

    FnCall(FnCall<'a>),

    LabeledExpr(LabeledExpr<'a>),
    If(If<'a>),
    Return(Return<'a>),
}

impl<'a> fmt::Debug for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Expr::")?;
        match self {
            // Self::Block(x) => fmt::Debug::fmt(x, f),
            Self::UniOp(x) => fmt::Debug::fmt(x, f),
            Self::DuoOp(x) => fmt::Debug::fmt(x, f),
            Self::Var(x) => fmt::Debug::fmt(x, f),
            Self::Int(x) => fmt::Debug::fmt(x, f),
            Self::Float(x) => fmt::Debug::fmt(x, f),
            Self::Str(x) => fmt::Debug::fmt(x, f),
            Self::Bool(x) => fmt::Debug::fmt(x, f),
            Self::List(x) => fmt::Debug::fmt(x, f),
            Self::Tuple(x) => fmt::Debug::fmt(x, f),
            // Self::PropAccess(x) => fmt::Debug::fmt(x, f),
            // Self::TupleAccess(x) => fmt::Debug::fmt(x, f),
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
            // Self::Block(block) => block.span,
            Self::DuoOp(duo_op) => duo_op.total_span,
            Self::UniOp(uni_op) => uni_op.total_span,
            Self::Var(var) => var.span,
            Self::Int(int) => int.span,
            Self::Float(float) => float.span,
            Self::Str(s) => s.span,
            Self::Bool(b) => b.span,
            Self::List(list) => list.span,
            Self::Tuple(tuple) => tuple.span,
            // Self::PropAccess(prop_access) => prop_access.total_span,
            // Self::TupleAccess(tuple_access) => tuple_access.total_span,
            Self::FnCall(fn_call) => fn_call.total_span,
            Self::If(if_expr) => if_expr.span,
            Self::Return(return_expr) => return_expr.span,
            Self::LabeledExpr(labeled_expr) => labeled_expr.span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum CoreValueType {
    Var(String),
    Str(String),
    Int(u64),
    Float(f64),
    Bool(bool),
}
#[derive(Debug, Clone)]
pub struct CoreValue<'a> {
    pub ty: CoreValueType,
    pub span: Span<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct CoreValueParser {
    pub ignore_newline: bool,
}

impl<'a> Spanned<'a> for CoreValue<'a> {
    fn span(&self) -> Span<'a> {
        self.span
    }
}

impl<'a> Parser<'a> for CoreValueParser {
    type Output = Expr<'a>;

    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let value = Chained2(
            MatchParser::SkipWhitespace {
                skip_newline: self.ignore_newline,
            },
            OneOf7 {
                name: "a core value",
                parsers: (
                    IdentParser,
                    StrParser,
                    IntParser,
                    FloatParser,
                    BoolParser,
                    GroupParser {
                        delim: Delimiter::Parentheses,
                        body: ListParser {
                            ignore_newline: true,
                            allow_empty: true,
                            parser: ExprParser {
                                ignore_newline: true,
                            },
                        },
                    },
                    GroupParser {
                        delim: Delimiter::Brackets,
                        body: ListParser {
                            ignore_newline: true,
                            allow_empty: true,
                            parser: ExprParser {
                                ignore_newline: true,
                            },
                        },
                    },
                ),
            },
        )
        .parse(tokens)?;

        Ok(match value.1 {
            OneOfValue7::T0(IdentValue { name, span }) => Expr::Var(Var { name, span }),
            OneOfValue7::T1(StrValue { value, span }) => Expr::Str(Str { value, span }),
            OneOfValue7::T2(IntValue { value, span }) => Expr::Int(Int { value, span }),
            OneOfValue7::T3(FloatValue { value, span }) => Expr::Float(Float { value, span }),
            OneOfValue7::T4(BoolValue { value, span }) => Expr::Bool(Bool { value, span }),
            OneOfValue7::T5(GroupValue { span, body }) => {
                if body.has_comma || body.elements.is_empty() {
                    Expr::Tuple(Tuple {
                        values: body.elements,
                        span,
                    })
                } else {
                    debug_assert_eq!(body.elements.len(), 1);
                    body.elements.into_iter().next().unwrap()
                }
            }
            OneOfValue7::T6(GroupValue { span, body }) => Expr::List(List {
                values: body.elements,
                span,
            }),
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ValueParser {
    pub ignore_newline: bool,
}
impl<'a> Parser<'a> for ValueParser {
    type Output = Expr<'a>;

    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let Self { ignore_newline } = *self;

        let value = Chained3(
            Repeat {
                body: PrefixParser { ignore_newline },
                sep: MatchParser::Empty,
                allow_empty: true,
                allow_trailing_sep: true,
            },
            CoreValueParser { ignore_newline },
            Repeat {
                body: SuffixParser { ignore_newline },
                sep: MatchParser::Empty,
                allow_empty: true,
                allow_trailing_sep: true,
            },
        )
        .parse(tokens)?;

        let mut expr = value.1;
        for suffix in value.2.body {
            expr = Expr::UniOp(UniOp {
                total_span: expr.span(),
                expr: Box::new(expr),
                op_span: suffix.span(),
                op: suffix.ty,
            });
        }
        for prefix in value.0.body.into_iter().rev() {
            expr = Expr::UniOp(UniOp {
                total_span: expr.span(),
                expr: Box::new(expr),
                op_span: prefix.span(),
                op: prefix.ty,
            });
        }
        Ok(expr)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ExprParser {
    pub ignore_newline: bool,
}
impl<'a> Parser<'a> for ExprParser {
    type Output = Expr<'a>;

    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let Self { ignore_newline } = *self;

        let value = Repeat {
            body: ValueParser { ignore_newline },
            sep: DuoOpParser { ignore_newline },
            allow_empty: false,
            allow_trailing_sep: false,
        }
        .parse(tokens)?;

        let mut value_iter = value.body.into_iter();

        let mut values = Vec::with_capacity(value.sep.len());
        let mut ops = Vec::<DuoOpValue>::with_capacity(value.sep.len());
        values.push(value_iter.next().unwrap());

        for (op, value) in value.sep.into_iter().zip(value_iter) {
            while let Some(last_op) = ops.last() {
                if last_op.ty.level() < op.ty.level()
                    || (last_op.ty.level() == op.ty.level()
                        && op.ty.level_type() == DuoOpLevelType::LeftToRight)
                {
                    let right_expr = Box::new(values.pop().unwrap());
                    let left_expr = Box::new(values.pop().unwrap());
                    let total_span = left_expr.span().union(right_expr.span());
                    values.push(Expr::DuoOp(DuoOp {
                        op: last_op.ty,
                        left_expr,
                        right_expr,
                        op_span: last_op.span,
                        total_span,
                    }));
                    ops.pop().unwrap();
                } else if last_op.ty.level() == op.ty.level()
                    && op.ty.level_type() == DuoOpLevelType::RequireParens
                {
                    let left_expr = &values[values.len() - 2];
                    let right_expr = &values[values.len() - 1];
                    return Err(ParseError::Multi(vec![
                        ParseError::Expected {
                            name: "opening parentheses `(`",
                            span: left_expr.span().begining(),
                        },
                        ParseError::Expected {
                            name: "closing parentheses `)`",
                            span: right_expr.span().ending(),
                        },
                    ]));
                } else {
                    break;
                }
            }
            ops.push(op);
            values.push(value);
        }
        while let Some(op) = ops.pop() {
            let right_expr = Box::new(values.pop().unwrap());
            let left_expr = Box::new(values.pop().unwrap());
            let total_span = left_expr.span().union(right_expr.span());
            values.push(Expr::DuoOp(DuoOp {
                op: op.ty,
                left_expr,
                right_expr,
                op_span: op.span,
                total_span,
            }));
        }

        debug_assert_eq!(values.len(), 1);
        Ok(values.pop().unwrap())
    }
}

/// Comma separated list
pub struct ListParser<T> {
    pub parser: T,
    pub allow_empty: bool,
    pub ignore_newline: bool,
}
pub struct ListValue<'a, T> {
    pub elements: Vec<T>,
    pub has_comma: bool,
    pub span: Span<'a>,
}
impl<'a, T> Spanned<'a> for ListValue<'a, T> {
    #[inline]
    fn span(&self) -> Span<'a> {
        self.span
    }
}

impl<'a, T: Parser<'a>> Parser<'a> for ListParser<T> {
    type Output = ListValue<'a, T::Output>;
    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let value = Repeat {
            body: &self.parser,
            sep: Chained2(
                MatchParser::SkipWhitespace {
                    skip_newline: self.ignore_newline,
                },
                MatchParser::Punct(b","),
            ),
            allow_empty: self.allow_empty,
            allow_trailing_sep: true,
        }
        .parse(tokens)?;
        Ok(ListValue {
            elements: value.body,
            has_comma: !value.sep.is_empty(),
            span: value.span,
        })
    }
}

pub struct TuplingExprParser {
    pub ignore_newline: bool,
}

impl<'a> Parser<'a> for TuplingExprParser {
    type Output = Expr<'a>;

    fn parse(&self, tokens: &mut Tokens<'a, '_>) -> Result<Self::Output, ParseError<'a>> {
        let value = ListParser {
            parser: ExprParser {
                ignore_newline: self.ignore_newline,
            },
            ignore_newline: self.ignore_newline,
            allow_empty: false,
        }
        .parse(tokens)?;

        Ok(Expr::Tuple(Tuple {
            values: value.elements,
            span: value.span,
        }))
    }
}
