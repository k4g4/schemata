use crate::{
    idents,
    item::Item,
    memory::ScopeHandle,
    proc::{Arith, Cmp, Cxr, Is, ListManip, Proc, Trig},
    sexpr::SExpr,
    utils,
};
use anyhow::{bail, ensure, Context, Result};
use std::{array, borrow::Cow, fmt, process, rc::Rc};

#[derive(PartialEq, Clone, Debug)]
pub enum Syn<'src> {
    Num(f64),
    String(Cow<'src, str>),
    Ident(&'src str),
    Reserved(Reserved),
    SExpr(SExpr<'src>),
    Quoted(Box<Syn<'src>>),
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Reserved {
    Define,
    Lambda,
    Let,
    Begin,
    Cond,
    Else,
    If,
    And,
    Or,
}

impl Reserved {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Define => idents::DEFINE,
            Self::Lambda => idents::LAMBDA,
            Self::Let => idents::LET,
            Self::Begin => idents::BEGIN,
            Self::Cond => idents::COND,
            Self::Else => idents::ELSE,
            Self::If => idents::IF,
            Self::And => idents::AND,
            Self::Or => idents::OR,
        }
    }
}

impl fmt::Display for Reserved {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Defs {
    Allowed,
    NotAllowed,
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Policy {
    Defer,
    Resolve,
}

impl<'src> Syn<'src> {
    pub fn eval(&self, scope: ScopeHandle<'src>, defs: Defs, policy: Policy) -> Result<Item<'src>> {
        scope.collect_garbage()?;

        match self {
            &Self::Num(n) => Ok(Item::Num(n)),

            Self::String(string) => Ok(Item::String(Rc::new(string.clone()))),

            Self::Ident(".") => bail!("Improperly placed dot"),

            &Self::Ident(ident) => {
                if let Some(lookup) = scope.lookup(ident)? {
                    Ok(lookup)
                } else {
                    let builtin = match ident {
                        idents::APPLY => Item::Proc(Proc::Apply),
                        idents::EVAL => Item::Proc(Proc::Eval(scope.clone())),
                        idents::EXIT => {
                            print!("-- Exited --");
                            process::exit(0)
                        }

                        idents::CONS => Item::Proc(Proc::ListManip(ListManip::Cons)),
                        idents::FIRST => Item::Proc(Proc::ListManip(ListManip::Nth(0))),
                        idents::SECOND => Item::Proc(Proc::ListManip(ListManip::Nth(1))),
                        idents::THIRD => Item::Proc(Proc::ListManip(ListManip::Nth(2))),
                        idents::FOURTH => Item::Proc(Proc::ListManip(ListManip::Nth(3))),
                        idents::FIFTH => Item::Proc(Proc::ListManip(ListManip::Nth(4))),
                        idents::SIXTH => Item::Proc(Proc::ListManip(ListManip::Nth(5))),
                        idents::SEVENTH => Item::Proc(Proc::ListManip(ListManip::Nth(6))),
                        idents::EIGHTH => Item::Proc(Proc::ListManip(ListManip::Nth(7))),
                        idents::NINTH => Item::Proc(Proc::ListManip(ListManip::Nth(8))),
                        idents::TENTH => Item::Proc(Proc::ListManip(ListManip::Nth(9))),

                        idents::TRUE => Item::bool(true),
                        idents::FALSE => Item::bool(false),
                        idents::VOID => Item::void(),

                        idents::ADD => Item::Proc(Proc::Arith(Arith::Add)),
                        idents::SUB => Item::Proc(Proc::Arith(Arith::Sub)),
                        idents::MUL => Item::Proc(Proc::Arith(Arith::Mul)),
                        idents::DIV => Item::Proc(Proc::Arith(Arith::Div)),

                        idents::EQ => Item::Proc(Proc::Cmp(Cmp::Eq)),
                        idents::GT => Item::Proc(Proc::Cmp(Cmp::Gt)),
                        idents::GE => Item::Proc(Proc::Cmp(Cmp::Ge)),
                        idents::LT => Item::Proc(Proc::Cmp(Cmp::Lt)),
                        idents::LE => Item::Proc(Proc::Cmp(Cmp::Le)),

                        idents::LOG => Item::Proc(Proc::Log),
                        idents::EXP => Item::Proc(Proc::Exp),
                        idents::REM => Item::Proc(Proc::Rem),
                        idents::MOD => Item::Proc(Proc::Mod),
                        idents::TRUNC => Item::Proc(Proc::Trunc),
                        idents::FLOOR => Item::Proc(Proc::Floor),
                        idents::CEIL => Item::Proc(Proc::Ceil),

                        idents::SIN => Item::Proc(Proc::Trig(Trig::Sin)),
                        idents::COS => Item::Proc(Proc::Trig(Trig::Cos)),
                        idents::TAN => Item::Proc(Proc::Trig(Trig::Tan)),
                        idents::ASIN => Item::Proc(Proc::Trig(Trig::Asin)),
                        idents::ACOS => Item::Proc(Proc::Trig(Trig::Acos)),
                        idents::ATAN => Item::Proc(Proc::Trig(Trig::Atan)),
                        idents::SINH => Item::Proc(Proc::Trig(Trig::Sinh)),
                        idents::COSH => Item::Proc(Proc::Trig(Trig::Cosh)),
                        idents::TANH => Item::Proc(Proc::Trig(Trig::Tanh)),
                        idents::ASINH => Item::Proc(Proc::Trig(Trig::Asinh)),
                        idents::ACOSH => Item::Proc(Proc::Trig(Trig::Acosh)),
                        idents::ATANH => Item::Proc(Proc::Trig(Trig::Atanh)),

                        idents::DISP => Item::Proc(Proc::Display),
                        idents::NEWL => Item::Proc(Proc::Newline),
                        idents::ERROR => Item::Proc(Proc::Error),
                        idents::STR_APP => Item::Proc(Proc::StrApp),

                        idents::IS_ATOM => Item::Proc(Proc::Is(Is::Atom)),
                        idents::IS_BOOL => Item::Proc(Proc::Is(Is::Bool)),
                        idents::IS_INT => Item::Proc(Proc::Is(Is::Int)),
                        idents::IS_LIST => Item::Proc(Proc::Is(Is::List)),
                        idents::IS_NUMBER => Item::Proc(Proc::Is(Is::Number)),
                        idents::IS_NULL => Item::Proc(Proc::Is(Is::Null)),
                        idents::IS_PAIR => Item::Proc(Proc::Is(Is::Pair)),
                        idents::IS_PROC => Item::Proc(Proc::Is(Is::Proc)),
                        idents::IS_STRING => Item::Proc(Proc::Is(Is::String)),
                        idents::IS_SYMBOL => Item::Proc(Proc::Is(Is::Symbol)),

                        idents::IS_EQ => Item::Proc(Proc::IsEq),
                        idents::IS_EQUAL => Item::Proc(Proc::IsEqual),

                        _ => {
                            if let [b'c', as_and_ds @ .., b'r'] = ident.as_bytes() {
                                ensure!(
                                    as_and_ds.iter().all(|a_or_d| matches!(a_or_d, b'a' | b'd')),
                                    "Failed to find definition for '{ident}'"
                                );
                                fn get_cxr<const LEN: usize>(as_and_ds: &[u8]) -> [Cxr; LEN] {
                                    array::from_fn(|i| match as_and_ds[i] {
                                        b'a' => Cxr::Car,
                                        b'd' => Cxr::Cdr,
                                        _ => panic!("should have already checked as and ds"),
                                    })
                                }
                                let list_manip = match as_and_ds.len() {
                                    1 => ListManip::Cxr(get_cxr(as_and_ds)),
                                    2 => ListManip::Cxxr(get_cxr(as_and_ds)),
                                    3 => ListManip::Cxxxr(get_cxr(as_and_ds)),
                                    4 => ListManip::Cxxxxr(get_cxr(as_and_ds)),
                                    _ => bail!("Failed to find definition for '{ident}'"),
                                };
                                Item::Proc(Proc::ListManip(list_manip))
                            } else {
                                bail!("Failed to find definition for '{ident}'")
                            }
                        }
                    };

                    Ok(builtin)
                }
            }

            Self::Reserved(reserved) => bail!("Unexpected '{reserved}'"),

            Self::SExpr(s_expr) => {
                let item = s_expr.eval(scope, defs)?;
                if policy == Policy::Defer {
                    Ok(item)
                } else {
                    Ok(item.resolve()?)
                }
            }

            Self::Quoted(quoted) => {
                fn eval_quoted<'src>(
                    syn: &Syn<'src>,
                    scope: ScopeHandle<'src>,
                ) -> Result<Item<'src>> {
                    match syn {
                        Syn::Ident(ident) => Ok(Item::Sym(ident)),
                        Syn::Reserved(reserved) => Ok(Item::Sym(reserved.as_str())),
                        Syn::SExpr(s_expr) => Ok(Item::from_items(
                            s_expr.iter().map(|syn| eval_quoted(syn, scope)),
                        )?),
                        _ => syn.eval(scope, Defs::NotAllowed, Policy::Resolve),
                    }
                }
                eval_quoted(quoted, scope)
            }
        }
    }
}

impl fmt::Display for Syn<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let width = f.width().unwrap_or(0);
        if f.alternate() {
            utils::indent(f, f.width().unwrap_or(0))?;
        }
        match self {
            Self::Num(num) => write!(f, "{num}"),
            Self::String(string) => write!(f, "\"{string}\""),
            Self::Ident(ident) => write!(f, "{ident}"),
            Self::Reserved(reserved) => write!(f, "{reserved}"),
            Self::SExpr(s_expr) if f.alternate() => write!(f, "{s_expr:#width$}"),
            Self::SExpr(s_expr) => write!(f, "{s_expr:width$}"),
            Self::Quoted(quoted) => write!(f, "'{quoted}"),
        }
    }
}

pub fn eval_body<'src>(
    body: &[Syn<'src>],
    scope: ScopeHandle<'src>,
    defs: Defs,
) -> Result<Item<'src>> {
    let (last, must_resolve) = body.split_last().context("Unexpected empty body")?;
    for syn in must_resolve {
        syn.eval(scope, defs, Policy::Resolve)?;
    }
    last.eval(scope, defs, Policy::Defer)
}
