use crate::{
    globals, idents,
    item::{Item, Pair, Token},
    memory::ScopeRef,
    syn::{Defs, Policy, Syn},
    utils::{ArgsIter, ItemsIter, NumsIter},
};
use anyhow::{anyhow, bail, ensure, Context, Result};
use std::{
    borrow::Cow,
    f64, fmt, iter,
    ops::{Add, Div, Mul, Sub},
    ptr,
    rc::Rc,
    str,
};

#[derive(Clone)]
pub enum Proc<'src> {
    ListManip(ListManip),
    Arith(Arith),
    Cmp(Cmp),
    Trig(Trig),
    Is(Is),
    IsEq,
    IsEqual,
    Log,
    Exp,
    Rem,
    Mod,
    Trunc,
    Floor,
    Ceil,
    Apply,
    Eval(ScopeRef<'src>),
    Display,
    Newline,
    Error,
    StrApp,
    Compound {
        name: Option<&'src str>,
        params: Rc<[&'src str]>,
        parent: ScopeRef<'src>,
        body: Rc<[Syn<'src>]>,
    },
}

impl<'src> Proc<'src> {
    pub fn apply(&self, args: ItemsIter<'_, 'src>) -> Result<Item<'src>> {
        match self {
            Self::ListManip(list_manip) => list_manip.apply(args),

            Self::Arith(arith) => arith.apply(args.try_into()?),

            Self::Cmp(cmp) => cmp.apply(args.try_into()?),

            Self::Trig(trig) => trig.apply(args.try_into()?),

            Self::Is(is) => is.apply(args),

            Self::IsEq => {
                let ([lhs, rhs], []) = args.get(self)?;
                let eq = match (lhs, rhs) {
                    (Item::Num(lhs), Item::Num(rhs)) => lhs == rhs,
                    (Item::Token(lhs), Item::Token(rhs)) => lhs == rhs,
                    (Item::Sym(lhs), Item::Sym(rhs)) => lhs == rhs,
                    (Item::String(lhs), Item::String(rhs)) => Rc::as_ptr(lhs) == Rc::as_ptr(rhs),
                    (Item::Pair(None), Item::Pair(None)) => true,
                    (Item::Pair(Some(lhs)), Item::Pair(Some(rhs))) => {
                        Rc::as_ptr(lhs) == Rc::as_ptr(rhs)
                    }
                    (
                        Item::Proc(Self::Compound { params: lhs, .. }),
                        Item::Proc(Self::Compound { params: rhs, .. }),
                    ) => ptr::eq(Rc::as_ptr(lhs), Rc::as_ptr(rhs)),
                    (Item::Proc(lhs), Item::Proc(rhs)) => format!("{lhs}") == format!("{rhs}"),
                    _ => false,
                };
                Ok(Item::bool(eq))
            }

            Self::IsEqual => {
                let ([lhs, rhs], []) = args.get(self)?;
                match (lhs, rhs) {
                    (Item::Num(_), Item::Num(_))
                    | (Item::Token(_), Item::Token(_))
                    | (Item::Sym(_), Item::Sym(_))
                    | (Item::Proc(_), Item::Proc(_)) => Self::IsEq.apply(args),
                    (Item::String(lhs), Item::String(rhs)) => Ok(Item::bool(lhs == rhs)),
                    (Item::Pair(None), Item::Pair(None)) => Ok(Item::bool(true)),
                    (Item::Pair(Some(lhs)), Item::Pair(Some(rhs))) => {
                        let (
                            Pair {
                                head: lhs_head,
                                tail: lhs_tail,
                            },
                            Pair {
                                head: rhs_head,
                                tail: rhs_tail,
                            },
                        ) = (lhs.as_ref(), rhs.as_ref());

                        let heads = Pair {
                            head: lhs_head.clone(),
                            tail: Item::cons(rhs_head.clone(), Item::nil()),
                        };
                        if !self.apply(ItemsIter::new(Some(&heads)))?.is_truthy() {
                            Ok(Item::bool(false))
                        } else {
                            let tails = Pair {
                                head: lhs_tail.clone(),
                                tail: Item::cons(rhs_tail.clone(), Item::nil()),
                            };
                            if !self.apply(ItemsIter::new(Some(&tails)))?.is_truthy() {
                                Ok(Item::bool(false))
                            } else {
                                Ok(Item::bool(true))
                            }
                        }
                    }
                    _ => Ok(Item::bool(false)),
                }
            }

            Self::Log => {
                let ([operand], [base]) = NumsIter::try_from(args)?.get(self)?;
                let base = base.unwrap_or(f64::consts::E);
                Ok(Item::Num(operand.log(base)))
            }

            Self::Exp => {
                let ([exp], []) = NumsIter::try_from(args)?.get(self)?;
                Ok(Item::Num(exp.exp()))
            }

            Self::Rem => {
                let ([dividend, divisor], []) = NumsIter::try_from(args)?.get(self)?;
                Ok(Item::Num(dividend % divisor))
            }

            Self::Mod => {
                let ([dividend, divisor], []) = NumsIter::try_from(args)?.get(self)?;

                Ok(Item::Num(dividend.rem_euclid(divisor)))
            }

            Self::Trunc => {
                let ([num], []) = NumsIter::try_from(args)?.get(self)?;
                Ok(Item::Num(num.trunc()))
            }

            Self::Floor => {
                let ([num], []) = NumsIter::try_from(args)?.get(self)?;
                Ok(Item::Num(num.floor()))
            }

            Self::Ceil => {
                let ([num], []) = NumsIter::try_from(args)?.get(self)?;
                Ok(Item::Num(num.ceil()))
            }

            Self::Apply => {
                let ([proc, args], []) = args.get(self)?;
                let invoke = Item::cons(proc.clone(), args.clone());
                invoke.apply()
            }

            &Self::Eval(scope) => {
                let ([item], []) = args.get(self)?;
                let syn = item.into_syn()?;
                syn.eval(scope, Defs::Allowed, Policy::Defer)
            }

            Self::Display => {
                let ([item], []) = args.get(self)?;
                if let Item::String(string) = item {
                    print!("{string}");
                } else {
                    print!("{item}");
                }
                Ok(Item::void())
            }

            Self::Newline => {
                let ([], []) = args.get(self)?;
                println!();
                Ok(Item::void())
            }

            Self::Error => {
                ensure!(args.clone().count() > 0, "No arguments provided for {self}");
                bail!(args.map(|arg| format!("{arg} ")).collect::<String>())
            }

            Self::StrApp => args
                .clone()
                .try_fold(String::new(), |appended, item| {
                    if let Item::String(string) = item {
                        Ok(appended + string)
                    } else {
                        Err(anyhow!("Arguments must be strings"))
                    }
                })
                .map(Cow::Owned)
                .map(Rc::new)
                .map(Item::String),

            Self::Compound {
                name,
                params,
                parent,
                body,
            } => {
                let debug = globals::debug();
                let scope = parent.new_local()?;
                let mut args = args;

                if debug {
                    eprint!("{}(", name.unwrap_or(idents::LAMBDA));
                }
                let before_dot_len = params
                    .iter()
                    .position(|&param| param == ".")
                    .unwrap_or(params.len());
                let before_dot_iter = params[..before_dot_len].iter().enumerate();
                for (i, &param) in before_dot_iter {
                    if let Some(item) = args.next() {
                        if debug {
                            eprint!("{param}: {item}");
                            if i != before_dot_len - 1 {
                                eprint!(", ");
                            }
                        }
                        scope.add(param, item.clone())?;
                    } else {
                        bail!("Expected {} argument(s), received {i}", before_dot_len);
                    }
                }
                if before_dot_len < params.len() {
                    let &rest_param = params.last().expect("dot is before last param");
                    let rest =
                        Item::from_items(args.cloned().map(Ok).collect::<Vec<_>>().into_iter())?;
                    if debug {
                        if before_dot_len != 0 {
                            eprint!(", ");
                        }
                        eprint!("{rest_param}: {rest}");
                    }
                    scope.add(rest_param, rest)?;
                } else {
                    ensure!(
                        args.next().is_none(),
                        "Too many arguments for {self}, expected {}",
                        params.len()
                    );
                }
                if debug {
                    eprintln!(") (Scope {scope:?})");
                    eprintln!();
                }

                Ok(Item::Deferred {
                    name: name.clone(),
                    scope,
                    body: body.clone(),
                })
            }
        }
    }
}

impl fmt::Display for Proc<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ListManip(list_manip) => write!(f, "{list_manip}"),
            Self::Arith(arith) => write!(f, "{arith}"),
            Self::Cmp(cmp) => write!(f, "{cmp}"),
            Self::Trig(trig) => write!(f, "{trig}"),
            Self::Is(is) => write!(f, "{is}"),
            Self::IsEq => write!(f, "<{}>", idents::IS_EQ),
            Self::IsEqual => write!(f, "<{}>", idents::IS_EQUAL),
            Self::Log => write!(f, "<{}>", idents::LOG),
            Self::Exp => write!(f, "<{}>", idents::EXP),
            Self::Rem => write!(f, "<{}>", idents::REM),
            Self::Mod => write!(f, "<{}>", idents::MOD),
            Self::Trunc => write!(f, "<{}>", idents::TRUNC),
            Self::Floor => write!(f, "<{}>", idents::FLOOR),
            Self::Ceil => write!(f, "<{}>", idents::CEIL),
            Self::Apply => write!(f, "<{}>", idents::APPLY),
            Self::Eval(_) => write!(f, "<{}>", idents::EVAL),
            Self::Display => write!(f, "<{}>", idents::DISP),
            Self::Newline => write!(f, "<{}>", idents::NEWL),
            Self::Error => write!(f, "<{}>", idents::ERROR),
            Self::StrApp => write!(f, "<{}>", idents::STR_APP),
            Self::Compound { name: None, .. } => write!(f, "<{}>", idents::LAMBDA),
            Self::Compound {
                name: Some(name), ..
            } => write!(f, "<proc '{name}'>"),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum ListManip {
    Cons,
    Nth(u8),
    Cxr([Cxr; 1]),
    Cxxr([Cxr; 2]),
    Cxxxr([Cxr; 3]),
    Cxxxxr([Cxr; 4]),
}

#[derive(Copy, Clone, Debug)]
pub enum Cxr {
    Car,
    Cdr,
}

impl ListManip {
    fn apply<'src>(self, args: ItemsIter<'_, 'src>) -> Result<Item<'src>> {
        fn access<'src, 'item>(
            cxrs: impl Iterator<Item = Cxr>,
            item: &Item<'src>,
        ) -> Result<Item<'src>> {
            cxrs.into_iter()
                .try_fold(item, |item, cxr| match item {
                    Item::Pair(Some(pair)) => match cxr {
                        Cxr::Car => Ok(&pair.head),
                        Cxr::Cdr => Ok(&pair.tail),
                    },
                    Item::Pair(_) => bail!("Cannot dereference an empty list"),
                    _ => bail!("Cannot dereference '{item}'"),
                })
                .cloned()
        }

        macro_rules! cxr_impl {
            ($cxrs:ident) => {{
                let ([list], []) = args.get(self)?;
                access($cxrs.into_iter().rev(), list)
            }};
        }

        match self {
            Self::Cons => {
                let ([head, tail], []) = args.get(self)?;
                Ok(Item::cons(head.clone(), tail.clone()))
            }

            Self::Nth(n) => {
                let ([list], []) = args.get(self)?;
                access(
                    iter::repeat(Cxr::Cdr)
                        .take(n as _)
                        .chain(iter::once(Cxr::Car)),
                    list,
                )
            }

            Self::Cxr(cxrs) => cxr_impl!(cxrs),
            Self::Cxxr(cxrs) => cxr_impl!(cxrs),
            Self::Cxxxr(cxrs) => cxr_impl!(cxrs),
            Self::Cxxxxr(cxrs) => cxr_impl!(cxrs),
        }
    }
}

impl fmt::Display for ListManip {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut cxr_helper = |cxrs: &[_]| {
            let mut as_and_ds = [0; 4];
            for (i, cxr) in cxrs.iter().enumerate() {
                as_and_ds[i] = match cxr {
                    Cxr::Car => b'a',
                    Cxr::Cdr => b'd',
                };
            }
            let as_and_ds = str::from_utf8(&as_and_ds[..cxrs.len()]).expect("must be utf8");
            write!(f, "<c{}r>", as_and_ds)
        };
        match self {
            Self::Cons => write!(f, "<{}>", idents::CONS),
            Self::Nth(0) => write!(f, "<{}>", idents::FIRST),
            Self::Nth(1) => write!(f, "<{}>", idents::SECOND),
            Self::Nth(2) => write!(f, "<{}>", idents::THIRD),
            Self::Nth(3) => write!(f, "<{}>", idents::FOURTH),
            Self::Nth(4) => write!(f, "<{}>", idents::FIFTH),
            Self::Nth(5) => write!(f, "<{}>", idents::SIXTH),
            Self::Nth(6) => write!(f, "<{}>", idents::SEVENTH),
            Self::Nth(7) => write!(f, "<{}>", idents::EIGHTH),
            Self::Nth(8) => write!(f, "<{}>", idents::NINTH),
            Self::Nth(9) => write!(f, "<{}>", idents::TENTH),
            Self::Nth(n) => panic!("Nth created with unexpected index: {n}"),
            Self::Cxr(cxrs) => cxr_helper(cxrs),
            Self::Cxxr(cxrs) => cxr_helper(cxrs),
            Self::Cxxxr(cxrs) => cxr_helper(cxrs),
            Self::Cxxxxr(cxrs) => cxr_helper(cxrs),
        }
    }
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Arith {
    Add,
    Sub,
    Mul,
    Div,
}

impl Arith {
    fn apply<'src>(self, args: NumsIter<'_, 'src>) -> Result<Item<'src>> {
        if args.clone().count() == 0 {
            match self {
                Self::Add => {
                    return Ok(Item::Num(0.0));
                }
                Self::Mul => {
                    return Ok(Item::Num(1.0));
                }
                Self::Sub | Self::Div => {
                    bail!("No arguments provided for {self}");
                }
            };
        }
        // deny certain cases where arg is 0
        if self == Self::Div {
            if args.clone().count() == 1 {
                ensure!(args.clone().next().unwrap() != 0.0, "Cannot divide by zero");
            } else {
                ensure!(
                    args.clone().skip(1).all(|num| num != 0.0),
                    "Cannot divide by zero"
                );
            }
        }
        if args.clone().count() == 1 {
            return match (self, args.clone().next()) {
                (Self::Add | Self::Mul, Some(num)) => Ok(Item::Num(num)),
                (Self::Sub, Some(num)) => Ok(Item::Num(-num)),
                (Self::Div, Some(num)) => Ok(Item::Num(1.0 / num)),
                _ => bail!("unreachable arithmetic"),
            };
        }

        args.reduce(match self {
            Self::Add => <_ as Add>::add,
            Self::Sub => <_ as Sub>::sub,
            Self::Mul => <_ as Mul>::mul,
            Self::Div => <_ as Div>::div,
        })
        .map(Item::Num)
        .context("unreachable arithmetic")
    }
}

impl fmt::Display for Arith {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<({})>",
            match self {
                Self::Add => idents::ADD,
                Self::Sub => idents::SUB,
                Self::Mul => idents::MUL,
                Self::Div => idents::DIV,
            }
        )
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Cmp {
    Eq,
    Gt,
    Ge,
    Lt,
    Le,
}

impl Cmp {
    fn apply<'src>(self, args: NumsIter<'_, 'src>) -> Result<Item<'src>> {
        let cmp = match self {
            Self::Eq => |lhs, rhs| lhs == rhs,
            Self::Gt => |lhs, rhs| lhs > rhs,
            Self::Ge => |lhs, rhs| lhs >= rhs,
            Self::Lt => |lhs, rhs| lhs < rhs,
            Self::Le => |lhs, rhs| lhs <= rhs,
        };
        let (bool, _) = args.fold((true, None), |(still_true, prev), num| {
            if still_true {
                prev.map_or((true, Some(num)), |prev| (cmp(prev, num), Some(num)))
            } else {
                (false, None)
            }
        });
        Ok(Item::bool(bool))
    }
}

impl fmt::Display for Cmp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<({})>",
            match self {
                Self::Eq => idents::EQ,
                Self::Gt => idents::GT,
                Self::Ge => idents::GE,
                Self::Lt => idents::LT,
                Self::Le => idents::LE,
            }
        )
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Trig {
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Sinh,
    Cosh,
    Tanh,
    Asinh,
    Acosh,
    Atanh,
}

impl Trig {
    fn apply<'src>(self, args: NumsIter<'_, 'src>) -> Result<Item<'src>> {
        let ([num], []) = args.get(self)?;
        let trig = match self {
            Self::Sin => f64::sin,
            Self::Cos => f64::cos,
            Self::Tan => f64::tan,
            Self::Asin => f64::asin,
            Self::Acos => f64::acos,
            Self::Atan => f64::atan,
            Self::Sinh => f64::sinh,
            Self::Cosh => f64::cosh,
            Self::Tanh => f64::tanh,
            Self::Asinh => f64::asinh,
            Self::Acosh => f64::acosh,
            Self::Atanh => f64::atanh,
        };
        Ok(Item::Num(trig(num)))
    }
}

impl fmt::Display for Trig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Sin => write!(f, "<{}>", idents::SIN),
            Self::Cos => write!(f, "<{}>", idents::COS),
            Self::Tan => write!(f, "<{}>", idents::TAN),
            Self::Asin => write!(f, "<{}>", idents::ASIN),
            Self::Acos => write!(f, "<{}>", idents::ACOS),
            Self::Atan => write!(f, "<{}>", idents::ATAN),
            Self::Sinh => write!(f, "<{}>", idents::SINH),
            Self::Cosh => write!(f, "<{}>", idents::COSH),
            Self::Tanh => write!(f, "<{}>", idents::TANH),
            Self::Asinh => write!(f, "<{}>", idents::ASINH),
            Self::Acosh => write!(f, "<{}>", idents::ACOSH),
            Self::Atanh => write!(f, "<{}>", idents::ATANH),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Is {
    Atom,
    Bool,
    Int,
    List,
    Number,
    Null,
    Pair,
    Proc,
    String,
    Symbol,
}

impl Is {
    fn apply<'src>(self, args: ItemsIter<'_, 'src>) -> Result<Item<'src>> {
        let ([item], []) = args.get(self)?;
        let is = match (self, item) {
            (
                Self::Atom,
                Item::Token(Token::True)
                | Item::Token(Token::False)
                | Item::Num(_)
                | Item::Sym(_)
                | Item::String(_)
                | Item::Pair(None),
            ) => true,
            (Self::Bool, Item::Token(Token::True) | Item::Token(Token::False)) => true,
            (Self::Int, Item::Num(n)) if *n == n.trunc() => true,
            (Self::Number, Item::Num(_)) => true,
            (Self::Null, Item::Pair(None)) => true,
            (Self::Pair, Item::Pair(Some(_))) => true,
            (Self::Proc, Item::Proc(_)) => true,
            (Self::String, Item::String(_)) => true,
            (Self::Symbol, Item::Sym(_)) => true,
            (Self::List, Item::Pair(pair)) => 'traverse: {
                let mut pair = pair;
                while let Some(Pair { tail, .. }) = pair.as_deref() {
                    if let Item::Pair(next_pair) = tail {
                        pair = next_pair;
                    } else {
                        break 'traverse false;
                    }
                }
                true
            }
            _ => false,
        };
        Ok(Item::bool(is))
    }
}

impl fmt::Display for Is {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Atom => write!(f, "<{}>", idents::IS_ATOM),
            Self::Bool => write!(f, "<{}>", idents::IS_BOOL),
            Self::Int => write!(f, "<{}>", idents::IS_INT),
            Self::List => write!(f, "<{}>", idents::IS_LIST),
            Self::Number => write!(f, "<{}>", idents::IS_NUMBER),
            Self::Null => write!(f, "<{}>", idents::IS_NULL),
            Self::Pair => write!(f, "<{}>", idents::IS_PAIR),
            Self::Proc => write!(f, "<{}>", idents::IS_PROC),
            Self::String => write!(f, "<{}>", idents::IS_STRING),
            Self::Symbol => write!(f, "<{}>", idents::IS_SYMBOL),
        }
    }
}
