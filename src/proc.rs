use crate::{
    globals, idents,
    item::{HeadsIter, Item, NumsIter, Token},
    scope::{Scope, ScopeHandle},
    syn::{Defs, Syn},
};
use anyhow::{anyhow, bail, ensure, Context, Result};
use std::{
    f64, fmt,
    ops::{Add, Div, Mul, Sub},
    rc::Rc,
};

#[derive(Clone, Debug)]
pub enum Proc<'src> {
    ListManip(ListManip),
    Arith(Arith),
    Cmp(Cmp),
    Trig(Trig),
    Log,
    Exp,
    Rem,
    Trunc,
    Floor,
    Ceil,
    Error,
    Compound {
        name: Option<&'src str>,
        params: Rc<[&'src str]>,
        scope_handle: ScopeHandle<'src>,
        body: &'src [Syn<'src>],
    },
}

impl<'src> Proc<'src> {
    pub fn apply(&self, args: HeadsIter<'_, 'src>) -> Result<Item<'src>> {
        match self {
            Self::ListManip(list_manip) => list_manip.apply(args),

            Self::Arith(arith) => arith.apply(args.try_into()?),

            Self::Cmp(cmp) => cmp.apply(args.try_into()?),

            Self::Trig(trig) => trig.apply(args.try_into()?),

            Self::Log => {
                let mut args = NumsIter::try_from(args)?;
                let Some(operand) = args.next() else {
                    bail!("Wrong number of arguments for {self}");
                };
                let base = args.next().unwrap_or(f64::consts::E);
                ensure!(
                    args.next().is_none(),
                    "Wrong number of arguments for {self}"
                );
                Ok(Item::Num(operand.log(base)))
            }

            Self::Exp => {
                let mut args = NumsIter::try_from(args)?;
                let Some(exp) = args.next() else {
                    bail!("Wrong number of arguments for {self}");
                };
                ensure!(
                    args.next().is_none(),
                    "Wrong number of arguments for {self}"
                );
                Ok(Item::Num(exp.exp()))
            }

            Self::Rem => {
                let mut args = NumsIter::try_from(args)?;
                let Some((first, second)) =
                    args.next().and_then(|first| Some((first, args.next()?)))
                else {
                    bail!("Wrong number of arguments for {self}");
                };
                Ok(Item::Num(first % second))
            }

            Self::Trunc => {
                let mut args = NumsIter::try_from(args)?;
                let Some(exp) = args.next() else {
                    bail!("Wrong number of arguments for {self}");
                };
                ensure!(
                    args.next().is_none(),
                    "Wrong number of arguments for {self}"
                );
                Ok(Item::Num(exp.trunc()))
            }

            Self::Floor => {
                let mut args = NumsIter::try_from(args)?;
                let Some(exp) = args.next() else {
                    bail!("Wrong number of arguments for {self}");
                };
                ensure!(
                    args.next().is_none(),
                    "Wrong number of arguments for {self}"
                );
                Ok(Item::Num(exp.floor()))
            }

            Self::Ceil => {
                let mut args = NumsIter::try_from(args)?;
                let Some(exp) = args.next() else {
                    bail!("Wrong number of arguments for {self}");
                };
                ensure!(
                    args.next().is_none(),
                    "Wrong number of arguments for {self}"
                );
                Ok(Item::Num(exp.ceil()))
            }

            Self::Error => {
                ensure!(
                    args.clone().count() > 0,
                    "Wrong number of arguments for {self}"
                );
                bail!(args.map(|arg| format!("{arg} ")).collect::<String>())
            }

            Self::Compound {
                name,
                params,
                scope_handle: scope,
                body,
            } => {
                let debug = globals::debug();

                if debug {
                    eprint!("{}(", name.unwrap_or(idents::LAMBDA));
                }
                let mut args = args;
                let scope = Scope::new_local(scope)?;
                for (i, param) in params.iter().enumerate() {
                    if let Some(item) = args.next() {
                        if debug {
                            eprint!("{param}: {item}");
                            if i != params.len() - 1 {
                                eprint!(", ");
                            }
                        }
                        scope.add(param, item.clone())?;
                    } else {
                        bail!("Expected {} parameter(s), got {i}", params.len());
                    }
                }
                ensure!(
                    args.next().is_none(),
                    "Too many parameters for {self}, expected {}",
                    params.len()
                );
                if debug {
                    eprintln!(")");
                    eprintln!();
                }

                let mut item = Item::nil();
                for syn in *body {
                    item = syn
                        .eval(&scope, Defs::Allowed)
                        .with_context(|| format!("[while evaluating {self}]"))?;
                }
                if matches!(item, Item::Defined) {
                    bail!("Ill-placed '{}'", idents::DEFINE);
                } else {
                    Ok(item)
                }
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
            Self::Log => write!(f, "<{}>", idents::LOG),
            Self::Exp => write!(f, "<{}>", idents::EXP),
            Self::Rem => write!(f, "<{}>", idents::REM),
            Self::Trunc => write!(f, "<{}>", idents::TRUNC),
            Self::Floor => write!(f, "<{}>", idents::FLOOR),
            Self::Ceil => write!(f, "<{}>", idents::CEIL),
            Self::Error => write!(f, "<{}>", idents::ERROR),
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
    Car,
    Cdr,
}

impl ListManip {
    fn apply<'src>(self, mut args: HeadsIter<'_, 'src>) -> Result<Item<'src>> {
        match self {
            Self::Cons => {
                let (head, tail) = args
                    .next()
                    .and_then(|head| args.next().map(|tail| (head, tail)))
                    .with_context(|| anyhow!("Wrong number of arguments for {self}"))?;
                ensure!(
                    args.next().is_none(),
                    "Wrong number of arguments for {self}"
                );
                Ok(Item::cons(head.clone(), tail.clone()))
            }
            Self::Car => {
                ensure!(
                    args.clone().count() == 1,
                    "Wrong number of arguments for {self}"
                );
                let item = args.next();
                if let Some(Item::List(Some(list))) = item {
                    Ok(list.head.clone())
                } else {
                    bail!(
                        "Cannot retrieve head from {}",
                        item.expect("already checked")
                    );
                }
            }
            Self::Cdr => {
                ensure!(
                    args.clone().count() == 1,
                    "Wrong number of arguments for {self}"
                );
                let item = args.next();
                if let Some(Item::List(Some(list))) = item {
                    Ok(list.tail.clone())
                } else {
                    bail!(
                        "Cannot retrieve tail from {}",
                        item.expect("already checked")
                    );
                }
            }
        }
    }
}

impl fmt::Display for ListManip {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Cons => write!(f, "<{}>", idents::CONS),
            Self::Car => write!(f, "<{}>", idents::CAR),
            Self::Cdr => write!(f, "<{}>", idents::CDR),
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
            return match self {
                Self::Add => Ok(Item::Num(0.0)),
                Self::Mul => Ok(Item::Num(1.0)),
                Self::Sub | Self::Div => {
                    bail!("Wrong number of arguments for {self}")
                }
            };
        }
        ensure!(
            !(self == Self::Div && args.clone().any(|item| item == 0.0)),
            "Cannot divide by zero"
        );
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
        .ok_or_else(|| anyhow!("unreachable arithmetic"))
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
        let (token, _) = args.fold((true, None), |(still_true, prev), num| {
            if still_true {
                prev.map_or((true, Some(num)), |prev| (cmp(prev, num), Some(num)))
            } else {
                (false, None)
            }
        });
        Ok(Item::Token(if token { Token::True } else { Token::False }))
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
    fn apply<'src>(self, mut args: NumsIter<'_, 'src>) -> Result<Item<'src>> {
        ensure!(
            args.clone().count() == 1,
            "Wrong number of arguments for {self}"
        );
        let func = match self {
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
        Ok(Item::Num(func(args.next().expect("already checked"))))
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
