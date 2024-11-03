use crate::{
    globals, idents,
    scope::{BorrowedScope, Scope, ScopeId, StoredScope},
    syn::{Defs, Syn},
};
use anyhow::{anyhow, bail, ensure, Context, Error, Result};
use std::{
    f64, fmt,
    ops::{Add, Div, Mul, Sub},
    rc::Rc,
};

#[derive(Clone, Debug)]
pub enum ItemScopeGeneric<'src, Proc> {
    Num(f64),
    List(Option<Rc<List<'src>>>),
    Proc(Proc),
    Token(Token),
    Defined,
}

pub type Item<'src> = ItemScopeGeneric<'src, Proc<'src>>;

pub type StoredItem<'src> = ItemScopeGeneric<'src, StoredProc<'src>>;

impl<'src> StoredItem<'src> {
    pub fn into_borrowed(self, id: ScopeId) -> Result<Item<'src>> {
        Ok(match self {
            Self::Num(num) => Item::Num(num),
            Self::List(list) => Item::List(list),
            Self::Proc(proc) => Item::Proc(proc.into_borrowed(id)?),
            Self::Token(token) => Item::Token(token),
            Self::Defined => Item::Defined,
        })
    }
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Token {
    True,
    False,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::True => write!(f, "{}", idents::TRUE),
            Self::False => write!(f, "{}", idents::FALSE),
        }
    }
}

impl<'src> Item<'src> {
    pub fn nil() -> Self {
        Self::List(None)
    }

    pub fn cons(head: Item<'src>, tail: Item<'src>) -> Self {
        Self::List(Some(Rc::new(List { head, tail })))
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Self::Token(Token::False))
    }

    pub fn into_stored(self, id: ScopeId) -> StoredItem<'src> {
        match self {
            Self::Num(num) => StoredItem::Num(num),
            Self::List(list) => StoredItem::List(list),
            Self::Proc(proc) => StoredItem::Proc(proc.into_stored(id)),
            Self::Token(token) => StoredItem::Token(token),
            Self::Defined => StoredItem::Defined,
        }
    }

    fn iter(&self) -> HeadsIter<'_, 'src> {
        HeadsIter(if let Self::List(list) = self {
            list.as_deref()
        } else {
            None
        })
    }

    pub fn apply(self) -> Result<Self> {
        if let Self::List(list) = &self {
            match list.as_deref() {
                Some(List {
                    head: Self::Proc(proc),
                    tail,
                }) => proc.apply(tail.iter()),

                Some(List { head, .. }) => bail!("'{head}' is not a procedure"),

                None => bail!("'()' cannot be evaluated"),
            }
        } else {
            Ok(self)
        }
    }
}

impl fmt::Display for Item<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let width = f.width().unwrap_or(0);
        let indent = |f: &mut fmt::Formatter| {
            for _ in 0..width {
                write!(f, "   ")?;
            }
            Ok(())
        };
        indent(f)?;

        match self {
            Self::Num(num) => write!(f, "{num}"),
            Self::Proc(proc) => write!(f, "{proc}"),
            Self::Token(token) => write!(f, "{token}"),
            Self::Defined => write!(f, "<{}>", idents::DEFINE),
            Self::List(list) => {
                if let Some(List { head, tail }) = list.as_deref() {
                    writeln!(f, "(")?;

                    let (mut head, mut tail) = (head, tail);
                    loop {
                        writeln!(f, "{head:width$}", width = width + 1)?;
                        (head, tail) = match tail {
                            Self::List(Some(list)) => (&list.head, &list.tail),

                            Self::List(_) => break,

                            _ => {
                                writeln!(f, ".")?;
                                indent(f)?;
                                writeln!(f, "{tail:width$}", width = width + 1)?;
                                break;
                            }
                        };
                    }

                    indent(f)?;
                    write!(f, ")")
                } else {
                    write!(f, "()")
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
struct HeadsIter<'a, 'src>(Option<&'a List<'src>>);

impl<'a, 'src> Iterator for HeadsIter<'a, 'src> {
    type Item = &'a Item<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.map(|List { head, tail }| {
            if let Item::List(list) = tail {
                self.0 = list.as_deref();
            } else {
                self.0 = None;
            }
            head
        })
    }
}

#[derive(Clone, Debug)]
struct NumsIter<'a, 'src>(HeadsIter<'a, 'src>);

impl<'a, 'src> TryFrom<HeadsIter<'a, 'src>> for NumsIter<'a, 'src> {
    type Error = Error;

    fn try_from(heads_iter: HeadsIter<'a, 'src>) -> Result<Self> {
        ensure!(
            heads_iter.clone().all(|item| matches!(item, Item::Num(_))),
            "cannot operate on non-number"
        );
        Ok(Self(heads_iter))
    }
}

impl<'a, 'src> Iterator for NumsIter<'a, 'src> {
    type Item = f64;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|item| {
            if let &Item::Num(n) = item {
                n
            } else {
                panic!("NumsIter invariant violated")
            }
        })
    }
}

#[derive(Clone, Debug)]
pub struct List<'src> {
    head: Item<'src>,
    tail: Item<'src>,
}

#[derive(Clone, Debug)]
pub enum ProcScopeGeneric<'src, Scope> {
    Arith(Arith),
    Cmp(Cmp),
    Log,
    Exp,
    Rem,
    Compound {
        name: Option<&'src str>,
        params: Rc<[&'src str]>,
        scope: Scope,
        body: &'src [Syn<'src>],
    },
}

pub type Proc<'src> = ProcScopeGeneric<'src, BorrowedScope<'src>>;

pub type StoredProc<'src> = ProcScopeGeneric<'src, StoredScope<'src>>;

impl<'src> StoredProc<'src> {
    pub fn into_borrowed(self, id: ScopeId) -> Result<Proc<'src>> {
        Ok(match self {
            Self::Arith(arith) => Proc::Arith(arith),
            Self::Cmp(cmp) => Proc::Cmp(cmp),
            Self::Log => Proc::Log,
            Self::Exp => Proc::Exp,
            Self::Rem => Proc::Rem,
            Self::Compound {
                name,
                params,
                scope,
                body,
            } => Proc::Compound {
                name,
                params,
                scope: scope.into_borrowed(id)?,
                body,
            },
        })
    }
}

impl<'src> Proc<'src> {
    pub fn into_stored(self, id: ScopeId) -> StoredProc<'src> {
        match self {
            Self::Arith(arith) => StoredProc::Arith(arith),
            Self::Cmp(cmp) => StoredProc::Cmp(cmp),
            Self::Log => StoredProc::Log,
            Self::Exp => StoredProc::Exp,
            Self::Rem => StoredProc::Rem,
            Self::Compound {
                name,
                params,
                scope,
                body,
            } => StoredProc::Compound {
                name,
                params,
                scope: scope.into_stored(id),
                body,
            },
        }
    }

    fn apply(&self, args: HeadsIter<'_, 'src>) -> Result<Item<'src>> {
        match self {
            Self::Arith(arith) => arith.apply(args.try_into()?),

            Self::Cmp(cmp) => cmp.apply(args.try_into()?),

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

            Self::Compound {
                name,
                params,
                scope,
                body,
            } => {
                let debug = globals::debug();

                if debug {
                    eprint!("{}(", name.unwrap_or(idents::LAMBDA));
                }
                let mut args = args;
                let scope = Scope::new_local(scope);
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
            Self::Arith(arith) => write!(f, "{arith}"),
            Self::Cmp(cmp) => write!(f, "{cmp}"),
            Self::Log => write!(f, "<{}>", idents::LOG),
            Self::Exp => write!(f, "<{}>", idents::EXP),
            Self::Rem => write!(f, "<{}>", idents::REM),
            Self::Compound { name: None, .. } => write!(f, "<{}>", idents::LAMBDA),
            Self::Compound {
                name: Some(name), ..
            } => write!(f, "<proc '{name}'>"),
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
        // guard against 0 args
        if args.clone().next().is_none() {
            return match self {
                Self::Add => Ok(Item::Num(0.0)),
                Self::Mul => Ok(Item::Num(1.0)),
                Self::Sub | Self::Div => {
                    bail!("Wrong number of arguments for {self}")
                }
            };
        }
        // guard against div by zero
        ensure!(
            !(self == Self::Div && args.clone().any(|item| item == 0.0)),
            "Cannot divide by zero"
        );
        // guard against 1 arg
        if args.clone().skip(1).next().is_none() {
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
                Cmp::Eq => idents::EQ,
                Cmp::Gt => idents::GT,
                Cmp::Ge => idents::GE,
                Cmp::Lt => idents::LT,
                Cmp::Le => idents::LE,
            }
        )
    }
}
