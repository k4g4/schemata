use crate::{
    idents,
    scope::Scope,
    syn::{Defs, Syn},
};
use anyhow::{bail, ensure, Result};
use std::{
    fmt,
    ops::{Add, Div, Mul, Sub},
    rc::Rc,
};

#[derive(Clone, Debug)]
pub enum Item<'src> {
    Num(f64),
    List(Option<Rc<List<'src>>>),
    Proc(Proc<'src>),
    Token(Token),
    Defined,
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
pub struct List<'src> {
    head: Item<'src>,
    tail: Item<'src>,
}

#[derive(Clone, Debug)]
pub enum Proc<'src> {
    Arith(Arith),
    Cmp(Cmp),
    User {
        name: Option<&'src str>,
        params: Rc<[&'src str]>,
        scope: Rc<Scope<'src>>,
        body: &'src [Syn<'src>],
    },
}

#[derive(Copy, Clone, Debug)]
pub enum Arith {
    Add,
    Sub,
    Mul,
    Div,
}

impl fmt::Display for Arith {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Arith::Add => idents::ADD,
                Arith::Sub => idents::SUB,
                Arith::Mul => idents::MUL,
                Arith::Div => idents::DIV,
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

impl fmt::Display for Cmp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
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

impl<'src> Proc<'src> {
    fn apply(&self, args: HeadsIter<'_, 'src>) -> Result<Item<'src>> {
        match self {
            Self::Arith(arith) => {
                // guard against invalid args
                ensure!(
                    args.clone().all(|item| matches!(item, Item::Num(_))),
                    "{self} cannot operate on non-number"
                );
                // guard against 0 args
                if args.clone().next().is_none() {
                    return match arith {
                        Arith::Add => Ok(Item::Num(0.0)),
                        Arith::Mul => Ok(Item::Num(1.0)),
                        Arith::Sub | Arith::Div => bail!("Wront number of arguments for <{arith}>"),
                    };
                }
                // guard against div by zero
                ensure!(
                    !(matches!(arith, Arith::Div)
                        && args.clone().any(|item| matches!(item, Item::Num(0.0)))),
                    "Cannot divide by zero"
                );

                // guard against 1 arg
                if args.clone().skip(1).next().is_none() {
                    return match (arith, args.clone().next()) {
                        (Arith::Add | Arith::Mul, Some(&Item::Num(num))) => Ok(Item::Num(num)),
                        (Arith::Sub, Some(Item::Num(num))) => Ok(Item::Num(-num)),
                        (Arith::Div, Some(Item::Num(num))) => Ok(Item::Num(1.0 / num)),
                        _ => bail!("unreachable arithmetic"),
                    };
                }

                args.flat_map(|item| {
                    if let &Item::Num(n) = item {
                        Some(n)
                    } else {
                        None
                    }
                })
                .reduce(match arith {
                    Arith::Add => <_ as Add>::add,
                    Arith::Sub => <_ as Sub>::sub,
                    Arith::Mul => <_ as Mul>::mul,
                    Arith::Div => <_ as Div>::div,
                })
                .map(Item::Num)
                .ok_or_else(|| anyhow::anyhow!("unreachable arithmetic"))
            }

            Self::Cmp(cmp) => {
                // guard against invalid args
                ensure!(
                    args.clone().all(|item| matches!(item, Item::Num(_))),
                    "{self} cannot operate on non-number"
                );

                let cmp = match cmp {
                    Cmp::Eq => |lhs, rhs| lhs == rhs,
                    Cmp::Gt => |lhs, rhs| lhs > rhs,
                    Cmp::Ge => |lhs, rhs| lhs >= rhs,
                    Cmp::Lt => |lhs, rhs| lhs < rhs,
                    Cmp::Le => |lhs, rhs| lhs <= rhs,
                };

                let (token, _) = args
                    .flat_map(|item| {
                        if let &Item::Num(n) = item {
                            Some(n)
                        } else {
                            None
                        }
                    })
                    .fold((true, None), |(still_true, prev), num| {
                        if still_true {
                            prev.map_or((true, Some(num)), |prev| (cmp(prev, num), Some(num)))
                        } else {
                            (false, None)
                        }
                    });

                Ok(Item::Token(if token { Token::True } else { Token::False }))
            }

            Self::User {
                name,
                params,
                scope,
                body,
            } => {
                let scope = {
                    let mut args = args;
                    let scope = params.iter().enumerate().try_fold(
                        Scope::new_local(scope.clone()),
                        |scope, (i, param)| {
                            if let Some(item) = args.next() {
                                Ok(scope.add(param, item.clone()))
                            } else {
                                bail!("Expected {} parameter(s), got {i}", params.len());
                            }
                        },
                    )?;
                    ensure!(
                        args.next().is_none(),
                        "Too many parameters for {self}, expected {}",
                        params.len()
                    );

                    if let Some(name) = name {
                        scope.add(&name, Item::Proc(self.clone()))
                    } else {
                        scope
                    }
                };

                body.iter()
                    .try_fold((Item::nil(), scope), |(_, scope), syn| {
                        syn.eval(scope, Defs::Allowed)
                            .and_then(|(item, scope)| Ok((item.apply()?, scope)))
                    })
                    .and_then(|(item, _)| {
                        if let Item::Defined = item {
                            bail!("Ill-placed '{}'", idents::DEFINE);
                        } else {
                            Ok(item)
                        }
                    })
            }
        }
    }
}

impl fmt::Display for Proc<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Arith(arith) => write!(f, "<({arith})>"),
            Self::Cmp(cmp) => write!(f, "<({cmp})'>"),
            Self::User { name: None, .. } => write!(f, "<lambda>"),
            Self::User {
                name: Some(name), ..
            } => write!(f, "<proc '{name}'>"),
        }
    }
}
