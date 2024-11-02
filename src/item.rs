use crate::{
    error::Error,
    idents,
    scope::Scope,
    syn::{Defs, Syn},
};
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
    Defined,
}

impl<'src> Item<'src> {
    pub fn nil() -> Self {
        Self::List(None)
    }

    pub fn cons(head: Item<'src>, tail: Item<'src>) -> Self {
        Self::List(Some(Rc::new(List { head, tail })))
    }

    fn iter(&self) -> HeadsIter<'_, 'src> {
        HeadsIter(if let Self::List(list) = self {
            list.as_deref()
        } else {
            None
        })
    }

    pub fn apply(self) -> Result<Self, Error> {
        if let Self::List(list) = &self {
            match list.as_deref() {
                Some(List {
                    head: Self::Proc(proc),
                    tail,
                }) => proc.apply(tail.iter()),

                Some(List { head, .. }) => Err(Error::Other(format!("{head} is not a procedure"))),

                None => Err(Error::Other("Empty list cannot be evaluated".into())),
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
            Self::Num(num) => num.fmt(f),
            Self::Proc(proc) => proc.fmt(f),
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
    User {
        name: &'src str,
        params: Rc<[&'src str]>,
        scope: Rc<Scope<'src>>,
        body: &'src Syn<'src>,
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
        match self {
            Arith::Add => idents::ADD,
            Arith::Sub => idents::SUB,
            Arith::Mul => idents::MUL,
            Arith::Div => idents::DIV,
        }
        .fmt(f)
    }
}

impl<'src> Proc<'src> {
    fn apply(&self, args: HeadsIter<'_, 'src>) -> Result<Item<'src>, Error> {
        match self {
            Proc::Arith(arith) => {
                // guard against invalid args
                if !args.clone().all(|item| matches!(item, Item::Num(_))) {
                    return Err(Error::Other(format!(
                        "<{arith}> cannot operate on non-number"
                    )));
                }
                // guard against 0 args
                if args.clone().next().is_none() {
                    return match arith {
                        Arith::Add => Ok(Item::Num(0.0)),
                        Arith::Mul => Ok(Item::Num(1.0)),
                        Arith::Sub | Arith::Div => Err(Error::Other(format!(
                            "Wront number of arguments for <{arith}>"
                        ))),
                    };
                }
                // guard against div by zero
                if matches!(arith, Arith::Div)
                    && args.clone().any(|item| matches!(item, Item::Num(0.0)))
                {
                    return Err(Error::Other("Cannot divide by zero".into()));
                };
                // guard against 1 arg
                if args.clone().skip(1).next().is_none() {
                    return match (arith, args.clone().next()) {
                        (Arith::Add | Arith::Mul, Some(&Item::Num(num))) => Ok(Item::Num(num)),
                        (Arith::Sub, Some(Item::Num(num))) => Ok(Item::Num(-num)),
                        (Arith::Div, Some(Item::Num(num))) => Ok(Item::Num(1.0 / num)),
                        _ => Err(Error::Unexpected),
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
                .ok_or(Error::Unexpected)
            }

            Proc::User {
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
                                Err(Error::Other(format!(
                                    "Expected {} param(s), got {i}",
                                    params.len()
                                )))
                            }
                        },
                    )?;
                    if !args.next().is_none() {
                        return Err(Error::Other(format!(
                            "Too many params, expected {}",
                            params.len()
                        )));
                    }
                    scope.add(&name, Item::Proc(self.clone()))
                };

                body.eval(scope, Defs::Allowed)
                    .and_then(|(item, _)| item.apply())
            }
        }
    }
}

impl fmt::Display for Proc<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Proc::Arith(arith) => arith.fmt(f),
            Proc::User { name, .. } => write!(f, "<proc '{name}'>"),
        }
    }
}