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

    fn args(&self) -> ProcArgs<'_, 'src> {
        ProcArgs(if let Self::List(list) = self {
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
                    ..
                }) => proc.apply(self.args()),

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
                f.write_str("   ")?;
            }
            Ok(())
        };
        indent(f)?;

        match self {
            Self::Num(num) => writeln!(f, "{num}"),
            Self::Proc(Proc::Arith(arith)) => writeln!(f, "<{arith}>"),
            Self::Proc(_) => writeln!(f, "<function>"),
            Self::Defined => writeln!(f, "<{}>", idents::DEFINE),
            Self::List(list) => {
                if let Some(List { head, tail }) = list.as_deref() {
                    writeln!(f, "(")?;

                    let (mut head, mut tail) = (head, tail);
                    loop {
                        write!(f, "{head:width$}", width = width + 1)?;
                        (head, tail) = match tail {
                            Self::List(Some(list)) => (&list.head, &list.tail),

                            Self::List(_) => break,

                            _ => {
                                writeln!(f, ".")?;
                                indent(f)?;
                                write!(f, "{tail:width$}", width = width + 1)?;
                                break;
                            }
                        };
                    }

                    indent(f)?;
                    writeln!(f, ")")
                } else {
                    writeln!(f, "()")
                }
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct ProcArgs<'a, 'src>(Option<&'a List<'src>>);

impl<'a, 'src> Iterator for ProcArgs<'a, 'src> {
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

impl<'src> Proc<'src> {
    fn apply(&self, args: ProcArgs<'_, 'src>) -> Result<Item<'src>, Error> {
        match self {
            Proc::Arith(arith) => {
                if matches!(arith, Arith::Div) && { args }
                    .any(|item| matches!(item, Item::Num(0.0)))
                {
                    return Err(Error::Other("Can't divide by zero!".into()));
                };

                if !{ args }.all(|item| matches!(item, Item::Num(_))) {
                    return Err(Error::Other(format!(
                        "<{arith}> can't operate on non-number"
                    )));
                }

                let reduced = args
                    .flat_map(|item| {
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
                    });

                if let Some(reduced) = reduced {
                    if matches!(arith, Arith::Div) {
                        Ok(Item::Num(1.0 / reduced))
                    } else {
                        Ok(Item::Num(reduced))
                    }
                } else {
                    match arith {
                        Arith::Add => Ok(Item::Num(0.0)),
                        Arith::Mul => Ok(Item::Num(1.0)),
                        Arith::Sub | Arith::Div => Err(Error::Other(format!(
                            "Wront number of arguments for <{arith}>"
                        ))),
                    }
                }
            }

            Proc::User {
                params,
                scope,
                body,
            } => {
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
                    Err(Error::Other(format!(
                        "Too many params, expected {}",
                        params.len()
                    )))
                } else {
                    body.eval(scope, Defs::Allowed).map(|(item, _)| item)
                }
            }
        }
    }
}

impl fmt::Display for Arith {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Arith::Add => idents::ADD,
            Arith::Sub => idents::SUB,
            Arith::Mul => idents::MUL,
            Arith::Div => idents::DIV,
        })
    }
}
