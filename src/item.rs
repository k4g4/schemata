use crate::{
    error::Error,
    idents::*,
    list::{List, Node},
    scope::Scope,
    syn::Syn,
};
use std::{
    fmt,
    ops::{Add, Div, Mul, Sub},
    rc::Rc,
};

#[derive(Clone, Debug)]
pub enum Item<'src> {
    Num(f64),
    Func(Func<'src>),
    Builtin(Builtin),
    Define,
    List(List<'src>),
}

impl<'src> Item<'src> {
    pub fn eval(self, scope: Rc<Scope<'src>>) -> Result<(Self, Rc<Scope>), Error> {
        if let Self::List(list) = &self {
            if let Some(Node { head, tail }) = list.as_ref() {
                match head {
                    Self::Func(func) => func.eval(tail).map(|item| (item, scope)),
                    Self::Builtin(builtin) => builtin.eval(&tail, scope),
                    _ => Ok((self, scope)),
                }
            } else {
                Ok((self, scope))
            }
        } else {
            Ok((self, scope))
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
            Self::Func(_) => writeln!(f, "<function>"),
            Self::Builtin(builtin) => writeln!(f, "{builtin}"),
            Self::Define => writeln!(f, "{IDENT_DEFINE}"),
            Self::List(list) => {
                if list.is_empty() {
                    writeln!(f, "()")
                } else {
                    writeln!(f, "(")?;
                    for item in list.iter() {
                        write!(f, "{item:width$}", width = width + 1)?;
                    }
                    indent(f)?;
                    writeln!(f, ")")
                }
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
}

impl Builtin {
    fn eval<'src>(
        &self,
        tail: &List,
        scope: Rc<Scope<'src>>,
    ) -> Result<(Item<'src>, Rc<Scope<'src>>), Error> {
        if matches!(self, Self::Div) && tail.iter().any(|item| matches!(item, Item::Num(0.0))) {
            return Err(Error::Other("Can't divide by zero!".into()));
        };

        if matches!(self, Self::Add | Self::Sub | Self::Mul | Self::Div) {
            if !tail.iter().all(|item| matches!(item, Item::Num(_))) {
                return Err(Error::Other(format!("{self} can't operate on non-number")));
            }

            let reduced = tail
                .iter()
                .flat_map(|item| {
                    if let &Item::Num(n) = item {
                        Some(n)
                    } else {
                        None
                    }
                })
                .reduce(match self {
                    Self::Add => <_ as Add>::add,
                    Self::Sub => <_ as Sub>::sub,
                    Self::Mul => <_ as Mul>::mul,
                    Self::Div => <_ as Div>::div,
                });

            if let Some(reduced) = reduced {
                if matches!(self, Self::Div) {
                    Ok((Item::Num(1.0 / reduced), scope))
                } else {
                    Ok((Item::Num(reduced), scope))
                }
            } else {
                match self {
                    Self::Add => Ok((Item::Num(0.0), scope)),
                    Self::Mul => Ok((Item::Num(1.0), scope)),
                    Self::Sub | Self::Div => Err(Error::Other(format!(
                        "Wront number of arguments for {self}"
                    ))),
                }
            }
        } else {
            unreachable!("Unsupported builtin: {self}")
        }
    }
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Builtin::Add => IDENT_ADD,
            Builtin::Sub => IDENT_SUB,
            Builtin::Mul => IDENT_MUL,
            Builtin::Div => IDENT_DIV,
        })
    }
}

#[derive(Clone, Debug)]
pub struct Func<'src> {
    params: &'src [&'src str],
    scope: Rc<Scope<'src>>,
    body: &'src Syn<'src>,
}

impl<'src> Func<'src> {
    fn eval(&self, tail: &List<'src>) -> Result<Item<'src>, Error> {
        let mut iter = tail.iter();

        let scope = self.params.iter().enumerate().try_fold(
            Scope::new_local(self.scope.clone()),
            |scope, (i, param)| {
                if let Some(item) = iter.next() {
                    Ok(scope.add(param, item.clone()))
                } else {
                    Err(Error::Other(format!(
                        "Expected {} param(s), got {i}",
                        self.params.len()
                    )))
                }
            },
        )?;

        if !iter.next().is_none() {
            Err(Error::Other(format!(
                "Too many params, expected {}",
                self.params.len()
            )))
        } else {
            self.body.interpret(&scope)
        }
    }
}
