use crate::{error::Error, scope::Scope};
use smartstring::alias::String;
use std::{
    fmt, mem,
    ops::{Add, Div, Mul, Sub},
    rc::Rc,
};

#[derive(Clone, Debug)]
pub enum Syn {
    Num(f64),
    Ident(String),
    List(Vec<Syn>),
}

impl Syn {
    pub fn eval(self, _scope: Rc<Scope>) -> Result<(Item, Rc<Scope>), Error> {
        todo!()
    }
}

impl fmt::Display for Syn {
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
            Self::Ident(ident) => writeln!(f, "{ident}"),
            Self::List(items) if items.is_empty() => f.write_str("()\n"),
            Self::List(items) => {
                f.write_str("(\n")?;
                for item in items {
                    write!(f, "{item:width$}", width = width + 1)?;
                }
                indent(f)?;
                f.write_str(")\n")
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Item {
    Num(f64),
    Func(Func),
    Builtin(Builtin),
    List(Vec<Item>),
}

impl Item {
    fn eval(self, scope: &mut Scope) -> Result<Self, Error> {
        let Self::List(mut list) = self else {
            return Ok(self);
        };
        let Some((head, tail)) = list.split_first_mut() else {
            return Ok(Default::default());
        };
        *head = mem::take(head).eval(scope)?;
        match head {
            Self::Define => match tail {
                [] => Err(Error::Other("Empty define".into())),
                [Self::Ident(ident), value] => {
                    let value = mem::take(value).eval(scope)?;
                    todo!()
                }
                [Self::List(signature), _, ..] => todo!(),
                _ => Err(Error::Other("Invalid define".into())),
            },
            _ => {
                for item in &mut *tail {
                    *item = mem::take(item).eval(scope)?;
                }
                if let Self::Builtin(builtin) = head {
                    builtin.eval(tail)
                } else {
                    Ok(Self::List(list))
                }
            }
        }
    }
}

impl fmt::Display for Item {
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
            Self::Func(_) => Ok(()),
            Self::Builtin(builtin) => writeln!(f, "{builtin}"),
            Self::List(items) if items.is_empty() => f.write_str("()\n"),
            Self::List(items) => {
                f.write_str("(\n")?;
                for item in items {
                    write!(f, "{item:width$}", width = width + 1)?;
                }
                indent(f)?;
                f.write_str(")\n")
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
}

impl Builtin {
    fn eval(self, mut tail: impl Iterator<Item = Item>) -> Result<Item, Error> {
        if matches!(self, Self::Add | Self::Sub | Self::Mul | Self::Div) {
            if matches!(self, Self::Div) && tail.any(|item| matches!(item, Item::Num(0.0))) {
                return Err(Error::Other("Can't divide by zero!".into()));
            };
            if !tail.all(|item| matches!(item, Item::Num(_))) {
                return Err(Error::Other(format!("{self} can't operate on non-number")));
            }
            let head = tail.next();
            if let Some(Item::Num(head)) = head {
                let op = match self {
                    Self::Add => <_ as Add>::add,
                    Self::Sub => <_ as Sub>::sub,
                    Self::Mul => <_ as Mul>::mul,
                    Self::Div => <_ as Div>::div,
                };
                let get_num = |item| {
                    if let Item::Num(n) = item {
                        Some(n)
                    } else {
                        None
                    }
                };
                Ok(Item::Num(tail.flat_map(get_num).fold(head, op)))
            } else if matches!(self, Self::Add | Self::Mul) {
                Ok(Item::Num(0.0))
            } else {
                Err(Error::Other(format!(
                    "Wrong number of arguments for {self}"
                )))
            }
        } else {
            unreachable!("Unsupported builtin: {self}")
        }
    }
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Builtin::Add => write!(f, "+"),
            Builtin::Sub => write!(f, "-"),
            Builtin::Mul => write!(f, "*"),
            Builtin::Div => write!(f, "/"),
        }
    }
}

#[derive(Clone, Debug)]
struct Func {
    params: Vec<String>,
    scope: Rc<Scope>,
    body: Vec<Syn>,
}
