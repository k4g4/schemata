use rustc_hash::FxHashMap;

use crate::error::Error;
use std::{
    fmt, mem,
    ops::{Add, Div, Mul, Sub},
    ptr,
};

#[derive(Debug)]
pub struct Scope {
    parent: *const Scope,
    vars: FxHashMap<String, Item>,
    funcs: FxHashMap<String, Vec<Item>>,
}

impl Scope {
    fn lookup(&self, var: &str) -> Option<&Item> {
        // SAFETY: The returned item has lifetime 'self and the parent has lifetime 'self
        // since it belongs to this scope, so aliasing rules can't be violated.
        self.vars
            .get(var)
            .or_else(|| unsafe { self.parent.as_ref() }.and_then(|parent| parent.lookup(var)))
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            parent: ptr::null(),
            vars: Default::default(),
            funcs: Default::default(),
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
    pub fn eval(self, tail: &[Item]) -> Result<Item, Error> {
        if matches!(self, Self::Add | Self::Sub | Self::Mul | Self::Div) {
            if matches!(self, Self::Div) && tail.iter().any(|item| matches!(item, Item::Num(0.0))) {
                return Err(Error::Other("Can't divide by zero!".into()));
            };
            if !tail.iter().all(|item| matches!(item, Item::Num(_))) {
                return Err(Error::Other(format!("{self} can't operate on non-number")));
            }
            if let Some((&Item::Num(head), tail)) = tail.split_first() {
                let op = match self {
                    Self::Add => <_ as Add>::add,
                    Self::Sub => <_ as Sub>::sub,
                    Self::Mul => <_ as Mul>::mul,
                    Self::Div => <_ as Div>::div,
                };
                let get_num = |item| {
                    if let &Item::Num(n) = item {
                        Some(n)
                    } else {
                        None
                    }
                };
                Ok(Item::Num(tail.iter().flat_map(get_num).fold(head, op)))
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
pub enum Item {
    Define,
    Num(f64),
    Ident(String),
    Builtin(Builtin),
    List(Vec<Item>),
}

impl Item {
    pub fn eval(self, scope: &mut Scope) -> Result<Self, Error> {
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

impl Default for Item {
    fn default() -> Self {
        Self::List(Default::default())
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
            Self::Define => f.write_str("define\n"),
            Self::Num(num) => writeln!(f, "{num}"),
            Self::Ident(ident) => writeln!(f, "{ident}"),
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
