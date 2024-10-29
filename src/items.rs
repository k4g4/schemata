use crate::error::Error;
use std::{
    fmt, mem,
    ops::{Add, Div, Mul, Sub},
};

#[derive(Copy, Clone, Debug)]
pub enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    Define,
}

impl Builtin {
    pub fn eval(self, cdr: &[Item]) -> Result<Item, Error> {
        if matches!(self, Self::Add | Self::Sub | Self::Mul | Self::Div) {
            if matches!(self, Self::Div) && cdr.iter().any(|item| matches!(item, Item::Num(0.0))) {
                return Err(Error::Other("Can't divide by zero!".into()));
            };
            if !cdr.iter().all(|item| matches!(item, Item::Num(_))) {
                return Err(Error::Other(format!("{self} can't operate on non-number")));
            }
            if let Some((&Item::Num(car), cdr)) = cdr.split_first() {
                let op = match self {
                    Self::Add => <_ as Add>::add,
                    Self::Sub => <_ as Sub>::sub,
                    Self::Mul => <_ as Mul>::mul,
                    Self::Div => <_ as Div>::div,
                    _ => unreachable!(),
                };
                let get_num = |item| {
                    if let &Item::Num(n) = item {
                        Some(n)
                    } else {
                        None
                    }
                };
                Ok(Item::Num(cdr.iter().flat_map(get_num).fold(car, op)))
            } else if matches!(self, Self::Add | Self::Mul) {
                Ok(Item::Num(0.0))
            } else {
                Err(Error::Other(format!(
                    "Wrong number of arguments for {self}"
                )))
            }
        } else if matches!(self, Self::Define) {
            todo!()
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
            Builtin::Define => write!(f, "define"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Item {
    Num(f64),
    Builtin(Builtin),
    List(Vec<Item>),
}

impl Item {
    pub fn eval(self) -> Result<Self, Error> {
        let Self::List(mut list) = self else {
            return Ok(self);
        };
        let Some((car, cdr)) = list.split_first_mut() else {
            return Ok(Default::default());
        };
        *car = mem::take(car).eval()?;
        for item in &mut *cdr {
            *item = mem::take(item).eval()?;
        }
        match car {
            Self::Builtin(builtin) => builtin.eval(cdr),
            _ => Ok(Self::List(list)),
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
            Self::Num(num) => writeln!(f, "{num}"),
            Self::Builtin(builtin) => writeln!(f, "{builtin}"),
            Self::List(items) if items.is_empty() => {
                writeln!(f, "()")
            }
            Self::List(items) => {
                writeln!(f, "(")?;
                for item in items {
                    write!(f, "{item:width$}", width = width + 1)?;
                }
                indent(f)?;
                writeln!(f, ")")
            }
        }
    }
}
