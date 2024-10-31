use crate::{error::Error, scope::Scope};
use smartstring::alias::String;
use std::{
    fmt,
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
    pub fn eval(self, scope: Rc<Scope>) -> Result<(Item, Rc<Scope>), Error> {
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
enum List {
    Nil,
    Cons { item: Item, next: Rc<List> },
}

struct ListIter<'a>(&'a List);

impl<'a> Iterator for ListIter<'a> {
    type Item = &'a Item;

    fn next(&mut self) -> Option<Self::Item> {
        if let List::Cons { item, next } = self.0 {
            *self = Self(next.as_ref());
            Some(item)
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
pub enum Item {
    Num(f64),
    Func(Func),
    Builtin(Builtin),
    List(Rc<List>),
}

impl Item {
    fn eval(self, scope: &mut Scope) -> Result<Self, Error> {
        if let Self::List(list) = &self {
            if let List::Cons { item, next } = list.as_ref() {
                match item {
                    Self::Func(func) => func.eval(&next),
                    Self::Builtin(builtin) => builtin.eval(&next),
                    _ => Ok(self),
                }
            } else {
                Ok(self)
            }
        } else {
            Ok(self)
        }
        // *head = mem::take(head).eval(scope)?;
        // match head {
        //     Self::Define => match tail {
        //         [] => Err(Error::Other("Empty define".into())),
        //         [Self::Ident(ident), value] => {
        //             let value = mem::take(value).eval(scope)?;
        //             todo!()
        //         }
        //         [Self::List(signature), _, ..] => todo!(),
        //         _ => Err(Error::Other("Invalid define".into())),
        //     },
        //     _ => {
        //         for item in &mut *tail {
        //             *item = mem::take(item).eval(scope)?;
        //         }
        //         if let Self::Builtin(builtin) = head {
        //             builtin.eval(tail)
        //         } else {
        //             Ok(Self::List(list))
        //         }
        //     }
        // }
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
            Self::List(list) => {
                if matches!(**list, List::Nil) {
                    writeln!(f, "()")
                } else {
                    f.write_str("(\n")?;
                    for item in ListIter(list) {
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
enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
}

impl Builtin {
    fn eval(&self, tail: &List) -> Result<Item, Error> {
        if matches!(self, Self::Div) && ListIter(tail).any(|item| matches!(item, Item::Num(0.0))) {
            return Err(Error::Other("Can't divide by zero!".into()));
        };

        if matches!(self, Self::Add | Self::Sub | Self::Mul | Self::Div) {
            if !ListIter(tail).all(|item| matches!(item, Item::Num(_))) {
                return Err(Error::Other(format!("{self} can't operate on non-number")));
            }

            let reduced = ListIter(tail)
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
                    Ok(Item::Num(1.0 / reduced))
                } else {
                    Ok(Item::Num(reduced))
                }
            } else {
                match self {
                    Self::Add => Ok(Item::Num(0.0)),
                    Self::Mul => Ok(Item::Num(1.0)),
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
    body: Syn,
}

impl Func {
    fn eval(&self, tail: &List) -> Result<Item, Error> {
        let mut iter = ListIter(tail);
        let mut scope = Scope::new(Some(self.scope.clone()));
        for (i, param) in self.params.iter().enumerate() {
            if let Some(item) = iter.next() {
                scope = scope.add(param.clone(), item.clone());
            } else {
                return Err(Error::Other(format!(
                    "Expected {} param(s), got {i}",
                    self.params.len()
                )));
            }
        }
        todo!()
    }
}
