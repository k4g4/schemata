use crate::{
    error::Error,
    idents::*,
    item::{Builtin, Item},
    list::List,
    scope::Scope,
};
use std::fmt;

#[derive(Clone, Debug)]
pub enum Syn<'src> {
    Num(f64),
    Ident(&'src str),
    List(Vec<Syn<'src>>),
}

impl<'src> Syn<'src> {
    pub fn interpret(&self, scope: &Scope<'src>) -> Result<Item<'src>, Error> {
        match self {
            &Syn::Num(n) => Ok(Item::Num(n)),
            Syn::Ident(IDENT_DEFINE) => Ok(Item::Define),
            &Syn::Ident(ident) => {
                if let Some(lookup) = scope.lookup(ident) {
                    Ok(lookup)
                } else {
                    let builtin = match ident {
                        IDENT_ADD => Builtin::Add,
                        IDENT_SUB => Builtin::Sub,
                        IDENT_MUL => Builtin::Mul,
                        IDENT_DIV => Builtin::Div,
                        _ => {
                            return Err(Error::Other(format!("Failed to find '{ident}' in scope")));
                        }
                    };
                    Ok(Item::Builtin(builtin))
                }
            }
            Syn::List(list) => {
                if let Some((head, tail)) = list.split_first() {
                    match head.interpret(scope)? {
                        Item::Num(_) => todo!(),
                        Item::Func(func) => todo!(),
                        Item::Builtin(builtin) => todo!(),
                        Item::Define => todo!(),
                        Item::List(list) => todo!(),
                    }
                    todo!()
                } else {
                    Ok(Item::List(List::nil()))
                }
            }
        }
    }
}

impl fmt::Display for Syn<'_> {
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
