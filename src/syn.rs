use crate::{
    error::Error,
    idents,
    item::{Arith, Item, Proc},
    scope::Scope,
};
use std::{fmt, rc::Rc};

#[derive(Clone, Debug)]
pub enum Syn<'src> {
    Num(f64),
    Define,
    Ident(&'src str),
    Group(Vec<Syn<'src>>),
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Defs {
    Allowed,
    NotAllowed,
}

impl<'src> Syn<'src> {
    pub fn eval(
        &'src self,
        scope: Rc<Scope<'src>>,
        defs: Defs,
    ) -> Result<(Item<'src>, Rc<Scope<'src>>), Error> {
        match self {
            &Self::Num(n) => Ok((Item::Num(n), scope)),

            Self::Define => Err(Error::Other(format!("Unexpected '{}'", idents::DEFINE))),

            &Self::Ident(ident) => {
                if let Some(lookup) = scope.lookup(ident) {
                    Ok((lookup, scope))
                } else {
                    let arith = match ident {
                        idents::ADD => Arith::Add,
                        idents::SUB => Arith::Sub,
                        idents::MUL => Arith::Mul,
                        idents::DIV => Arith::Div,
                        _ => {
                            return Err(Error::Other(format!(
                                "Failed to find definition for '{ident}'"
                            )));
                        }
                    };
                    Ok((Item::Proc(Proc::Arith(arith)), scope))
                }
            }

            Self::Group(group) => match group.as_slice() {
                [] => Ok((Item::nil(), scope)),

                [Self::Define, ..] if defs == Defs::NotAllowed => {
                    Err(Error::Other(format!("Ill-placed '{}'", idents::DEFINE)))
                }

                [Self::Define, Self::Ident(ident), def] => {
                    let (item, scope) = def.eval(scope, Defs::NotAllowed)?;
                    Ok((Item::Defined, scope.add(ident, item)))
                }

                [Self::Define, Self::Group(signature), body] => match signature.as_slice() {
                    [Self::Ident(ident), params @ ..] => {
                        let params = params
                            .iter()
                            .map(|param| {
                                if let &Syn::Ident(ident) = param {
                                    Ok(ident)
                                } else {
                                    Err(Error::Other(format!(
                                        "'{param}' is not a valid parameter name"
                                    )))
                                }
                            })
                            .collect::<Result<_, _>>()?;

                        //TODO
                        Ok((
                            Item::Defined,
                            scope.clone().add(
                                ident,
                                Item::Proc(Proc::User {
                                    name: ident,
                                    params,
                                    scope,
                                    body,
                                }),
                            ),
                        ))
                    }

                    _ => Err(Error::Other("Malformed signature".into())),
                },

                [Self::Define, ..] => Err(Error::Other(format!("Malformed '{}'", idents::DEFINE))),

                _ => group
                    .iter()
                    .try_rfold((Item::nil(), scope), |(tail, scope), syn| {
                        syn.eval(scope, Defs::NotAllowed)
                            .map(|(head, scope)| (Item::cons(head, tail), scope))
                    })
                    .and_then(|(item, scope)| Ok((item.apply()?, scope))),
            },
        }
    }
}

impl fmt::Display for Syn<'_> {
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
            Self::Define => write!(f, "{}", idents::DEFINE),
            Self::Ident(ident) => ident.fmt(f),
            Self::Group(items) if items.is_empty() => write!(f, "()"),
            Self::Group(items) => {
                writeln!(f, "(")?;
                for item in items {
                    writeln!(f, "{item:width$}", width = width + 1)?;
                }
                indent(f)?;
                write!(f, ")")
            }
        }
    }
}
