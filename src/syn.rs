use crate::{
    error::Error,
    idents,
    item::{Arith, Cmp, Item, Proc, Token},
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
                    let builtin = match ident {
                        idents::ADD => Item::Proc(Proc::Arith(Arith::Add)),
                        idents::SUB => Item::Proc(Proc::Arith(Arith::Sub)),
                        idents::MUL => Item::Proc(Proc::Arith(Arith::Mul)),
                        idents::DIV => Item::Proc(Proc::Arith(Arith::Div)),

                        idents::EQ => Item::Proc(Proc::Cmp(Cmp::Eq)),
                        idents::GT => Item::Proc(Proc::Cmp(Cmp::Gt)),
                        idents::GE => Item::Proc(Proc::Cmp(Cmp::Ge)),
                        idents::LT => Item::Proc(Proc::Cmp(Cmp::Lt)),
                        idents::LE => Item::Proc(Proc::Cmp(Cmp::Le)),

                        idents::TRUE => Item::Token(Token::True),
                        idents::FALSE => Item::Token(Token::False),

                        _ => {
                            return Err(Error::Other(format!(
                                "Failed to find definition for '{ident}'"
                            )));
                        }
                    };

                    Ok((builtin, scope))
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

                [Self::Define, Self::Group(signature), body @ ..] if !body.is_empty() => {
                    match signature.as_slice() {
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

                            Ok((
                                Item::Defined,
                                scope.clone().add(
                                    ident,
                                    Item::Proc(Proc::User {
                                        name: Some(ident),
                                        params,
                                        scope,
                                        body,
                                    }),
                                ),
                            ))
                        }

                        _ => Err(Error::Other("Malformed signature".into())),
                    }
                }

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
            Self::Num(num) => write!(f, "{num}"),
            Self::Define => write!(f, "{}", idents::DEFINE),
            Self::Ident(ident) => write!(f, "{ident}"),
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
