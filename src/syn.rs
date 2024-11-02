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
    Ident(&'src str),
    Reserved(Reserved),
    Group(Vec<Syn<'src>>),
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Reserved {
    Define,
    Cond,
    Else,
}

impl fmt::Display for Reserved {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Reserved::Define => write!(f, "{}", idents::DEFINE),
            Reserved::Cond => write!(f, "{}", idents::COND),
            Reserved::Else => write!(f, "{}", idents::ELSE),
        }
    }
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

            Self::Reserved(reserved) => Err(Error::Other(format!("Unexpected '{reserved}'"))),

            Self::Group(group) => match group.as_slice() {
                [] => Ok((Item::nil(), scope)),

                [Self::Reserved(Reserved::Define), ..] if defs == Defs::NotAllowed => {
                    Err(Error::Other(format!("Ill-placed '{}'", idents::DEFINE)))
                }

                [Self::Reserved(Reserved::Define), Self::Ident(ident), def] => {
                    let (item, scope) = def.eval(scope, Defs::NotAllowed)?;
                    Ok((Item::Defined, scope.add(ident, item)))
                }

                [Self::Reserved(Reserved::Define), Self::Group(signature), body @ ..]
                    if !body.is_empty() =>
                {
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

                [Self::Reserved(Reserved::Define), ..] => {
                    Err(Error::Other(format!("Malformed '{}'", idents::DEFINE)))
                }

                [Self::Reserved(Reserved::Cond), conds @ ..] if !conds.is_empty() => {
                    let mut scope = scope;
                    for (i, cond) in conds.iter().enumerate() {
                        if let Self::Group(group) = cond {
                            match group.as_slice() {
                                [] => {
                                    return Err(Error::Other(format!(
                                        "Malformed '{}'",
                                        idents::COND
                                    )));
                                }

                                [Self::Reserved(Reserved::Else)] => {
                                    return Err(Error::Other(format!(
                                        "Malformed '{}'",
                                        idents::ELSE
                                    )))
                                }

                                [Self::Reserved(Reserved::Else), syn, syns @ ..] => {
                                    return if i == conds.len() - 1 {
                                        syns.iter()
                                            .try_fold(
                                                syn.eval(scope, Defs::NotAllowed)?,
                                                |(_, scope), syn| syn.eval(scope, Defs::NotAllowed),
                                            )
                                            .and_then(|(item, scope)| Ok((item.apply()?, scope)))
                                    } else {
                                        Err(Error::Other(format!("Ill-placed '{}'", idents::ELSE)))
                                    };
                                }

                                [cond, syns @ ..] => {
                                    scope = {
                                        let (cond, scope) = cond.eval(scope, Defs::NotAllowed)?;
                                        let cond = cond.apply()?;

                                        if !matches!(cond, Item::Token(Token::False)) {
                                            return syns
                                                .iter()
                                                .try_fold((cond, scope), |(_, scope), syn| {
                                                    syn.eval(scope, Defs::NotAllowed)
                                                })
                                                .and_then(|(item, scope)| {
                                                    Ok((item.apply()?, scope))
                                                });
                                        }
                                        scope
                                    }
                                }
                            }
                        } else {
                            return Err(Error::Other(format!("Malformed '{}'", idents::COND)));
                        }
                    }

                    Err(Error::Other(format!(
                        "All conditions are '{}'",
                        idents::FALSE
                    )))
                }

                [Self::Reserved(Reserved::Cond), ..] => {
                    Err(Error::Other(format!("Malformed '{}'", idents::COND)))
                }

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
            Self::Ident(ident) => write!(f, "{ident}"),
            Self::Reserved(reserved) => write!(f, "{reserved}"),
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
