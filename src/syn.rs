use crate::{
    idents,
    item::{Arith, Cmp, Item, Proc, Token},
    scope::Scope,
};
use anyhow::{bail, ensure, Result};
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
    If,
    And,
    Or,
}

impl fmt::Display for Reserved {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Define => write!(f, "{}", idents::DEFINE),
            Self::Cond => write!(f, "{}", idents::COND),
            Self::Else => write!(f, "{}", idents::ELSE),
            Self::If => write!(f, "{}", idents::IF),
            Self::And => write!(f, "{}", idents::AND),
            Self::Or => write!(f, "{}", idents::OR),
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
    ) -> Result<(Item<'src>, Rc<Scope<'src>>)> {
        match self {
            &Self::Num(n) => Ok((Item::Num(n), scope)),

            &Self::Ident(ident) => {
                if let Some(lookup) = scope.lookup(ident) {
                    Ok((lookup, scope))
                } else {
                    let builtin = match ident {
                        idents::TRUE => Item::Token(Token::True),
                        idents::FALSE => Item::Token(Token::False),

                        idents::ADD => Item::Proc(Proc::Arith(Arith::Add)),
                        idents::SUB => Item::Proc(Proc::Arith(Arith::Sub)),
                        idents::MUL => Item::Proc(Proc::Arith(Arith::Mul)),
                        idents::DIV => Item::Proc(Proc::Arith(Arith::Div)),

                        idents::EQ => Item::Proc(Proc::Cmp(Cmp::Eq)),
                        idents::GT => Item::Proc(Proc::Cmp(Cmp::Gt)),
                        idents::GE => Item::Proc(Proc::Cmp(Cmp::Ge)),
                        idents::LT => Item::Proc(Proc::Cmp(Cmp::Lt)),
                        idents::LE => Item::Proc(Proc::Cmp(Cmp::Le)),

                        _ => {
                            bail!("Failed to find definition for '{ident}'");
                        }
                    };

                    Ok((builtin, scope))
                }
            }

            Self::Reserved(reserved) => bail!("Unexpected '{reserved}'"),

            Self::Group(group) => match group.as_slice() {
                [] => Ok((Item::nil(), scope)),

                [Self::Reserved(Reserved::Define), ..] if defs == Defs::NotAllowed => {
                    bail!("Ill-placed '{}'", idents::DEFINE);
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
                                        bail!("'{param}' is not a valid parameter name");
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

                        _ => bail!("Malformed signature"),
                    }
                }

                [Self::Reserved(Reserved::Define), ..] => {
                    bail!("Malformed '{}'", idents::DEFINE)
                }

                [Self::Reserved(Reserved::Cond), conds @ ..] if !conds.is_empty() => {
                    let mut scope = scope;
                    for (i, cond) in conds.iter().enumerate() {
                        if let Self::Group(group) = cond {
                            match group.as_slice() {
                                [] => bail!("Malformed '{}'", idents::COND),

                                [Self::Reserved(Reserved::Else)] => {
                                    bail!("Malformed '{}'", idents::ELSE);
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
                                        bail!("Ill-placed '{}'", idents::ELSE);
                                    };
                                }

                                [cond, syns @ ..] => {
                                    scope = {
                                        let (cond, scope) = cond.eval(scope, Defs::NotAllowed)?;
                                        let cond = cond.apply()?;

                                        if cond.is_truthy() {
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
                            bail!("Malformed '{}'", idents::COND);
                        }
                    }

                    bail!("All conditions are '{}'", idents::FALSE);
                }

                [Self::Reserved(Reserved::Cond), ..] => bail!("Malformed '{}'", idents::COND),

                [Self::Reserved(Reserved::If), syns @ ..] => {
                    ensure!(matches!(syns.len(), 2 | 3), "Malformed '{}'", idents::IF);
                    let (cond, scope) = syns[0].eval(scope, Defs::NotAllowed)?;
                    let cond = cond.apply()?;
                    if cond.is_truthy() {
                        let (conseq, scope) = syns[1].eval(scope, Defs::NotAllowed)?;
                        Ok((conseq.apply()?, scope))
                    } else if let Some(alt) = syns.get(2) {
                        let (alt, scope) = alt.eval(scope, Defs::NotAllowed)?;
                        Ok((alt.apply()?, scope))
                    } else {
                        bail!(
                            "No alternative value for '{}' with {} predicate",
                            idents::IF,
                            idents::FALSE
                        );
                    }
                }

                [Self::Reserved(reserved @ (Reserved::And | Reserved::Or)), syns @ ..] => {
                    let is_and = *reserved == Reserved::And;
                    let initial = Item::Token(if is_and { Token::False } else { Token::True });

                    syns.iter()
                        .try_fold((false, initial, scope), |(done, prev, scope), syn| {
                            if done {
                                Ok((true, prev, scope))
                            } else {
                                let (item, scope) = syn.eval(scope, Defs::NotAllowed)?;
                                let item = item.apply()?;
                                let done = item.is_truthy() ^ is_and;
                                Ok((done, item, scope))
                            }
                        })
                        .map(|(_, item, scope)| (item, scope))
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
