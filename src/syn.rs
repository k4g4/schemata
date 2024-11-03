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
    pub fn eval(&'src self, scope: &Rc<Scope<'src>>, defs: Defs) -> Result<Item<'src>> {
        match self {
            &Self::Num(n) => Ok(Item::Num(n)),

            &Self::Ident(ident) => {
                if let Some(lookup) = scope.lookup(ident)? {
                    Ok(lookup)
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

                        idents::LOG => Item::Proc(Proc::Log),
                        idents::EXP => Item::Proc(Proc::Exp),
                        idents::REM => Item::Proc(Proc::Rem),

                        _ => {
                            bail!("Failed to find definition for '{ident}'");
                        }
                    };

                    Ok(builtin)
                }
            }

            Self::Reserved(reserved) => bail!("Unexpected '{reserved}'"),

            Self::Group(group) => match group.as_slice() {
                [] => Ok(Item::nil()),

                [Self::Reserved(Reserved::Define), ..] if defs == Defs::NotAllowed => {
                    bail!("Ill-placed '{}'", idents::DEFINE);
                }

                [Self::Reserved(Reserved::Define), Self::Ident(ident), def] => {
                    scope.add(ident, def.eval(scope, Defs::NotAllowed)?)?;
                    Ok(Item::Defined)
                }

                [Self::Reserved(Reserved::Define), Self::Group(signature), body @ ..] => {
                    ensure!(!body.is_empty(), "Empty procedure body");
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
                            let proc = Item::Proc(Proc::Compound {
                                name: Some(ident),
                                params,
                                scope: scope.clone().into(),
                                body,
                            });
                            scope.add(ident, proc)?;
                            Ok(Item::Defined)
                        }

                        _ => bail!("Malformed signature"),
                    }
                }

                [Self::Reserved(Reserved::Define), ..] => {
                    bail!("Malformed '{}'", idents::DEFINE)
                }

                [Self::Reserved(Reserved::Cond), conds @ ..] if !conds.is_empty() => {
                    for (i, cond) in conds.iter().enumerate() {
                        if let Self::Group(group) = cond {
                            match group.as_slice() {
                                [] => bail!("Malformed '{}'", idents::COND),

                                [Self::Reserved(Reserved::Else)] => {
                                    bail!("Malformed '{}'", idents::ELSE);
                                }

                                [Self::Reserved(Reserved::Else), syn, syns @ ..] => {
                                    return if i == conds.len() - 1 {
                                        let mut item = syn.eval(scope, Defs::NotAllowed)?;
                                        for syn in syns {
                                            item = syn.eval(scope, Defs::NotAllowed)?;
                                        }
                                        item.apply()
                                    } else {
                                        bail!("Ill-placed '{}'", idents::ELSE);
                                    };
                                }

                                [cond, syns @ ..] => {
                                    let mut item = cond.eval(scope, Defs::NotAllowed)?.apply()?;
                                    if item.is_truthy() {
                                        for syn in syns {
                                            item = syn.eval(scope, Defs::NotAllowed)?;
                                        }
                                        return item.apply();
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
                    let cond = syns[0].eval(scope, Defs::NotAllowed)?.apply()?;
                    if cond.is_truthy() {
                        syns[1].eval(scope, Defs::NotAllowed)?.apply()
                    } else if let Some(alt) = syns.get(2) {
                        alt.eval(scope, Defs::NotAllowed)?.apply()
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
                        .try_fold((false, initial), |(done, prev), syn| {
                            if done {
                                Ok((true, prev))
                            } else {
                                let item = syn.eval(scope, Defs::NotAllowed)?.apply()?;
                                let done = item.is_truthy() ^ is_and;
                                Ok((done, item))
                            }
                        })
                        .map(|(_, item)| item)
                }

                _ => group
                    .iter()
                    .try_rfold(Item::nil(), |tail, syn| {
                        syn.eval(scope, Defs::NotAllowed)
                            .map(|head| Item::cons(head, tail))
                    })
                    .and_then(Item::apply),
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
