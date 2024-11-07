use crate::{
    idents,
    item::{Item, Token},
    proc::{Arith, Cmp, ListManip, Proc, Trig},
    scope::Scope,
};
use anyhow::{bail, ensure, Result};
use std::{borrow::Cow, collections::VecDeque, fmt, rc::Rc};

#[derive(PartialEq, Clone, Debug)]
pub enum Syn<'src> {
    Num(f64),
    String(Cow<'src, str>),
    Ident(&'src str),
    Reserved(Reserved),
    SExpr(Vec<Syn<'src>>),
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Reserved {
    Define,
    Lambda,
    Let,
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
            Self::Lambda => write!(f, "{}", idents::LAMBDA),
            Self::Let => write!(f, "{}", idents::LET),
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

            Self::String(string) => Ok(Item::String(string.clone())),

            &Self::Ident(ident) => {
                if let Some(lookup) = scope.lookup(ident)? {
                    Ok(lookup)
                } else {
                    let builtin = match ident {
                        idents::CONS => Item::Proc(Proc::ListManip(ListManip::Cons)),
                        idents::CAR => Item::Proc(Proc::ListManip(ListManip::Car)),
                        idents::CDR => Item::Proc(Proc::ListManip(ListManip::Cdr)),

                        idents::TRUE => Item::Token(Token::True),
                        idents::FALSE => Item::Token(Token::False),
                        idents::VOID => Item::Token(Token::Void),

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
                        idents::TRUNC => Item::Proc(Proc::Trunc),
                        idents::FLOOR => Item::Proc(Proc::Floor),
                        idents::CEIL => Item::Proc(Proc::Ceil),

                        idents::SIN => Item::Proc(Proc::Trig(Trig::Sin)),
                        idents::COS => Item::Proc(Proc::Trig(Trig::Cos)),
                        idents::TAN => Item::Proc(Proc::Trig(Trig::Tan)),
                        idents::ASIN => Item::Proc(Proc::Trig(Trig::Asin)),
                        idents::ACOS => Item::Proc(Proc::Trig(Trig::Acos)),
                        idents::ATAN => Item::Proc(Proc::Trig(Trig::Atan)),
                        idents::SINH => Item::Proc(Proc::Trig(Trig::Sinh)),
                        idents::COSH => Item::Proc(Proc::Trig(Trig::Cosh)),
                        idents::TANH => Item::Proc(Proc::Trig(Trig::Tanh)),
                        idents::ASINH => Item::Proc(Proc::Trig(Trig::Asinh)),
                        idents::ACOSH => Item::Proc(Proc::Trig(Trig::Acosh)),
                        idents::ATANH => Item::Proc(Proc::Trig(Trig::Atanh)),

                        idents::DISP => Item::Proc(Proc::Display),
                        idents::NEWL => Item::Proc(Proc::Newline),
                        idents::ERROR => Item::Proc(Proc::Error),

                        _ => {
                            bail!("Failed to find definition for '{ident}'");
                        }
                    };

                    Ok(builtin)
                }
            }

            Self::Reserved(reserved) => bail!("Unexpected '{reserved}'"),

            Self::SExpr(group) => match group.as_slice() {
                [] => Ok(Item::nil()),

                [Self::Reserved(Reserved::Define), ..] if defs == Defs::NotAllowed => {
                    bail!("Ill-placed '{}'", idents::DEFINE);
                }

                [Self::Reserved(Reserved::Define), Self::Ident(ident), def] => {
                    scope.add(ident, def.eval(scope, Defs::NotAllowed)?)?;
                    Ok(Item::Defined)
                }

                [Self::Reserved(Reserved::Define), Self::SExpr(signature), body @ ..] => {
                    ensure!(!body.is_empty(), "Empty '{}' body", idents::DEFINE);
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
                                .collect::<Result<_>>()?;
                            let proc = Item::Proc(Proc::Compound {
                                name: Some(ident),
                                params,
                                scope_handle: scope.get_handle(),
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

                [Self::Reserved(Reserved::Lambda), Self::SExpr(params), body @ ..] => {
                    ensure!(!body.is_empty(), "Empty '{}' body", idents::LAMBDA);
                    let params = params
                        .iter()
                        .map(|param| {
                            if let &Syn::Ident(ident) = param {
                                Ok(ident)
                            } else {
                                bail!("'{param}' is not a valid parameter name");
                            }
                        })
                        .collect::<Result<_>>()?;
                    Ok(Item::Proc(Proc::Compound {
                        name: None,
                        params,
                        scope_handle: scope.get_handle(),
                        body,
                    }))
                }

                [Self::Reserved(Reserved::Lambda), ..] => {
                    bail!("Malformed '{}'", idents::LAMBDA)
                }

                [Self::Reserved(Reserved::Let), Self::SExpr(mappings), body @ ..] => {
                    ensure!(!body.is_empty(), "Empty '{}' body", idents::LET);
                    let params = mappings
                        .iter()
                        .map(|mapping| {
                            if let Syn::SExpr(group) = mapping {
                                if let &[Self::Ident(param), _] = group.as_slice() {
                                    Ok(param)
                                } else {
                                    bail!("Malformed '{}'", idents::LET);
                                }
                            } else {
                                bail!("Malformed '{}'", idents::LET);
                            }
                        })
                        .collect::<Result<_>>()?;
                    let defs = mappings.iter().map(|mapping| {
                        if let Syn::SExpr(group) = mapping {
                            &group[1]
                        } else {
                            unreachable!("already checked")
                        }
                    });
                    let lambda = {
                        let proc = Item::Proc(Proc::Compound {
                            name: None,
                            params,
                            scope_handle: scope.get_handle(),
                            body,
                        });
                        let mut defs = defs
                            .map(|def| def.eval(scope, Defs::NotAllowed))
                            .collect::<VecDeque<_>>();
                        defs.push_front(Ok(proc));
                        Item::from_items(defs)?
                    };
                    lambda.apply()
                }

                [Self::Reserved(Reserved::Let), ..] => {
                    bail!("Malformed '{}'", idents::LET)
                }

                [Self::Reserved(Reserved::Cond), conds @ ..] if !conds.is_empty() => {
                    for (i, cond) in conds.iter().enumerate() {
                        if let Self::SExpr(group) = cond {
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
                        syns[1].eval(scope, Defs::NotAllowed)
                    } else if let Some(alt) = syns.get(2) {
                        alt.eval(scope, Defs::NotAllowed)
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

                _ => Item::from_items(group.iter().map(|syn| syn.eval(scope, Defs::NotAllowed)))
                    .and_then(Item::apply),
            },
        }
    }
}

impl fmt::Display for Syn<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pretty = f.alternate();
        let width = f.width().unwrap_or(0);
        let indent = |f: &mut fmt::Formatter| {
            if pretty {
                for _ in 0..width {
                    write!(f, "   ")?;
                }
            }
            Ok(())
        };
        indent(f)?;

        match self {
            Self::Num(num) => write!(f, "{num}"),
            Self::String(string) => write!(f, "\"{string}\""),
            Self::Ident(ident) => write!(f, "{ident}"),
            Self::Reserved(reserved) => write!(f, "{reserved}"),
            Self::SExpr(items) if items.is_empty() => write!(f, "()"),
            Self::SExpr(items) => {
                write!(f, "(")?;
                if pretty {
                    writeln!(f)?;
                }
                for item in items {
                    if pretty {
                        writeln!(f, "{item:#width$}", width = width + 1)?;
                    } else {
                        write!(f, "{item} ")?;
                    }
                }
                indent(f)?;
                write!(f, ")")
            }
        }
    }
}
