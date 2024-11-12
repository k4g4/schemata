use crate::{
    idents,
    item::Item,
    memory::ScopeRef,
    proc::{Compound, Proc},
    syn::{self, Defs, Policy, Reserved, Syn},
    utils,
};
use anyhow::{bail, ensure, Result};
use std::fmt;

#[derive(PartialEq, Clone, Debug)]
pub struct SExpr<'src>(Vec<Syn<'src>>);

impl<'src> SExpr<'src> {
    pub fn new(syns: Vec<Syn<'src>>) -> Self {
        Self(syns)
    }

    pub fn eval(&self, scope: ScopeRef<'src>, defs: Defs) -> Result<Item<'src>> {
        match self.0.as_slice() {
            [] => Ok(Item::nil()),

            [Syn::Reserved(Reserved::Define), ..] if defs == Defs::NotAllowed => {
                bail!("Ill-placed '{}'", idents::DEFINE);
            }

            [Syn::Reserved(Reserved::Define), Syn::Ident(ident), def] => def
                .eval(scope, Defs::NotAllowed, Policy::Resolve)
                .and_then(|item| scope.add(ident, item))
                .map(|_| Item::Defined),

            [Syn::Reserved(Reserved::Define), Syn::SExpr(signature), body @ ..] => {
                if let [Syn::Ident(ident), params @ ..] = signature.0.as_slice() {
                    let item = Item::Proc(Proc::Compound(Compound::new(Some(ident), scope, params.iter(), body)?));
                        scope.add(ident, item)?;
                        Ok(Item::Defined)
                } else {
                    bail!("Malformed signature")
                }
            }

            [Syn::Reserved(Reserved::Lambda), Syn::SExpr(params), body @ ..] => {
                Ok(Item::Proc(Proc::Compound(Compound::new(None, scope, params.0.iter(), body)?)))
            }

            [Syn::Reserved(Reserved::Let), Syn::SExpr(mappings), body @ ..] => {
                ensure!(!body.is_empty(), "Empty '{}' body", idents::LET);
                ensure!(
                    mappings.0.iter().all(|mapping| {
                        matches!(mapping, Syn::SExpr(s_expr) if matches!(s_expr.0.as_slice(), [Syn::Ident(_), _]))                    
                    }),
                    "Malformed '{}'", idents::LET,
                );
                let params = mappings.0.iter().map(|mapping| {
                    if let Syn::SExpr(s_expr) = mapping {
                        if let [param @ Syn::Ident(_), _] = s_expr.0.as_slice() {
                            param
                        } else {
                            panic!("already checked")
                        }
                    } else {
                        panic!("already checked")
                    }
                });
                let defs = mappings.0.iter().map(|mapping| {
                    if let Syn::SExpr(s_expr) = mapping {
                        &s_expr.0[1]
                    } else {
                        unreachable!("already checked")
                    }
                });
                let proc = Item::Proc(Proc::Compound(Compound::new(None, scope, params, body)?));
                let mut items = Vec::with_capacity(defs.len() + 1);
                items.push(Ok(proc));
                items.extend(defs.map(|def| def.eval(scope, Defs::NotAllowed, Policy::Resolve)));
                Item::from_items(items.into_iter()).and_then(Item::apply)
            }

            [Syn::Reserved(Reserved::Begin), syns @ ..] if !syns.is_empty() => {
                syn::eval_body(syns, scope, defs)
            }

            [Syn::Reserved(Reserved::Cond), conds @ ..] if !conds.is_empty() => {
                for (i, cond) in conds.iter().enumerate() {
                    if let Syn::SExpr(s_expr) = cond {
                        match s_expr.0.as_slice() {
                            [] => bail!("Malformed '{}'", idents::COND),

                            [Syn::Reserved(Reserved::Else)] => {
                                bail!("Malformed '{}'", idents::ELSE);
                            }

                            [Syn::Reserved(Reserved::Else), syns @ ..] if !syns.is_empty() => {
                                if i == conds.len() - 1 {
                                    return syn::eval_body(syns, scope, Defs::NotAllowed);
                                } else {
                                    bail!("Ill-placed '{}'", idents::ELSE);
                                };
                            }

                            [cond, syns @ ..] => {
                                let item = cond.eval(scope, Defs::NotAllowed, Policy::Resolve)?;
                                if item.is_truthy() {
                                    return syn::eval_body(syns, scope, Defs::NotAllowed);
                                }
                            }
                        }
                    } else {
                        bail!("Malformed '{}'", idents::COND);
                    }
                }

                bail!("All conditions are '{}'", idents::FALSE);
            }

            [Syn::Reserved(Reserved::If), syns @ ..] => {
                ensure!(matches!(syns.len(), 2 | 3), "Malformed '{}'", idents::IF);
                let cond = syns[0].eval(scope, Defs::NotAllowed, Policy::Resolve)?;
                if cond.is_truthy() {
                    syns[1].eval(scope, Defs::NotAllowed, Policy::Defer)
                } else if let Some(alt) = syns.get(2) {
                    alt.eval(scope, Defs::NotAllowed, Policy::Defer)
                } else {
                    Ok(Item::void())
                }
            }

            [Syn::Reserved(reserved @ (Reserved::And | Reserved::Or)), syns @ ..] => {
                let is_and = *reserved == Reserved::And;
                let initial = Item::bool(!is_and);

                syns.iter()
                    .try_fold((false, initial), |(done, prev), syn| {
                        if done {
                            Ok((true, prev))
                        } else {
                            let item = syn.eval(scope, Defs::NotAllowed, Policy::Resolve)?;
                            let done = item.is_truthy() ^ is_and;
                            Ok((done, item))
                        }
                    })
                    .map(|(_, item)| item)
            }

            [Syn::Reserved(reserved), ..] => bail!("Malformed '{}'", reserved.as_str()),

            _ => Item::from_items(
                self.0
                    .iter()
                    .map(|syn| syn.eval(scope, Defs::NotAllowed, Policy::Resolve)),
            )
            .and_then(Item::apply),
        }
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &Syn<'src>> {
        self.0.iter()
    }
}

impl<'src> fmt::Display for SExpr<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pretty = f.alternate();
        let width = f.width().unwrap_or(0);

        if self.0.is_empty() {
            write!(f, "()")
        } else {
            write!(f, "(")?;
            if pretty {
                writeln!(f)?;
            }
            for (i, item) in self.0.iter().enumerate() {
                if pretty {
                    writeln!(f, "{item:#width$}", width = width + 1)?;
                } else {
                    write!(f, "{item}")?;
                    if i != self.0.len() - 1 {
                        write!(f, " ")?;
                    }
                }
            }
            utils::indent(f, width)?;
            write!(f, ")")
        }
    }
}
