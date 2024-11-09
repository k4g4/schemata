use crate::{
    idents,
    item::Item,
    proc::Proc,
    scope::Scope,
    syn::{Defs, Reserved, Syn},
    utils,
};
use anyhow::{bail, ensure, Result};
use std::{fmt, rc::Rc};

#[derive(PartialEq, Clone, Debug)]
pub struct SExpr<'src>(Vec<Syn<'src>>);

impl<'src> SExpr<'src> {
    pub fn new(syns: Vec<Syn<'src>>) -> Self {
        Self(syns)
    }

    pub fn eval(&'src self, scope: &Rc<Scope<'src>>, defs: Defs) -> Result<Item<'src>> {
        match self.0.as_slice() {
            [] => Ok(Item::nil()),

            [Syn::Reserved(Reserved::Define), ..] if defs == Defs::NotAllowed => {
                bail!("Ill-placed '{}'", idents::DEFINE);
            }

            [Syn::Reserved(Reserved::Define), Syn::Ident(ident), def] => {
                scope.add(ident, def.eval(scope, Defs::NotAllowed)?)?;
                Ok(Item::Defined)
            }

            [Syn::Reserved(Reserved::Define), Syn::SExpr(signature), body @ ..] => {
                if let [Syn::Ident(ident), params @ ..] = signature.0.as_slice() {
                    scope.add(ident, Self::create_proc(scope, Some(ident), params, body)?)?;
                    Ok(Item::Defined)
                } else {
                    bail!("Malformed signature")
                }
            }

            [Syn::Reserved(Reserved::Lambda), Syn::SExpr(params), body @ ..] => {
                Self::create_proc(scope, None, &params.0, body)
            }

            [Syn::Reserved(Reserved::Let), Syn::SExpr(mappings), body @ ..] => {
                ensure!(!body.is_empty(), "Empty '{}' body", idents::LET);
                let params = mappings
                    .0
                    .iter()
                    .map(|mapping| {
                        if let Syn::SExpr(sexpr) = mapping {
                            if let &[Syn::Ident(param), _] = sexpr.0.as_slice() {
                                Ok(param)
                            } else {
                                bail!("Malformed '{}'", idents::LET);
                            }
                        } else {
                            bail!("Malformed '{}'", idents::LET);
                        }
                    })
                    .collect::<Result<_>>()?;
                let defs = mappings.0.iter().map(|mapping| {
                    if let Syn::SExpr(group) = mapping {
                        &group.0[1]
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
                    let mut items = Vec::with_capacity(defs.len() + 1);
                    items.push(Ok(proc));
                    items.extend(defs.map(|def| def.eval(scope, Defs::NotAllowed)));
                    Item::from_items(items.into_iter())?
                };
                lambda.apply()
            }

            [Syn::Reserved(Reserved::Begin), syn, syns @ ..] => syns
                .iter()
                .try_fold(syn.eval(scope, defs)?, |_, syn| syn.eval(scope, defs)),

            [Syn::Reserved(Reserved::Cond), conds @ ..] if !conds.is_empty() => {
                for (i, cond) in conds.iter().enumerate() {
                    if let Syn::SExpr(group) = cond {
                        match group.0.as_slice() {
                            [] => bail!("Malformed '{}'", idents::COND),

                            [Syn::Reserved(Reserved::Else)] => {
                                bail!("Malformed '{}'", idents::ELSE);
                            }

                            [Syn::Reserved(Reserved::Else), syn, syns @ ..] => {
                                if i == conds.len() - 1 {
                                    return syns
                                        .iter()
                                        .try_fold(syn.eval(scope, Defs::NotAllowed)?, |_, syn| {
                                            syn.eval(scope, Defs::NotAllowed)
                                        });
                                } else {
                                    bail!("Ill-placed '{}'", idents::ELSE);
                                };
                            }

                            [cond, syns @ ..] => {
                                let item = cond.eval(scope, Defs::NotAllowed)?;
                                if item.is_truthy() {
                                    return syns.iter().try_fold(item, |_, syn| {
                                        syn.eval(scope, Defs::NotAllowed)
                                    });
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
                let cond = syns[0].eval(scope, Defs::NotAllowed)?.apply()?;
                if cond.is_truthy() {
                    syns[1].eval(scope, Defs::NotAllowed)
                } else if let Some(alt) = syns.get(2) {
                    alt.eval(scope, Defs::NotAllowed)
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
                            let item = syn.eval(scope, Defs::NotAllowed)?.apply()?;
                            let done = item.is_truthy() ^ is_and;
                            Ok((done, item))
                        }
                    })
                    .map(|(_, item)| item)
            }

            [Syn::Reserved(reserved), ..] => bail!("Malformed '{}'", reserved.as_str()),

            _ => Item::from_items(self.0.iter().map(|syn| syn.eval(scope, Defs::NotAllowed)))
                .and_then(Item::apply),
        }
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &Syn> {
        self.0.iter()
    }

    fn create_proc(
        scope: &Rc<Scope<'src>>,
        name: Option<&'src str>,
        params: &[Syn<'src>],
        body: &'src [Syn<'src>],
    ) -> Result<Item<'src>> {
        ensure!(!body.is_empty(), "Empty '{}' body", idents::DEFINE);
        if let Some(Syn::Ident(duplicate)) = params
            .iter()
            .find(|param| params.iter().filter(|p| p == param).count() != 1)
        {
            bail!(
                "Duplicate parameter '{duplicate}' specified for {}",
                name.unwrap_or(idents::LAMBDA)
            );
        }
        if let Some(dot_pos) = params.iter().position(|syn| matches!(syn, Syn::Ident("."))) {
            ensure!(
                dot_pos == params.len().wrapping_sub(2),
                "Misplaced dot in signature in {}",
                name.unwrap_or(idents::LAMBDA)
            );
        }

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
            name,
            params,
            scope_handle: scope.get_handle(),
            body,
        }))
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
            for item in &self.0 {
                if pretty {
                    writeln!(f, "{item:#width$}", width = width + 1)?;
                } else {
                    write!(f, "{item} ")?;
                }
            }
            utils::indent(f, width)?;
            write!(f, ")")
        }
    }
}