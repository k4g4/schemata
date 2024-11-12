use crate::{
    idents,
    memory::ScopeRef,
    parse,
    proc::{Compound, Proc},
    sexpr::SExpr,
    syn::{self, Defs, Syn},
    utils::{self, ItemsIter},
};
use anyhow::{anyhow, bail, Context, Result};
use std::{borrow::Cow, f64, fmt, rc::Rc};

#[derive(Clone)]
pub enum Item<'src> {
    Num(f64),
    String(Rc<Cow<'src, str>>),
    Pair(Option<Rc<Pair<'src>>>),
    Proc(Proc<'src>),
    Token(Token),
    Sym(&'src str),
    Defined,
    Deferred {
        scope: ScopeRef<'src>,
        compound: Rc<Compound<'src>>,
    },
}

#[derive(Clone)]
pub struct Pair<'src> {
    pub head: Item<'src>,
    pub tail: Item<'src>,
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Token {
    True,
    False,
    Void,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::True => write!(f, "{}", idents::TRUE),
            Self::False => write!(f, "{}", idents::FALSE),
            Self::Void => write!(f, "{}", idents::VOID),
        }
    }
}

impl<'src> Item<'src> {
    pub fn nil() -> Self {
        Self::Pair(None)
    }

    pub fn cons(head: Self, tail: Self) -> Self {
        Self::Pair(Some(Rc::new(Pair { head, tail })))
    }

    pub fn bool(bool: bool) -> Self {
        Self::Token(if bool { Token::True } else { Token::False })
    }

    pub fn void() -> Self {
        Self::Token(Token::Void)
    }

    pub fn from_items(mut items: impl DoubleEndedIterator<Item = Result<Self>>) -> Result<Self> {
        items.try_rfold(Item::nil(), |tail, head| Ok(Item::cons(head?, tail)))
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Self::Token(Token::False))
    }

    pub fn iter(&self) -> ItemsIter<'_, 'src> {
        ItemsIter::new(if let Self::Pair(pair) = self {
            pair.as_deref()
        } else {
            None
        })
    }

    pub fn parent_scope(&self) -> Option<ScopeRef<'src>> {
        if let Self::Proc(Proc::Compound(compound)) = self {
            Some(compound.parent_scope())
        } else {
            None
        }
    }

    pub fn resolve(mut self) -> Result<Self> {
        let mut first_scope = None;
        while let Self::Deferred { scope, compound } = self {
            scope.push_stack();
            first_scope = first_scope.or(Some(scope));
            self = syn::eval_body(&compound.body(), scope, Defs::Allowed)
                .with_context(|| anyhow!("[while evaluating {compound}]"))?;
            scope.remove_heap()?;
            if matches!(self, Self::Defined) {
                bail!("Ill-placed '{}'", idents::DEFINE);
            }
        }
        first_scope.map(ScopeRef::pop_stack);
        Ok(self)
    }

    pub fn apply(self) -> Result<Self> {
        if let Self::Pair(pair) = &self {
            match pair.as_deref() {
                Some(Pair {
                    head: Self::Proc(proc),
                    tail,
                }) => proc.apply(tail.iter()),

                Some(Pair { head, .. }) => bail!("'{head}' is not a procedure"),

                None => bail!("'()' cannot be evaluated"),
            }
        } else {
            Ok(self)
        }
    }

    pub fn into_syn(&self) -> Result<Syn<'src>> {
        match self {
            &Self::Num(num) => Ok(Syn::Num(num)),
            Self::String(string) => Ok(Syn::String(string.as_ref().clone())),
            Self::Token(Token::True) => Ok(Syn::Ident(idents::TRUE)),
            Self::Token(Token::False) => Ok(Syn::Ident(idents::FALSE)),
            Self::Token(Token::Void) => Ok(Syn::Ident(idents::VOID)),
            Self::Sym(sym) => {
                if let Ok((b"", reserved)) = parse::reserved(sym.as_bytes()) {
                    Ok(Syn::Reserved(reserved))
                } else {
                    Ok(Syn::Ident(sym))
                }
            }
            Self::Proc(_) | Self::Deferred { .. } | Self::Defined => {
                Err(anyhow!("Unable to reflect on '{self}'"))
            }
            Self::Pair(pair) => {
                let syns = ItemsIter::new(pair.as_deref())
                    .map(Self::into_syn)
                    .collect::<Result<Vec<_>>>()?;
                Ok(Syn::SExpr(SExpr::new(syns)))
            }
        }
    }
}

impl fmt::Display for Item<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pretty = f.alternate();
        let width = f.width().unwrap_or(0);

        if pretty {
            utils::indent(f, width)?;
        }
        match self {
            Self::Num(num) => write!(f, "{num}"),
            Self::String(string) => write!(f, "\"{string}\""),
            Self::Proc(proc) => write!(f, "{proc}"),
            Self::Token(token) => write!(f, "{token}"),
            Self::Sym(sym) => write!(f, "{sym}"),
            Self::Defined => write!(f, "[{}]", idents::DEFINE),
            Self::Deferred { compound, .. } => {
                write!(f, "[Deferred - {compound}]")
            }

            Self::Pair(pair) => {
                if let Some(Pair { head, tail }) = pair.as_deref() {
                    write!(f, "(")?;
                    if pretty {
                        writeln!(f)?;
                    }

                    let (mut head, mut tail) = (head, tail);
                    loop {
                        if pretty {
                            writeln!(f, "{head:#width$}", width = width + 1)?;
                        } else {
                            write!(f, "{head}")?;
                        }
                        (head, tail) = match tail {
                            Self::Pair(Some(pair)) => {
                                write!(f, " ")?;
                                (&pair.head, &pair.tail)
                            }

                            Self::Pair(_) => break,

                            _ => {
                                if pretty {
                                    writeln!(f, ".")?;
                                } else {
                                    write!(f, " . ")?;
                                }
                                utils::indent(f, width)?;
                                if pretty {
                                    writeln!(f, "{tail:#width$}", width = width + 1)?;
                                } else {
                                    write!(f, "{tail} ")?;
                                }
                                break;
                            }
                        };
                    }

                    utils::indent(f, width)?;
                    write!(f, ")")
                } else {
                    write!(f, "()")
                }
            }
        }
    }
}
