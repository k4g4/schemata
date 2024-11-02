use crate::item::Item;
use rustc_hash::FxHashMap;
use std::{cell::RefCell, rc::Rc};

type Defs<'src> = FxHashMap<&'src str, Item<'src>>;

// In Scheme, global scopes are mutable! Thus functions that read from global defs
// aren't actually idempotent. Local scopes, however, will copy-on-write like they should.
#[derive(Clone, Debug)]
pub enum Scope<'src> {
    Global {
        defs: RefCell<Defs<'src>>,
    },
    Local {
        parent: Rc<Scope<'src>>,
        defs: Defs<'src>,
    },
}

impl<'src> Scope<'src> {
    pub fn new_global() -> Rc<Self> {
        Rc::new(Self::Global {
            defs: Default::default(),
        })
    }

    pub fn new_local(parent: Rc<Scope<'src>>) -> Rc<Self> {
        Rc::new(Self::Local {
            parent,
            defs: Default::default(),
        })
    }

    pub fn lookup(&self, ident: &str) -> Option<Item<'src>> {
        match self {
            Self::Global { defs } => defs.borrow().get(ident).cloned(),
            Self::Local { defs, parent } => {
                defs.get(ident).cloned().or_else(|| parent.lookup(ident))
            }
        }
    }

    pub fn add(mut self: Rc<Self>, ident: &'src str, item: Item<'src>) -> Rc<Self> {
        match self.as_ref() {
            Self::Global { defs } => {
                defs.borrow_mut().insert(ident, item);
            }
            Self::Local { .. } => match Rc::make_mut(&mut self) {
                Self::Local { defs, .. } => {
                    defs.insert(ident, item);
                }
                _ => unreachable!("already checked that it's Local"),
            },
        }
        self
    }
}
