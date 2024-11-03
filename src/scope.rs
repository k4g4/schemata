use crate::item::Item;
use rustc_hash::FxHashMap;
use std::{cell::RefCell, rc::Rc};

type Defs<'src> = FxHashMap<&'src str, Item<'src>>;

#[derive(Clone, Debug)]
pub struct Scope<'src> {
    parent: Option<Rc<Scope<'src>>>,
    defs: RefCell<Defs<'src>>,
}

impl<'src> Scope<'src> {
    pub fn new_global() -> Rc<Self> {
        Rc::new(Self {
            parent: Default::default(),
            defs: Default::default(),
        })
    }

    pub fn new_local(parent: &Rc<Scope<'src>>) -> Rc<Self> {
        Rc::new(Self {
            parent: Some(parent.clone()),
            defs: Default::default(),
        })
    }

    pub fn lookup(&self, ident: &str) -> Option<Item<'src>> {
        self.defs
            .borrow()
            .get(ident)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.lookup(ident)))
    }

    pub fn add(&self, ident: &'src str, item: Item<'src>) {
        self.defs.borrow_mut().insert(ident, item);
    }
}
