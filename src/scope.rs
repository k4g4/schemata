use crate::item::Item;
use anyhow::{anyhow, Result};
use rustc_hash::FxHashMap;
use std::{cell::RefCell, rc::Rc};

#[derive(Clone, Default, Debug)]
pub struct Scope<'src> {
    parent: Option<Rc<Scope<'src>>>,
    defs: RefCell<FxHashMap<&'src str, Item<'src>>>,
}

impl<'src> Scope<'src> {
    pub fn new_global() -> Rc<Self> {
        Rc::new(Default::default())
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

    pub fn add(&self, ident: &'src str, item: Item<'src>) -> Result<()> {
        // Global scope allows renaming variables, local scopes do not
        if self.defs.borrow_mut().insert(ident, item).is_some() && self.parent.is_some() {
            Err(anyhow!("Duplicate definition for '{ident}'"))
        } else {
            Ok(())
        }
    }
}
