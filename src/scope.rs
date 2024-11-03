use crate::item::{Item, StoredItem};
use anyhow::{anyhow, Context, Result};
use rustc_hash::FxHashMap;
use std::{
    cell::{Cell, RefCell},
    rc::{Rc, Weak},
};

// Scopes act as an environment for each item, including compound procedures.
// These compound procedures need to be able to refer to themselves, which means
// they need references to the scope... creating circular dependencies that might
// leak memory when just using Rc. This file provides StoredScope and BorrowedScope
// handles that keep track of the ID for their original scope. If an item is
// stored in a scope, it must only hold a StoredScope. If an item is looked up
// from a scope, it will hold a BorrowedScope. While a StoredScope is stored
// in the scope it originated from, it will remain downgraded to a weak reference.

#[derive(PartialEq, Copy, Clone, Debug)]
pub struct ScopeId(usize);

fn new_id() -> ScopeId {
    thread_local! {
        static CURR_ID: Cell<ScopeId> = Cell::new(ScopeId(0));
    }
    let new_id = ScopeId(CURR_ID.get().0 + 1);
    CURR_ID.set(new_id);
    new_id
}

#[derive(Clone, Debug)]
pub struct Scope<'src> {
    id: ScopeId,
    parent: Option<Rc<Scope<'src>>>,
    defs: RefCell<FxHashMap<&'src str, StoredItem<'src>>>,
}

impl<'src> Scope<'src> {
    pub fn new_global() -> Rc<Self> {
        Rc::new(Self {
            id: new_id(),
            parent: None,
            defs: Default::default(),
        })
    }

    pub fn new_local(parent: impl AsRef<Rc<Scope<'src>>>) -> Rc<Self> {
        Rc::new(Self {
            id: new_id(),
            parent: Some(parent.as_ref().clone()),
            defs: Default::default(),
        })
    }

    pub fn lookup(self: &Rc<Self>, ident: &str) -> Result<Option<Item<'src>>> {
        self.defs
            .borrow()
            .get(ident)
            .cloned()
            .map(|item| item.into_borrowed(self.id))
            .or_else(|| {
                self.parent
                    .as_ref()
                    .and_then(|parent| parent.lookup(ident).transpose())
            })
            .transpose()
    }

    pub fn add(self: &Rc<Self>, ident: &'src str, item: Item<'src>) -> Result<()> {
        // Global scope allows renaming variables, local scopes do not
        if self
            .defs
            .borrow_mut()
            .insert(ident, item.into_stored(self.id))
            .is_some()
            && self.parent.is_some()
        {
            Err(anyhow!("Duplicate definition for '{ident}'"))
        } else {
            Ok(())
        }
    }
}

#[derive(Clone, Debug)]
pub enum StoredScope<'src> {
    Strong(ScopeId, Rc<Scope<'src>>),
    Weak(Weak<Scope<'src>>),
}

impl<'src> StoredScope<'src> {
    pub fn into_borrowed(self, id: ScopeId) -> Result<BorrowedScope<'src>> {
        match self {
            Self::Strong(self_id, scope) => Ok(BorrowedScope { id: self_id, scope }),
            Self::Weak(scope) => Ok(BorrowedScope {
                id,
                scope: scope.upgrade().context("While upgrading scope")?,
            }),
        }
    }
}

#[derive(Clone, Debug)]
pub struct BorrowedScope<'src> {
    id: ScopeId,
    scope: Rc<Scope<'src>>,
}

impl<'src> BorrowedScope<'src> {
    pub fn into_stored(self, id: ScopeId) -> StoredScope<'src> {
        if id == self.id {
            StoredScope::Weak(Rc::downgrade(&self.scope))
        } else {
            StoredScope::Strong(self.id, self.scope)
        }
    }
}

impl<'src> AsRef<Rc<Scope<'src>>> for BorrowedScope<'src> {
    fn as_ref(&self) -> &Rc<Scope<'src>> {
        &self.scope
    }
}

impl<'src> From<Rc<Scope<'src>>> for BorrowedScope<'src> {
    fn from(scope: Rc<Scope<'src>>) -> Self {
        Self {
            id: scope.id,
            scope,
        }
    }
}
