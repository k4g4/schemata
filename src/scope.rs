use crate::{item::Item, proc::Proc};
use anyhow::{anyhow, Result};
use rustc_hash::FxHashMap;
use std::{
    cell::{Cell, RefCell},
    rc::{Rc, Weak},
};

#[derive(PartialEq, Copy, Clone, Debug)]
struct ScopeId(usize);

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
    defs: RefCell<FxHashMap<&'src str, Item<'src>>>,
}

impl<'src> Scope<'src> {
    pub fn new_global() -> Rc<Self> {
        Rc::new(Self {
            id: new_id(),
            parent: None,
            defs: Default::default(),
        })
    }

    pub fn new_local(parent: &ScopeHandle<'src>) -> Result<Rc<Self>> {
        if let ScopeHandle::Strong(_, scope) = parent {
            Ok(Rc::new(Self {
                id: new_id(),
                parent: Some(scope.clone()),
                defs: Default::default(),
            }))
        } else {
            Err(anyhow!("Unexpected weak ScopeHandle"))
        }
    }

    pub fn get_handle(self: &Rc<Self>) -> ScopeHandle<'src> {
        ScopeHandle::Strong(self.id, self.clone())
    }

    pub fn lookup(self: &Rc<Self>, ident: &str) -> Result<Option<Item<'src>>> {
        self.defs
            .borrow()
            .get(ident)
            .cloned()
            .map(|mut item| {
                // Always return a strong scope handle
                if let Item::Proc(Proc::Compound {
                    scope_handle: scope,
                    ..
                }) = &mut item
                {
                    // if let ScopeHandle::Weak(weak_scope) = scope {
                    //     let strong_scope = weak_scope
                    //         .upgrade()
                    //         .ok_or_else(|| anyhow!("failed to upgrade scope handle"))?;
                    //     *scope = ScopeHandle::Strong(self.id, strong_scope);
                    // }
                }
                Ok(item)
            })
            .or_else(|| {
                self.parent
                    .as_ref()
                    .and_then(|parent| parent.lookup(ident).transpose())
            })
            .transpose()
    }

    pub fn add(self: &Rc<Self>, ident: &'src str, mut item: Item<'src>) -> Result<()> {
        // Insert a weak scope handle if there's a matching ID
        if let Item::Proc(Proc::Compound {
            scope_handle: scope,
            ..
        }) = &mut item
        {
            // if let ScopeHandle::Strong(id, strong_scope) = scope {
            //     if *id == self.id {
            //         *scope = ScopeHandle::Weak(Rc::downgrade(strong_scope));
            //     }
            // }
        }
        // Global scope allows renaming variables, local scopes do not
        if self.defs.borrow_mut().insert(ident, item).is_some() && self.parent.is_some() {
            Err(anyhow!("Duplicate definition for '{ident}'"))
        } else {
            Ok(())
        }
    }
}

#[expect(private_interfaces)]
#[derive(Clone, Debug)]
pub enum ScopeHandle<'src> {
    Strong(ScopeId, Rc<Scope<'src>>),
    Weak(Weak<Scope<'src>>),
}
