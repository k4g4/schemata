use crate::{globals, item::Item};
use anyhow::{bail, Result};
use rustc_hash::{FxHashMap, FxHashSet};
use std::cell::{Cell, RefCell};

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
struct ScopeId(usize);

fn new_id() -> ScopeId {
    thread_local! {
        static CURR_ID: Cell<ScopeId> = Cell::new(ScopeId(0));
    }
    CURR_ID.replace(ScopeId(CURR_ID.get().0 + 1))
}

#[derive(Debug)]
struct Scope<'src> {
    parent: Option<ScopeId>,
    defs: FxHashMap<&'src str, Item<'src>>,
}

#[derive(Default, Debug)]
pub struct Heap<'src>(RefCell<FxHashMap<ScopeId, Scope<'src>>>);

#[derive(Copy, Clone, Debug)]
pub struct ScopeHandle<'src> {
    heap: &'src Heap<'src>,
    id: ScopeId,
}

impl<'src> ScopeHandle<'src> {
    pub fn new_global(heap: &'src Heap<'src>) -> Self {
        let id = new_id();
        heap.0.borrow_mut().insert(
            id,
            Scope {
                parent: None,
                defs: Default::default(),
            },
        );
        Self { heap, id }
    }

    pub fn new_local(self) -> Self {
        let id = new_id();
        self.heap.0.borrow_mut().insert(
            id,
            Scope {
                parent: Some(self.id),
                defs: Default::default(),
            },
        );
        Self {
            heap: self.heap,
            id,
        }
    }

    pub fn add(self, ident: &'src str, item: Item<'src>) -> Result<()> {
        let mut heap = self.heap.0.borrow_mut();
        let Some(scope) = heap.get_mut(&self.id) else {
            bail!("Failed to find scope for {self:?}");
        };
        // Redefinitions are only permitted in the global scope
        if scope.defs.insert(ident, item).is_some() && scope.parent.is_some() {
            bail!("Duplicate definition for '{ident}'");
        }
        Ok(())
    }

    pub fn lookup(self, ident: &str) -> Result<Option<Item<'src>>> {
        let heap = self.heap.0.borrow();
        let Some(scope) = heap.get(&self.id) else {
            bail!("Failed to find scope for {self:?}");
        };
        if let Some(item) = scope.defs.get(ident).cloned() {
            Ok(Some(item))
        } else if let Some(parent) = scope.parent {
            Self {
                heap: self.heap,
                id: parent,
            }
            .lookup(ident)
        } else {
            Ok(None)
        }
    }

    pub fn collect_garbage(self) -> Result<()> {
        // Run garbage collection based on the configured frequency
        thread_local! {
            static FREQ: Cell<usize> = Cell::new(0);
        }
        let freq = globals::gc_freq();
        if freq == 0 || FREQ.replace((FREQ.get() + 1) % freq) != freq - 1 {
            return Ok(());
        }

        let heap = self.heap.0.borrow();
        let mut reachable = Default::default();
        let mut next_id = Some(self.id);

        fn get_reachable_ids(
            heap: &Heap,
            id: ScopeId,
            reachable: &mut FxHashSet<ScopeId>,
        ) -> Result<()> {
            let heap_ref = heap.0.borrow();
            let Some(scope) = heap_ref.get(&id) else {
                bail!("Failed to find scope for {id:?}");
            };
            let handles = scope.defs.values().flat_map(Item::scope);
            for handle in handles {
                get_reachable_ids(heap, handle.id, reachable)?;
            }
            Ok(())
        }

        while let Some(id) = next_id {
            let Some(scope) = heap.get(&id) else {
                bail!("Failed to find scope for {id:?}");
            };
            get_reachable_ids(&self.heap, id, &mut reachable)?;
            next_id = scope.parent;
        }

        drop(heap);
        self.heap
            .0
            .borrow_mut()
            .retain(|id, _| reachable.contains(id));
        Ok(())
    }
}
