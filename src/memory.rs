use crate::{globals, item::Item};
use anyhow::{anyhow, bail, Context, Result};
use rustc_hash::{FxHashMap, FxHashSet};
use std::{
    cell::{Cell, Ref, RefCell},
    fmt,
};

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
struct ScopeId(u64);

fn new_id() -> ScopeId {
    thread_local! {
        static CURR_ID: Cell<ScopeId> = Cell::new(ScopeId(0));
    }
    CURR_ID.replace(ScopeId(CURR_ID.get().0 + 1))
}

struct Scope<'src> {
    parent: Option<ScopeId>,
    defs: FxHashMap<&'src str, Item<'src>>,
}

#[derive(Default)]
pub struct Mem<'src> {
    heap: FxHashMap<ScopeId, Scope<'src>>,
    stack: Vec<ScopeId>,
    defs_bin: Vec<FxHashMap<&'src str, Item<'src>>>,
}

impl fmt::Debug for Mem<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Stack: {:?}", self.stack)?;
        writeln!(f, "Heap:")?;
        for (ScopeId(id), scope) in self.heap.iter() {
            writeln!(f, "{id:03}:")?;
            writeln!(f, "  parent({:?})", scope.parent)?;
            for (ident, item) in &scope.defs {
                writeln!(f, "    {ident} - {item}")?;
            }
        }
        Ok(())
    }
}

#[derive(Copy, Clone)]
pub struct ScopeHandle<'src> {
    mem: &'src RefCell<Mem<'src>>,
    id: ScopeId,
}

impl<'src> ScopeHandle<'src> {
    pub fn new_global(mem: &'src RefCell<Mem<'src>>) -> Self {
        let id = new_id();
        {
            let mut mem = mem.borrow_mut();
            let defs = mem.defs_bin.pop().unwrap_or_default();
            mem.heap.insert(id, Scope { parent: None, defs });
            mem.stack.push(id);
        }
        Self { mem, id }
    }

    pub fn new_local(self) -> Self {
        let id = new_id();
        let mut mem = self.mem.borrow_mut();
        let defs = mem.defs_bin.pop().unwrap_or_default();
        mem.heap.insert(
            id,
            Scope {
                parent: Some(self.id),
                defs,
            },
        );
        mem.stack.push(id);
        Self { mem: self.mem, id }
    }

    pub fn add(self, ident: &'src str, item: Item<'src>) -> Result<()> {
        let mut mem = self.mem.borrow_mut();
        let Some(scope) = mem.heap.get_mut(&self.id) else {
            bail!("Failed to find scope for {self:?}");
        };
        // Redefinitions are only permitted in the global scope
        if scope.defs.insert(ident, item).is_some() && scope.parent.is_some() {
            bail!("Duplicate definition for '{ident}'");
        }
        Ok(())
    }

    pub fn lookup(self, ident: &str) -> Result<Option<Item<'src>>> {
        let mem = self.mem.borrow();
        let Some(scope) = mem.heap.get(&self.id) else {
            bail!("Failed to find scope for {self:?}");
        };
        if let Some(item) = scope.defs.get(ident).cloned() {
            Ok(Some(item))
        } else if let Some(parent) = scope.parent {
            Self {
                mem: self.mem,
                id: parent,
            }
            .lookup(ident)
        } else {
            Ok(None)
        }
    }

    pub fn pop_stack(self) -> Result<()> {
        let mut mem = self.mem.borrow_mut();
        if mem.stack.last().is_some_and(|&last_id| last_id == self.id) {
            mem.stack.pop();
        } else {
            bail!("Corrupted stack ({:?})", mem.stack);
        }
        Ok(())
    }

    pub fn remove_heap(self) -> Result<()> {
        self.mem
            .borrow_mut()
            .heap
            .remove(&self.id)
            .map(|_| ())
            .with_context(|| anyhow!("Failed to find scope for {self:?}"))
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

        fn get_reachable(
            mem: Ref<Mem>,
            id: ScopeId,
            reachable: &mut FxHashSet<ScopeId>,
        ) -> Result<()> {
            reachable.insert(id);
            let mut next_id = Some(id);
            while let Some(id) = next_id {
                let Some(scope) = mem.heap.get(&id) else {
                    bail!("Failed to find scope for {id:?}");
                };
                let parent_scopes = scope.defs.values().flat_map(Item::parent_scope);
                for parent_scope in parent_scopes {
                    if !reachable.contains(&parent_scope.id) {
                        get_reachable(Ref::clone(&mem), parent_scope.id, reachable)?;
                    }
                }
                next_id = scope.parent;
            }
            Ok(())
        }

        let mut reachable = Default::default();
        let mem = self.mem.borrow();
        for &id in mem.stack.iter() {
            get_reachable(Ref::clone(&mem), id, &mut reachable)?;
        }

        self.mem
            .borrow_mut()
            .heap
            .retain(|id, _| reachable.contains(id));
        Ok(())
    }
}

impl fmt::Debug for ScopeHandle<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id.0)
    }
}
