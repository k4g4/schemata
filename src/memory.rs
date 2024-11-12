use crate::{globals, item::Item};
use anyhow::{anyhow, bail, Result};
use rustc_hash::{FxHashMap, FxHashSet};
use std::{
    cell::{Ref, RefCell},
    fmt, mem,
};

type Defs<'src> = FxHashMap<&'src str, Item<'src>>;

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
struct ScopeId(u64);

struct Scope<'src> {
    parent: Option<ScopeId>,
    borrowed: bool,
    defs: Defs<'src>,
}

#[derive(Default)]
pub struct Mem<'src> {
    heap: FxHashMap<ScopeId, Scope<'src>>,
    stack: Vec<ScopeId>,
    id_counter: u64,
    gc_counter: u64,
    defs_bin: Vec<Defs<'src>>,
}

impl fmt::Debug for Mem<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Stack: {:?}", self.stack)?;
        writeln!(f, "Heap: ({} scopes)", self.heap.len())?;
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
pub struct ScopeRef<'src> {
    mem: &'src RefCell<Mem<'src>>,
    id: ScopeId,
}

impl<'src> ScopeRef<'src> {
    pub fn new_global(mem: &'src RefCell<Mem<'src>>) -> Self {
        println!("{}", mem::size_of::<Item>());
        let id = {
            let mut mem = mem.borrow_mut();
            let id = ScopeId(mem.id_counter);
            mem.id_counter += 1;
            mem.heap.insert(
                id,
                Scope {
                    parent: None,
                    borrowed: false,
                    defs: Default::default(),
                },
            );
            id
        };
        Self { mem, id }
    }

    pub fn new_local(self) -> Result<Self> {
        let mut mem = self.mem.borrow_mut();
        let id = ScopeId(mem.id_counter);
        mem.id_counter += 1;
        let defs = mem.defs_bin.pop().unwrap_or_default();
        mem.heap.insert(
            id,
            Scope {
                parent: Some(self.id),
                borrowed: false,
                defs,
            },
        );
        if let Some(scope) = mem.heap.get_mut(&self.id) {
            scope.borrowed = true;
            Ok(Self { mem: self.mem, id })
        } else {
            Err(anyhow!("Failed to find scope for {self:?}"))
        }
    }

    pub fn add(self, ident: &'src str, item: Item<'src>) -> Result<()> {
        let mut mem = self.mem.borrow_mut();
        let Some(scope) = mem.heap.get_mut(&self.id) else {
            bail!("Failed to find scope for {self:?}");
        };
        // Redefinitions are only permitted in the global scope
        if scope.defs.insert(ident, item).is_some() && scope.parent.is_some() {
            Err(anyhow!("Duplicate definition for '{ident}'"))
        } else {
            Ok(())
        }
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

    pub fn push_stack(self) {
        self.mem.borrow_mut().stack.push(self.id);
    }

    pub fn pop_stack(self) -> Result<()> {
        let mut mem = self.mem.borrow_mut();
        loop {
            let Some(id) = mem.stack.pop() else {
                bail!("Failed to find {self:?} in stack");
            };
            if id == self.id {
                break Ok(());
            }
        }
    }

    pub fn remove_heap(self) -> Result<()> {
        let mut mem = self.mem.borrow_mut();
        let Mem { heap, defs_bin, .. } = &mut *mem;
        let Some(mut scope) = heap.remove(&self.id) else {
            bail!("Failed to find scope for {self:?}");
        };
        if scope.borrowed {
            heap.insert(self.id, scope);
        } else if scope.defs.capacity() > 0 {
            scope.defs.clear();
            defs_bin.push(mem::take(&mut scope.defs));
        }
        Ok(())
    }

    pub fn collect_garbage(self) -> Result<()> {
        // Run garbage collection based on the configured frequency
        {
            let mut mem = self.mem.borrow_mut();
            let freq = globals::gc_freq();
            if freq == 0 {
                return Ok(());
            }
            mem.gc_counter += 1;
            mem.gc_counter %= freq;
            if mem.gc_counter % freq != freq - 1 {
                return Ok(());
            }
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
                    return Ok(());
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

        let reachable = {
            let mut reachable = Default::default();
            let mem = self.mem.borrow();
            for &id in mem.stack.iter() {
                get_reachable(Ref::clone(&mem), id, &mut reachable)?;
            }
            reachable
        };

        let mut mem = self.mem.borrow_mut();
        let Mem { heap, defs_bin, .. } = &mut *mem;
        heap.retain(|id, Scope { defs, .. }| {
            let retain = reachable.contains(id);
            if !retain && defs.capacity() > 0 {
                defs.clear();
                defs_bin.push(mem::take(defs));
            }
            retain
        });
        Ok(())
    }
}

impl fmt::Debug for ScopeRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id.0)
    }
}
