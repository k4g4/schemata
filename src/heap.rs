use crate::item::Item;
use rustc_hash::FxHashMap;
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
}
