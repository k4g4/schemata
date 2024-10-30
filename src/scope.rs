use crate::items::Item;
use rustc_hash::FxHashMap;
use smartstring::alias::String;
use std::rc::Rc;

#[derive(Clone, Default, Debug)]
pub struct Scope {
    parent: Option<Rc<Scope>>,
    defs: FxHashMap<String, Item>,
}

impl Scope {
    fn lookup(&self, ident: &str) -> Option<&Item> {
        self.defs
            .get(ident)
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.lookup(ident)))
    }

    fn add(mut self: Rc<Self>, ident: String, item: Item) -> Rc<Self> {
        Rc::make_mut(&mut self).defs.insert(ident, item);
        self
    }
}
