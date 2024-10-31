use crate::item::Item;
use std::{fmt, rc::Rc};

#[derive(Clone)]
pub struct List<'src>(Option<Rc<Node<'src>>>);

#[derive(Debug)]
pub struct Node<'src> {
    pub head: Item<'src>,
    pub tail: List<'src>,
}

pub struct ListIter<'a, 'src>(Option<&'a Node<'src>>);

impl<'a, 'src> Iterator for ListIter<'a, 'src> {
    type Item = &'a Item<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.map(|Node { head, tail }| {
            *self = tail.iter();
            head
        })
    }
}

impl<'src> List<'src> {
    pub fn nil() -> Self {
        Self(None)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_none()
    }

    pub fn as_ref(&self) -> Option<&Node<'src>> {
        self.0.as_deref()
    }

    pub fn iter(&self) -> ListIter<'_, 'src> {
        ListIter(self.as_ref())
    }
}

impl fmt::Debug for List<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}
