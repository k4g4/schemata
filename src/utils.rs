use crate::item::{Item, Pair};
use anyhow::{ensure, Error, Result};
use std::{array, f64, fmt, iter::FusedIterator};

pub fn indent(f: &mut fmt::Formatter, width: usize) -> fmt::Result {
    for _ in 0..width {
        write!(f, "   ")?;
    }
    Ok(())
}

#[derive(Clone, Debug)]
pub struct ItemsIter<'a, 'src>(Option<&'a Pair<'src>>);

impl<'a, 'src> ItemsIter<'a, 'src> {
    pub fn new(pair: Option<&'a Pair<'src>>) -> Self {
        Self(pair)
    }
}

impl<'a, 'src> Iterator for ItemsIter<'a, 'src> {
    type Item = &'a Item<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.map(|Pair { head, tail }| {
            if let Item::Pair(pair) = tail {
                self.0 = pair.as_deref();
            } else {
                self.0 = None;
            }
            head
        })
    }
}

impl FusedIterator for ItemsIter<'_, '_> {}

#[derive(Clone, Debug)]
pub struct NumsIter<'a, 'src>(ItemsIter<'a, 'src>);

impl<'a, 'src> TryFrom<ItemsIter<'a, 'src>> for NumsIter<'a, 'src> {
    type Error = Error;

    fn try_from(heads_iter: ItemsIter<'a, 'src>) -> Result<Self> {
        ensure!(
            heads_iter.clone().all(|item| matches!(item, Item::Num(_))),
            "cannot operate on non-number"
        );
        Ok(Self(heads_iter))
    }
}

impl<'a, 'src> Iterator for NumsIter<'a, 'src> {
    type Item = f64;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|item| {
            if let &Item::Num(n) = item {
                n
            } else {
                panic!("NumsIter invariant violated")
            }
        })
    }
}

impl FusedIterator for NumsIter<'_, '_> {}

pub trait ArgsIter<'src>: FusedIterator + Clone + Sized {
    fn get<const REQUIRED: usize, const OPTIONAL: usize>(
        &self,
        this: impl fmt::Display,
    ) -> Result<(
        [<Self as Iterator>::Item; REQUIRED],
        [Option<<Self as Iterator>::Item>; OPTIONAL],
    )> {
        ensure!(
            (REQUIRED..=REQUIRED + OPTIONAL).contains(&self.clone().count()),
            "Not enough arguments for {this} (Expected {REQUIRED})"
        );
        let mut iter = self.clone();
        let required = array::from_fn(|_| iter.next().expect("already checked"));
        let optional = array::from_fn(|_| iter.next());
        ensure!(
            iter.next().is_none(),
            "Too many arguments for {this} (Expected at most {})",
            REQUIRED + OPTIONAL
        );
        Ok((required, optional))
    }
}

impl<'src, T: FusedIterator + Clone> ArgsIter<'src> for T {}
