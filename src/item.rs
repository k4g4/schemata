use crate::{idents, proc::Proc};
use anyhow::{bail, ensure, Error, Result};
use std::{array, borrow::Cow, f64, fmt, iter::FusedIterator, rc::Rc};

#[derive(Clone, Debug)]
pub enum Item<'src> {
    Num(f64),
    String(Cow<'src, str>),
    List(Option<Rc<List<'src>>>),
    Proc(Proc<'src>),
    Token(Token),
    Defined,
}

#[derive(Clone, Debug)]
pub struct List<'src> {
    pub head: Item<'src>,
    pub tail: Item<'src>,
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Token {
    True,
    False,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::True => write!(f, "{}", idents::TRUE),
            Self::False => write!(f, "{}", idents::FALSE),
        }
    }
}

impl<'src> Item<'src> {
    pub fn nil() -> Self {
        Self::List(None)
    }

    pub fn cons(head: Self, tail: Self) -> Self {
        Self::List(Some(Rc::new(List { head, tail })))
    }

    pub fn from_items(
        items: impl IntoIterator<IntoIter = impl DoubleEndedIterator<Item = Result<Self>>>,
    ) -> Result<Self> {
        items
            .into_iter()
            .try_rfold(Item::nil(), |tail, head| Ok(Item::cons(head?, tail)))
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Self::Token(Token::False))
    }

    fn iter(&self) -> ItemsIter<'_, 'src> {
        ItemsIter(if let Self::List(list) = self {
            list.as_deref()
        } else {
            None
        })
    }

    pub fn apply(self) -> Result<Self> {
        if let Self::List(list) = &self {
            match list.as_deref() {
                Some(List {
                    head: Self::Proc(proc),
                    tail,
                }) => proc.apply(tail.iter()),

                Some(List { head, .. }) => bail!("'{head}' is not a procedure"),

                None => bail!("'()' cannot be evaluated"),
            }
        } else {
            Ok(self)
        }
    }
}

impl fmt::Display for Item<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pretty = f.alternate();
        let width = f.width().unwrap_or(0);
        let indent = |f: &mut fmt::Formatter| {
            if pretty {
                for _ in 0..width {
                    write!(f, "   ")?;
                }
            }
            Ok(())
        };
        indent(f)?;

        match self {
            Self::Num(num) => write!(f, "{num}"),
            Self::String(string) => write!(f, "\"{string}\""),
            Self::Proc(proc) => write!(f, "{proc}"),
            Self::Token(token) => write!(f, "{token}"),
            Self::Defined => write!(f, "<{}>", idents::DEFINE),
            Self::List(list) => {
                if let Some(List { head, tail }) = list.as_deref() {
                    write!(f, "(")?;
                    if pretty {
                        writeln!(f)?;
                    }

                    let (mut head, mut tail) = (head, tail);
                    loop {
                        if pretty {
                            writeln!(f, "{head:#width$}", width = width + 1)?;
                        } else {
                            write!(f, "{head} ")?;
                        }
                        (head, tail) = match tail {
                            Self::List(Some(list)) => (&list.head, &list.tail),

                            Self::List(_) => break,

                            _ => {
                                if pretty {
                                    writeln!(f, ".")?;
                                } else {
                                    write!(f, " . ")?;
                                }
                                indent(f)?;
                                if pretty {
                                    writeln!(f, "{tail:#width$}", width = width + 1)?;
                                } else {
                                    write!(f, "{tail} ")?;
                                }
                                break;
                            }
                        };
                    }

                    indent(f)?;
                    write!(f, ")")
                } else {
                    write!(f, "()")
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct ItemsIter<'a, 'src>(Option<&'a List<'src>>);

impl<'a, 'src> Iterator for ItemsIter<'a, 'src> {
    type Item = &'a Item<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.map(|List { head, tail }| {
            if let Item::List(list) = tail {
                self.0 = list.as_deref();
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
