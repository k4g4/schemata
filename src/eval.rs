use crate::error::Error;
use nom::{
    branch::alt,
    character::complete::{char, digit1, multispace0},
    combinator::opt,
    multi::many_till,
    sequence::delimited,
    Parser,
};
use std::{collections::VecDeque, fmt, mem};

type I = [u8];

type ParseRes<'i, O> = nom::IResult<&'i I, O, Error>;

fn other<O>() -> ParseRes<'static, O> {
    Err(nom::Err::Error(Error::Other))
}

#[derive(Clone, Debug)]
enum Item {
    Num(i64),
    List(VecDeque<Item>),
}

impl Item {
    fn eval(self) -> Self {
        match self {
            Self::List(mut list) => {
                if let Some(car) = list.pop_front() {
                    let car = car.eval();
                    let dummy = Self::Num(0);
                    for item in &mut list {
                        *item = mem::replace(item, dummy.clone()).eval();
                    }
                    if matches!(car, Self::Num(1)) {
                        list.make_contiguous().reverse();
                    }
                }
                Self::List(list)
            }
            _ => self,
        }
    }
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let width = f.width().unwrap_or(0);
        let indent = |f: &mut fmt::Formatter| {
            for _ in 0..width {
                f.write_str("   ")?;
            }
            Ok(())
        };
        indent(f)?;

        match self {
            Self::List(items) if items.is_empty() => {
                writeln!(f, "()")
            }
            Self::List(items) => {
                writeln!(f, "(")?;
                for item in items {
                    write!(f, "{item:width$}", width = width + 1)?;
                }
                indent(f)?;
                writeln!(f, ")")
            }
            Self::Num(num) => writeln!(f, "{num}"),
        }
    }
}

pub fn eval(mut input: &I) -> Result<(), Error> {
    while !input.is_empty() {
        match expr(input) {
            Ok((new_input, item)) => {
                input = new_input;
                println!("Before eval:");
                println!("{item}");
                println!("After eval:");
                println!("{}", item.eval());
            }
            Err(nom::Err::Error(error) | nom::Err::Failure(error)) => return Err(error),
            _ => return Err(Error::Other),
        }
    }

    Ok(())
}

fn expr(input: &I) -> ParseRes<Item> {
    let item = alt((list.map(Item::List), num.map(Item::Num)));
    let (input, item) = delimited(multispace0, item, multispace0)(input)?;
    Ok((input, item))
}

fn num(input: &I) -> ParseRes<i64> {
    let (input, opt) = opt(char('-'))(input)?;
    let (input, unum) = unum(input)?;
    if unum > i64::MAX as u64 {
        return other();
    }
    let sign = opt.map_or(1, |_| -1);
    Ok((input, sign * (unum as i64)))
}

fn unum(input: &I) -> ParseRes<u64> {
    let (input, digits) = digit1(input)?;
    let num = digits
        .iter()
        .fold(0, |acc, &digit| acc * 10 + (digit - 0x30) as u64);
    Ok((input, num))
}

fn list(input: &I) -> ParseRes<VecDeque<Item>> {
    let (input, _) = char('(')(input)?;
    let (input, (list, _)) = many_till(expr, char(')'))(input)?;
    Ok((input, list.into()))
}
