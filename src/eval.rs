use crate::error::Error;
use nom::{
    branch::alt,
    character::complete::{char, digit1},
    combinator::opt,
    Parser,
};
use std::fmt;

type I = [u8];

type ParseRes<'i, O> = nom::IResult<&'i I, O, Error>;

fn other<O>() -> ParseRes<'static, O> {
    Err(nom::Err::Error(Error::Other))
}

enum Item {
    Nil,
    Num(i64),
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => Ok(()),
            Self::Num(num) => writeln!(f, "{num}"),
        }
    }
}

pub fn eval(mut input: &I) -> Result<(), Error> {
    while !input.is_empty() {
        match expr(input) {
            Ok((new_input, item)) => {
                input = new_input;
                print!("{item}");
            }
            Err(nom::Err::Error(error) | nom::Err::Failure(error)) => return Err(error),
            _ => return Err(Error::Other),
        }
    }

    Ok(())
}

fn expr(input: &I) -> ParseRes<Item> {
    alt((nil.map(|_| Item::Nil), num.map(Item::Num)))(input)
}

fn nil(_: &I) -> ParseRes<()> {
    other()
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
        .rev()
        .fold(0, |acc, &digit| acc * 10 + (digit - 0x30) as u64);
    Ok((input, num))
}
