use crate::{
    error::Error,
    items::{Builtin, Item},
};
use nom::{
    branch::alt,
    character::complete::{char, multispace0},
    combinator::value,
    error::context,
    multi::many0,
    number::complete::double,
    sequence::delimited,
    Parser,
};
use std::fmt::Write;

type I = [u8];

type ParseRes<'i, O> = nom::IResult<&'i I, O, Error>;

pub fn repl(mut input: &I) -> Result<(), Error> {
    let mut display = String::new();
    while !input.is_empty() {
        input = match expr(input) {
            Ok((input, item)) => {
                println!("Expression:");
                display.clear();
                write!(&mut display, "{item}").map_err(|_| Error::Unexpected)?;
                println!("{display}");

                println!("Evaluated:");
                match item.eval() {
                    Ok(item) => println!("{item}"),
                    Err(error) => {
                        return Err(Error::WithContext {
                            ctx: format!(
                                "While evaluating:\n   {}\n",
                                display.trim().replace("\n", "\n   ")
                            ),
                            error: Box::new(error),
                        })
                    }
                }
                input
            }
            Err(nom::Err::Error(error) | nom::Err::Failure(error)) => return Err(error),
            Err(nom::Err::Incomplete(_)) => return Err(Error::Unexpected),
        }
    }

    Ok(())
}

fn expr(input: &I) -> ParseRes<Item> {
    let item = alt((
        context("list", list).map(Item::List),
        context("builtin", builtin).map(Item::Builtin),
        context("number", double).map(Item::Num),
    ));
    let (input, item) = delimited(multispace0, item, multispace0)(input)?;
    Ok((input, item))
}

fn builtin(input: &I) -> ParseRes<Builtin> {
    value(Builtin::Add, char('+'))
        .or(value(Builtin::Sub, char('-')))
        .or(value(Builtin::Mul, char('*')))
        .or(value(Builtin::Div, char('/')))
        .parse(input)
}

fn list(input: &I) -> ParseRes<Vec<Item>> {
    delimited(char('('), many0(expr), char(')'))(input)
}
