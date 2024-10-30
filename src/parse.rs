use crate::{
    error::Error,
    items::{Builtin, Item},
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till1},
    character::complete::{char, multispace0, one_of},
    combinator::{into, map_res, not, value},
    error::context,
    multi::many0,
    number::complete::double,
    sequence::{delimited, preceded},
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
                match item.eval(&mut Default::default()) {
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
    let items = (
        value(Item::Define, tag("define")),
        double.map(Item::Num),
        context("identifier", ident).map(Item::Ident),
        context("builtin", builtin).map(Item::Builtin),
        context("list", list).map(Item::List),
    );
    delimited(multispace0, alt(items), multispace0)(input)
}

fn ident(input: &I) -> ParseRes<String> {
    let delims = "();\"'`|[]{}";
    let ident = preceded(
        not(one_of::<_, _, Error>("#,").or(one_of(delims))),
        take_till1(|c| delims.as_bytes().contains(&c)),
    );
    map_res(into(ident), String::from_utf8)(input)
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
