use crate::{
    error::Error,
    items::{Item, Syn},
    scope::Scope,
};
use nom::{
    branch::alt,
    bytes::complete::take_till1,
    character::complete::{char, multispace0, one_of},
    combinator::{into, map_res, not},
    error::context,
    multi::many0,
    number::complete::double,
    sequence::{delimited, preceded},
    Parser,
};
use smartstring::alias::String;
use std::{fmt::Write, str};

type I = [u8];

type ParseRes<'i, O> = nom::IResult<&'i I, O, Error>;

pub fn repl(mut input: &I) -> Result<(), Error> {
    let mut scope = Scope::new(None);
    let mut display = String::new();

    while !input.is_empty() {
        (input, scope) = match expr(input) {
            Ok((input, syn)) => {
                println!("Expression:");
                display.clear();
                write!(&mut display, "{syn}").map_err(|_| Error::Unexpected)?;
                println!("{display}");

                match syn.eval(scope) {
                    Ok((Item::Func(..), _)) => {
                        return Err(Error::Other("Cannot display a function".into()))
                    }
                    Ok((item, scope)) => {
                        println!("Evaluated:");
                        println!("{item}");

                        (input, scope)
                    }
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
            }
            Err(nom::Err::Error(error) | nom::Err::Failure(error)) => return Err(error),
            Err(nom::Err::Incomplete(_)) => return Err(Error::Unexpected),
        }
    }

    Ok(())
}

fn expr(input: &I) -> ParseRes<Syn> {
    let syns = (
        double.map(Syn::Num),
        context("identifier", ident).map(Syn::Ident),
        context("list", list).map(Syn::List),
    );
    delimited(multispace0, alt(syns), multispace0)(input)
}

fn ident(input: &I) -> ParseRes<String> {
    let delims = "();\"'`|[]{}";
    let ident = preceded(
        not(one_of::<_, _, Error>("#,").or(one_of(delims))),
        take_till1(|c| delims.as_bytes().contains(&c)),
    );
    into(map_res(ident, str::from_utf8))(input)
}

fn list(input: &I) -> ParseRes<Vec<Syn>> {
    delimited(char('('), many0(expr), char(')'))(input)
}
