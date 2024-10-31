use crate::{error::Error, scope::Scope, syn::Syn};
use nom::{
    branch::alt,
    bytes::complete::take_till1,
    character::complete::{char, multispace0, one_of},
    combinator::{all_consuming, map_res, not},
    error::context,
    multi::many0,
    number::complete::double,
    sequence::{delimited, preceded},
    Parser,
};
use std::str;

type I = [u8];

type ParseRes<'src, O> = nom::IResult<&'src I, O, Error>;

pub fn repl(input: &I) -> Result<(), Error> {
    let mut scope = Scope::new_global();

    let syns = match syns(input) {
        Ok((_, syns)) => syns,
        Err(nom::Err::Error(error) | nom::Err::Failure(error)) => return Err(error),
        Err(nom::Err::Incomplete(_)) => return Err(Error::Unexpected),
    };

    for syn in &syns {
        println!("Expression:");
        println!("{syn}");

        let (item, new_scope) = syn.interpret(&scope)?.eval(scope)?;
        scope = new_scope;

        println!("Evaluated:");
        println!("{item}");
    }

    Ok(())
}

fn syns(input: &I) -> ParseRes<Vec<Syn>> {
    all_consuming(many0(expr))(input)
}

fn expr(input: &I) -> ParseRes<Syn> {
    let syns = (
        double.map(Syn::Num),
        context("identifier", ident).map(Syn::Ident),
        context("list", list).map(Syn::List),
    );
    delimited(multispace0, alt(syns), multispace0)(input)
}

fn ident(input: &I) -> ParseRes<&str> {
    let delims = "();\"'`|[]{}";
    let ident = preceded(
        not(one_of::<_, _, Error>("#,").or(one_of(delims))),
        take_till1(|c| delims.as_bytes().contains(&c)),
    );
    map_res(ident, str::from_utf8)(input)
}

fn list(input: &I) -> ParseRes<Vec<Syn>> {
    delimited(char('('), many0(expr), char(')'))(input)
}
