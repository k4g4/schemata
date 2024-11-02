use crate::{
    error::Error,
    idents,
    scope::Scope,
    syn::{Defs, Syn},
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till1},
    character::complete::{char, multispace0, one_of},
    combinator::{all_consuming, map_res, not, value},
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
    let syns = match syns(input) {
        Ok((_, syns)) => syns,
        Err(nom::Err::Error(error) | nom::Err::Failure(error)) => return Err(error),
        Err(nom::Err::Incomplete(_)) => return Err(Error::Unexpected),
    };

    syns.iter().try_fold(Scope::new_global(), |scope, syn| {
        println!("Expression:");
        println!("{syn}");
        println!();

        let (item, scope) = syn.eval(scope, Defs::Allowed)?;

        println!("Evaluated:");
        println!("{item}");
        println!();

        Ok(scope)
    })?;

    Ok(())
}

fn syns(input: &I) -> ParseRes<Vec<Syn>> {
    all_consuming(many0(expr))(input)
}

fn expr(input: &I) -> ParseRes<Syn> {
    let syns = (
        double.map(Syn::Num),
        value(Syn::Define, tag(idents::DEFINE)),
        value(Syn::Cond, tag(idents::COND)),
        token.map(Syn::Ident),
        context("identifier", ident).map(Syn::Ident),
        context("list", list).map(Syn::Group),
    );
    delimited(multispace0, alt(syns), multispace0)(input)
}

fn token(input: &I) -> ParseRes<&str> {
    map_res(alt((tag(idents::TRUE), tag(idents::FALSE))), str::from_utf8)(input)
}

fn ident(input: &I) -> ParseRes<&str> {
    let delims = "();\"'`|[]{} ";
    let ident = preceded(
        not(one_of::<_, _, Error>("#,").or(one_of(delims))),
        take_till1(|c| delims.as_bytes().contains(&c)),
    );
    map_res(ident, str::from_utf8)(input)
}

fn list(input: &I) -> ParseRes<Vec<Syn>> {
    delimited(
        char('(').and(multispace0),
        many0(expr),
        multispace0.and(char(')')),
    )(input)
}
