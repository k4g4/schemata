use crate::{
    error::Error,
    idents,
    scope::Scope,
    syn::{Defs, Reserved, Syn},
};
use nom::{
    branch::{alt, Alt},
    bytes::complete::{tag, take_till1, take_until},
    character::complete::{char, multispace0, one_of},
    combinator::{all_consuming, map_res, not, opt, peek, value},
    error::context,
    multi::many0,
    number::complete::double,
    sequence::{delimited, preceded, terminated},
    Parser,
};
use std::str;

type I = [u8];

type ParseRes<'src, O> = nom::IResult<&'src I, O, Error>;

const DELIMS: &[u8] = b"();\"'`|[]{} \r\t\n";

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

fn comment(input: &I) -> ParseRes<()> {
    value((), tag(";;").and(take_until("\n")).and(tag("\n")))(input)
}

fn ignore(input: &I) -> ParseRes<()> {
    delimited(multispace0, value((), opt(comment)), multispace0)(input)
}

fn expr(input: &I) -> ParseRes<Syn> {
    let syns = (
        reserved.map(Syn::Reserved),
        double.map(Syn::Num),
        token.map(Syn::Ident),
        context("identifier", ident).map(Syn::Ident),
        context("list", list).map(Syn::Group),
    );
    delimited(ignore, alt(syns), ignore)(input)
}

fn any_delim<'i, O>(mut list: impl Alt<&'i I, O, Error>) -> impl FnMut(&'i I) -> ParseRes<'i, O> {
    move |input| terminated(|i| list.choice(i), peek(one_of(DELIMS)))(input)
}

fn reserved(input: &I) -> ParseRes<Reserved> {
    any_delim((
        value(Reserved::Define, tag(idents::DEFINE)),
        value(Reserved::Cond, tag(idents::COND)),
        value(Reserved::Else, tag(idents::ELSE)),
    ))(input)
}

fn token(input: &I) -> ParseRes<&str> {
    map_res(
        any_delim((tag(idents::TRUE), tag(idents::FALSE))),
        str::from_utf8,
    )(input)
}

fn ident(input: &I) -> ParseRes<&str> {
    let ident = preceded(
        not(one_of::<_, _, Error>("#,").or(one_of(DELIMS))),
        take_till1(|c| DELIMS.contains(&c)),
    );
    map_res(ident, str::from_utf8)(input)
}

fn list(input: &I) -> ParseRes<Vec<Syn>> {
    delimited(char('(').and(ignore), many0(expr), ignore.and(char(')')))(input)
}
