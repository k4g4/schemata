use crate::{
    error::ParserError,
    idents,
    scope::Scope,
    syn::{Defs, Reserved, Syn},
};
use anyhow::Result;
use nom::{
    branch::{alt, Alt},
    bytes::complete::{tag, take_till1, take_until},
    character::complete::{char, multispace0, one_of},
    combinator::{all_consuming, cut, map_res, not, peek, value},
    error::context,
    multi::{many0, many0_count},
    number::complete::double,
    sequence::{delimited, pair, preceded, terminated},
    Parser,
};
use std::str;

type I = [u8];

type ParseRes<'src, O> = nom::IResult<&'src I, O, ParserError>;

const DELIMS: &[u8] = b"();\"'`|[]{} \r\t\n";

pub fn repl(prelude: &I, input: &I) -> Result<()> {
    let scope = Scope::new_global();

    let prelude_syns = read(syns)(prelude)?;
    let input_syns = read(syns)(input)?;

    for syn in &prelude_syns {
        syn.eval(&scope, Defs::Allowed)?;
    }

    for syn in &input_syns {
        println!("Expression:");
        println!("{syn}");
        println!();

        let item = syn.eval(&scope, Defs::Allowed)?;

        println!("Evaluated:");
        println!("{item}");
        println!();
    }

    Ok(())
}

fn read<'a, O>(mut parser: impl FnMut(&'a I) -> ParseRes<O>) -> impl FnMut(&'a I) -> Result<O> {
    move |input| match parser(input) {
        Ok((_, out)) => Ok(out),
        Err(nom::Err::Error(error) | nom::Err::Failure(error)) => Err(error.into()),
        Err(nom::Err::Incomplete(_)) => Err(ParserError::Unexpected.into()),
    }
}

fn syns(input: &I) -> ParseRes<Vec<Syn>> {
    all_consuming(many0(syn))(input)
}

fn comment(input: &I) -> ParseRes<()> {
    value((), tag(";;").and(take_until("\n")).and(tag("\n")))(input)
}

fn ignore(input: &I) -> ParseRes<()> {
    value((), multispace0.and(many0_count(comment.and(multispace0))))(input)
}

fn syn(input: &I) -> ParseRes<Syn> {
    let syn_parsers = (
        reserved.map(Syn::Reserved),
        double.map(Syn::Num),
        token.map(Syn::Ident),
        context("identifier", ident).map(Syn::Ident),
        context("list", list).map(Syn::Group),
    );
    delimited(ignore, alt(syn_parsers), ignore)(input)
}

fn any_delim<'i, O>(
    mut list: impl Alt<&'i I, O, ParserError>,
) -> impl FnMut(&'i I) -> ParseRes<'i, O> {
    move |input| terminated(|i| list.choice(i), peek(one_of(DELIMS)))(input)
}

fn reserved(input: &I) -> ParseRes<Reserved> {
    any_delim((
        value(Reserved::Define, tag(idents::DEFINE)),
        value(Reserved::Lambda, tag(idents::LAMBDA)),
        value(Reserved::Cond, tag(idents::COND)),
        value(Reserved::Else, tag(idents::ELSE)),
        value(Reserved::If, tag(idents::IF)),
        value(Reserved::And, tag(idents::AND)),
        value(Reserved::Or, tag(idents::OR)),
    ))(input)
}

fn token(input: &I) -> ParseRes<&str> {
    map_res(
        any_delim((tag(idents::TRUE), tag(idents::FALSE))),
        str::from_utf8,
    )(input)
}

fn ident(input: &I) -> ParseRes<&str> {
    map_res(
        preceded(
            not(one_of::<_, _, ParserError>("#,").or(one_of(DELIMS))),
            take_till1(|c| DELIMS.contains(&c)),
        ),
        str::from_utf8,
    )(input)
}

fn list(input: &I) -> ParseRes<Vec<Syn>> {
    delimited(
        pair(char('('), ignore),
        many0(syn),
        cut(pair(ignore, char(')'))),
    )(input)
}
