use crate::{
    error::ParserError,
    globals, idents,
    scope::Scope,
    syn::{Defs, Reserved, Syn},
};
use anyhow::{anyhow, Result};
use core::str;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till1, take_until},
    character::complete::{char, multispace0, not_line_ending, one_of},
    combinator::{all_consuming, map, map_res, not, value},
    error::context,
    multi::{many0, many0_count},
    number::complete::double,
    sequence::{delimited, pair, preceded},
};
use std::borrow::Cow;

type I = [u8];
type ParseRes<'i, O> = nom::IResult<&'i I, O, ParserError<&'i I>>;

pub fn repl(prelude: &I, input: &I) -> Result<()> {
    let scope = Scope::new_global();
    let pretty = globals::pretty();

    let prelude_syns = read(syns)(prelude)?;
    let input_syns = read(syns)(input)?;

    for syn in &prelude_syns {
        syn.eval(&scope, Defs::Allowed)?;
    }

    for syn in &input_syns {
        println!("Expression:");
        if pretty {
            println!("{syn:#}");
        } else {
            println!("{syn}");
        }
        println!();

        let item = syn.eval(&scope, Defs::Allowed)?;

        println!("Evaluated:");
        if pretty {
            println!("{item:+#}");
        } else {
            println!("{item:+}");
        }
        println!();
    }

    Ok(())
}

fn read<'i, O>(parser: impl FnMut(&'i I) -> ParseRes<O>) -> impl FnOnce(&'i I) -> Result<O> {
    move |i| match all_consuming(parser)(i) {
        Ok((_, out)) => Ok(out),
        Err(nom::Err::Error(error) | nom::Err::Failure(error)) => Err(anyhow!("{error}")),
        Err(nom::Err::Incomplete(_)) => Err(anyhow!("Incomplete input")),
    }
}

fn comment(i: &I) -> ParseRes<&I> {
    preceded(tag(";;"), not_line_ending)(i)
}

fn ignore(i: &I) -> ParseRes<()> {
    value(
        (),
        preceded(multispace0, many0_count(pair(comment, multispace0))),
    )(i)
}

fn syns(i: &I) -> ParseRes<Vec<Syn>> {
    many0(delimited(ignore, syn, ignore))(i)
}

fn syn(i: &I) -> ParseRes<Syn> {
    alt((
        context("reserved", map(reserved, Syn::Reserved)),
        context("number", map(double, Syn::Num)),
        context("token", map(token, Syn::Ident)),
        context("identifier", map(ident, Syn::Ident)),
        context("string", map(string, Syn::String)),
        context("s-expression", map(s_expr, Syn::SExpr)),
    ))(i)
}

fn reserved(i: &I) -> ParseRes<Reserved> {
    alt((
        value(Reserved::Define, tag(idents::DEFINE)),
        value(Reserved::Lambda, tag(idents::LAMBDA)),
        value(Reserved::Let, tag(idents::LET)),
        value(Reserved::Cond, tag(idents::COND)),
        value(Reserved::Else, tag(idents::ELSE)),
        value(Reserved::If, tag(idents::IF)),
        value(Reserved::And, tag(idents::AND)),
        value(Reserved::Or, tag(idents::OR)),
    ))(i)
}

fn token(i: &I) -> ParseRes<&str> {
    map_res(
        alt((tag(idents::TRUE), tag(idents::FALSE), tag(idents::VOID))),
        str::from_utf8,
    )(i)
}

fn ident(i: &I) -> ParseRes<&str> {
    let delim = "();\"'`|[]{} \r\t\n";
    map_res(
        preceded(
            not(alt((one_of("#,"), one_of(delim)))),
            take_till1(|b| delim.as_bytes().contains(&b)),
        ),
        str::from_utf8,
    )(i)
}

fn string(i: &I) -> ParseRes<Cow<'_, str>> {
    map(
        delimited(char('"'), take_until("\""), char('"')),
        String::from_utf8_lossy,
    )(i)
}

fn s_expr(i: &I) -> ParseRes<Vec<Syn>> {
    delimited(char('('), syns, char(')'))(i)
}
