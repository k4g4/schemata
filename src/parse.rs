use crate::{
    error::ParserError,
    globals, idents,
    item::{Item, Token},
    memory::{Mem, ScopeRef},
    sexpr::SExpr,
    syn::{Defs, Policy, Reserved, Syn},
};
use anyhow::{anyhow, Result};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till1},
    character::complete::{char, multispace0, none_of, not_line_ending, one_of},
    combinator::{all_consuming, into, map, map_res, not, recognize, value, verify},
    error::context,
    multi::{many0, many0_count},
    number::complete::double,
    sequence::{delimited, pair, preceded, terminated},
};
use std::{cell::RefCell, rc::Rc, str};

type I = [u8];
type ParseRes<'src, O> = nom::IResult<&'src I, O, ParserError<&'src I>>;

pub fn repl(prelude: &I, input: &I) -> Result<()> {
    let (debug, pretty) = (globals::debug(), globals::pretty());
    let mem = RefCell::new(Mem::default());
    let scope = ScopeRef::new_global(&mem);
    scope.push_stack();

    let prelude_syns = read(syns)(prelude)?;
    let input_syns = read(syns)(input)?;

    for syn in &prelude_syns {
        syn.eval(scope, Defs::Allowed, Policy::Resolve)?;
    }

    for syn in &input_syns {
        if debug {
            println!("Expression:");
            if pretty {
                println!("{syn:#}");
            } else {
                println!("{syn}");
            }
        }

        let item = syn.eval(scope, Defs::Allowed, Policy::Resolve)?;

        if debug {
            println!("Evaluated:");
        }
        if !matches!(item, Item::Token(Token::Void) | Item::Defined) {
            if pretty {
                println!("{item:#}");
            } else {
                println!("{item}");
            }
        }
    }
    scope.pop_stack()?;

    if debug {
        eprintln!();
        eprintln!("-- Memory --");
        eprintln!("{mem:?}");
    }
    Ok(())
}

fn read<'src, O>(parser: impl FnMut(&'src I) -> ParseRes<O>) -> impl FnOnce(&'src I) -> Result<O> {
    move |i| match all_consuming(parser)(i) {
        Ok((_, out)) => Ok(out),
        Err(nom::Err::Error(error) | nom::Err::Failure(error)) => Err(anyhow!("{error}")),
        Err(nom::Err::Incomplete(_)) => Err(anyhow!("Incomplete input")),
    }
}

fn comment(i: &I) -> ParseRes<&I> {
    preceded(char(';'), not_line_ending)(i)
}

fn ignore(i: &I) -> ParseRes<()> {
    value(
        (),
        pair(multispace0, many0_count(pair(comment, multispace0))),
    )(i)
}

fn syns(i: &I) -> ParseRes<Vec<Syn>> {
    preceded(ignore, many0(terminated(syn, ignore)))(i)
}

fn syn(i: &I) -> ParseRes<Syn> {
    alt((
        context("reserved", map(reserved, Syn::Reserved)),
        context("number", map(num, Syn::Num)),
        context("token", map(token, Syn::Ident)),
        context("identifier", map(ident, Syn::Ident)),
        context("string", map(string, Syn::String)),
        context("s-expression", map(s_expr, Syn::SExpr)),
        context("quoted", map(quoted, Syn::Quoted)),
    ))(i)
}

pub fn reserved(i: &I) -> ParseRes<Reserved> {
    alt((
        value(Reserved::Define, tag(idents::DEFINE)),
        value(Reserved::Lambda, tag(idents::LAMBDA)),
        value(Reserved::Let, tag(idents::LET)),
        value(Reserved::Begin, tag(idents::BEGIN)),
        value(Reserved::Cond, tag(idents::COND)),
        value(Reserved::Else, tag(idents::ELSE)),
        value(Reserved::If, tag(idents::IF)),
        value(Reserved::And, tag(idents::AND)),
        value(Reserved::Or, tag(idents::OR)),
    ))(i)
}

fn num(i: &I) -> ParseRes<f64> {
    verify(double, |n| {
        !n.is_nan() && !n.is_subnormal() && n.is_finite()
    })(i)
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
        recognize(pair(
            not(alt((one_of("#,"), one_of(delim)))),
            take_till1(|b| delim.as_bytes().contains(&b)),
        )),
        str::from_utf8,
    )(i)
}

fn string(i: &I) -> ParseRes<Rc<str>> {
    let (quote, escaped) = (r#"""#, r#"\""#);
    into(map(
        map(
            delimited(
                char::<_, ParserError<&I>>('"'),
                recognize(many0_count(alt((value('"', tag(escaped)), none_of(quote))))),
                char('"'),
            ),
            String::from_utf8_lossy,
        ),
        move |mut s| {
            if let Some(at) = s.find(escaped) {
                s.to_mut().replace_range(at..at + escaped.len(), quote);
            }
            s
        },
    ))(i)
}

fn s_expr(i: &I) -> ParseRes<SExpr> {
    map(delimited(char('('), syns, char(')')), SExpr::new)(i)
}

fn quoted(i: &I) -> ParseRes<Box<Syn>> {
    into(preceded(pair(char('\''), ignore), syn))(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EMPTY: &[u8] = b"";

    #[test]
    fn reserved_idents() {
        let reserved_idents = [
            (Reserved::Define, b"define" as &[_]),
            (Reserved::Lambda, b"lambda"),
            (Reserved::Let, b"let"),
            (Reserved::Cond, b"cond"),
            (Reserved::Else, b"else"),
            (Reserved::If, b"if"),
            (Reserved::And, b"and"),
            (Reserved::Or, b"or"),
        ];
        for (ident, input) in reserved_idents {
            assert_eq!((EMPTY, ident), reserved(input).unwrap());
        }
    }

    #[test]
    fn token_idents() {
        let token_idents = [b"#f" as &[_], b"#t", b"#!void"];
        for input in token_idents {
            assert_eq!(
                (EMPTY, str::from_utf8(input).unwrap()),
                token(input).unwrap()
            );
        }
    }

    #[test]
    fn other_idents() {
        assert!(ident(b"#should-fail").is_err());
        assert!(ident(b"(should-fail)").is_err());
        assert_eq!((EMPTY, "should-succeed"), ident(b"should-succeed").unwrap());
        assert_eq!((EMPTY, "a"), ident(b"a").unwrap());
        assert_eq!((b" second" as _, "first"), ident(b"first second").unwrap());
        assert_eq!((EMPTY, "a#b"), ident(b"a#b").unwrap());
        assert_eq!(
            (b"(second)" as _, "first"),
            ident(b"first(second)").unwrap()
        );
    }

    #[test]
    fn nums() {
        assert_eq!((EMPTY, 0.0), num(b"0").unwrap());
        assert_eq!((EMPTY, 10.0), num(b"10").unwrap());
        assert_eq!((EMPTY, -1.0), num(b"-1").unwrap());
        assert_eq!((EMPTY, 0.5), num(b".5").unwrap());
        assert_eq!((EMPTY, 2500.0), num(b"2.5e3").unwrap());
        assert!(num(b"inf").is_err());
        assert!(num(b"NaN").is_err());
    }

    #[test]
    fn strings() {
        assert_eq!((EMPTY, "".into()), string(br#""""#).unwrap());
        assert_eq!((EMPTY, "foo".into()), string(br#""foo""#).unwrap());
        assert_eq!((EMPTY, "foo bar".into()), string(br#""foo bar""#).unwrap());
        assert_eq!((b"bar" as _, "foo".into()), string(br#""foo"bar"#).unwrap());
        assert_eq!((EMPTY, "\"".into()), string(br#""\"""#).unwrap());
        assert_eq!(
            (EMPTY, "invalid: �".into()),
            string(b"\"invalid: \xff\"").unwrap()
        );
    }

    #[test]
    fn s_exprs() {
        assert_eq!((EMPTY, SExpr::new(vec![])), s_expr(b"()").unwrap());
        assert_eq!(
            (EMPTY, SExpr::new(vec![Syn::Reserved(Reserved::Define)])),
            s_expr(b"(define)").unwrap()
        );
        assert_eq!((b")" as _, SExpr::new(vec![])), s_expr(b"())").unwrap());
        assert!(s_expr(b"))").is_err());
        assert_eq!(
            (
                EMPTY,
                SExpr::new(vec![Syn::String("foo".into()), Syn::String("bar".into())])
            ),
            s_expr(br#"("foo" "bar")"#).unwrap()
        );
        assert_eq!(
            (
                EMPTY,
                SExpr::new(vec![
                    Syn::Num(42.0),
                    Syn::SExpr(SExpr::new(vec![Syn::Num(42.0)]))
                ])
            ),
            s_expr(b"(42(42))").unwrap()
        );
        assert_eq!(
            (
                EMPTY,
                SExpr::new(vec![
                    Syn::SExpr(SExpr::new(vec![Syn::Num(42.0)])),
                    Syn::SExpr(SExpr::new(vec![]))
                ])
            ),
            s_expr(b"((42) (\n) )").unwrap()
        );
        assert_eq!(
            (
                EMPTY,
                SExpr::new(vec![
                    Syn::SExpr(SExpr::new(vec![Syn::Num(43.0)])),
                    Syn::SExpr(SExpr::new(vec![]))
                ])
            ),
            s_expr(b"( (43);comment\n;; comment! \n   (\n) )").unwrap()
        );
    }

    #[test]
    fn quotes() {
        assert_eq!(
            (EMPTY, Box::new(Syn::Ident("foo"))),
            quoted(b"'foo").unwrap()
        );
        assert_eq!(
            (EMPTY, Box::new(Syn::Ident("foo"))),
            quoted(b"'  \t ;comment\n foo").unwrap()
        );
        assert!(quoted(b"foo").is_err());
        assert_eq!(
            (EMPTY, Box::new(Syn::Reserved(Reserved::Define))),
            quoted(b"'define").unwrap()
        );
        assert_eq!(
            (EMPTY, Box::new(Syn::SExpr(SExpr::new(vec![])))),
            quoted(b"'()").unwrap()
        );
        assert!(quoted(b"()").is_err());
        assert_eq!(
            (
                EMPTY,
                Box::new(Syn::SExpr(SExpr::new(vec![
                    Syn::Num(1.0),
                    Syn::SExpr(SExpr::new(vec![Syn::Num(2.0)])),
                    Syn::String("3".into())
                ])))
            ),
            quoted(br#"'(1 (2) "3")"#).unwrap()
        );
    }
}
