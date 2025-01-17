use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag},
    character::complete,
    combinator::{cut, map, opt, value},
    error::ParseError,
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    IResult, Parser,
};

pub(crate) fn spaced<'a, O, E, P>(parser: P) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    preceded(complete::multispace0, parser)
}

pub(crate) fn spaced_tag<'a, E>(pat: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    spaced(tag(pat))
}

pub(crate) fn curlied<'a, O, E, P>(parser: P) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    delimited(spaced_tag("{"), parser, spaced_tag("}"))
}

pub(crate) fn squared<'a, O, E, P>(parser: P) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    delimited(spaced_tag("["), parser, spaced_tag("]"))
}

pub(crate) fn parend<'a, O, E, P>(parser: P) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    delimited(spaced_tag("("), parser, spaced_tag(")"))
}

pub(crate) fn list<'a, O, E, P>(item: P) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    squared(terminated(separated_list0(tag(","), item), opt(tag(","))))
}

pub(crate) fn hashmap<'a, O1, O2, E, P1, P2>(
    key: P1,
    value: P2,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<(O1, O2)>, E>
where
    E: ParseError<&'a str>,
    P1: Parser<&'a str, O1, E>,
    P2: Parser<&'a str, O2, E>,
{
    curlied(many0(separated_pair(key, spaced_tag(":"), value)))
}

pub(crate) fn option<'a, O, E, P>(item: P) -> impl FnMut(&'a str) -> IResult<&'a str, Option<O>, E>
where
    O: Clone,
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    alt((
        value(None, spaced_tag("None")),
        tuple_struct_parser("Some", field(item), Some),
    ))
}

pub(crate) fn result<'a, O1, O2, E, P1, P2>(
    ok: P1,
    err: P2,
) -> impl FnMut(&'a str) -> IResult<&'a str, Result<O1, O2>, E>
where
    E: ParseError<&'a str>,
    P1: Parser<&'a str, O1, E>,
    P2: Parser<&'a str, O2, E>,
{
    alt((
        tuple_struct_parser("Ok", field(ok), Ok),
        tuple_struct_parser("Err", field(err), Err),
    ))
}

pub(crate) fn unit(input: &str) -> IResult<&str, ()> {
    value((), spaced_tag("()"))(input)
}

pub(crate) fn parser_todo(input: &str) -> IResult<&str, ()> {
    todo!(
        "this parser has not been implemented. tried to parse: {}",
        input
    )
}

pub(crate) fn struct_field<'a, O, E, P>(
    name: &'a str,
    value: P,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    preceded(pair(spaced_tag(name), tag(":")), cut(field(value)))
}

pub(crate) fn struct_parser<'a, O, O2, E, P>(
    name: &'a str,
    fields: P,
    transform: fn(O) -> O2,
) -> impl FnMut(&'a str) -> IResult<&'a str, O2, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    map(preceded(spaced_tag(name), cut(curlied(fields))), transform)
}

pub(crate) fn field<'a, O, E, P>(value: P) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    terminated(cut(value), opt(tag(",")))
}

pub(crate) fn tuple_struct_parser<'a, O, O2, E, P>(
    name: &'a str,
    fields: P,
    transform: fn(O) -> O2,
) -> impl FnMut(&'a str) -> IResult<&'a str, O2, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    map(preceded(spaced_tag(name), parend(cut(fields))), transform)
}

pub(crate) fn tuple_parser<'a, O, E, P>(fields: P) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    parend(fields)
}

pub(crate) fn unit_struct_parser<'a, T: Clone, E: ParseError<&'a str>>(
    name: &'a str,
    val: T,
) -> impl FnMut(&'a str) -> IResult<&'a str, T, E> {
    value(val, spaced_tag(name))
}

pub(crate) fn discard<T>(_: T) {}

pub(crate) fn id<T>(x: T) -> T {
    x
}

pub(crate) fn spaced_string(input: &str) -> IResult<&str, &str> {
    spaced(delimited(
        tag("\""),
        map(
            opt(escaped(is_not("\"\\"), '\\', complete::one_of("\"'"))),
            |x| x.unwrap_or(""),
        ),
        tag("\""),
    ))(input)
}

pub(crate) fn parse_bool(input: &str) -> IResult<&str, bool> {
    alt((
        value(true, spaced_tag("true")),
        value(false, spaced_tag("false")),
    ))(input)
}
