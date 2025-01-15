use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag},
    character::complete::{self, multispace0, one_of},
    combinator::{map, value},
    error::ParseError,
    multi::separated_list0,
    sequence::{delimited, pair, preceded, tuple},
    IResult, Parser,
};

fn spaced<'a, O, E, P>(parser: P) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    preceded(multispace0, parser)
}

fn spaced_tag<'a, E>(pat: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    spaced(tag(pat))
}

fn curlied<'a, O, E, P>(parser: P) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    delimited(spaced_tag("{"), parser, spaced_tag("}"))
}

fn squared<'a, O, E, P>(parser: P) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    delimited(spaced_tag("["), parser, spaced_tag("]"))
}

fn parend<'a, O, E, P>(parser: P) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    delimited(spaced_tag("("), parser, spaced_tag(")"))
}

fn struct_field<'a, O, E, P>(
    name: &'a str,
    value: P,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    delimited(pair(spaced_tag(name), tag(":")), value, spaced_tag(","))
}

fn list<'a, O, E, P>(item: P) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    squared(separated_list0(spaced_tag(","), item))
}

fn option<'a, O, E, P>(item: P) -> impl FnMut(&'a str) -> IResult<&'a str, Option<O>, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    alt((
        map(spaced_tag("None"), |_| None),
        map(preceded(spaced_tag("Some"), parend(item)), Some),
    ))
}

#[derive(Debug, Clone)]
struct Crate<'a> {
    attrs: Vec<Attribute<'a>>,
    items: Vec<Item<'a>>,
}

impl<'a> Crate<'a> {
    fn new(attrs: Vec<Attribute<'a>>, items: Vec<Item<'a>>) -> Self {
        Self { attrs, items }
    }
}

fn krate(input: &str) -> IResult<&str, Crate> {
    map(
        preceded(
            spaced_tag("Crate"),
            curlied(tuple((
                struct_field("attrs", list(attribute)),
                struct_field("items", list(item)),
                struct_field("spans", modspans),
                struct_field("id", node_id),
                struct_field("is_placeholder", parse_bool),
            ))),
        ),
        |(attrs, items, _, _, _)| Crate::new(attrs, items),
    )(input)
}

fn node_id(input: &str) -> IResult<&str, u32> {
    preceded(spaced_tag("NodeId"), parend(complete::u32))(input)
}

fn parse_bool(input: &str) -> IResult<&str, bool> {
    alt((
        value(true, spaced_tag("true")),
        value(false, spaced_tag("false")),
    ))(input)
}

#[derive(Debug, Clone)]
struct Attribute<'a> {
    kind: AttrKind<'a>,
    span: Span<'a>,
}

impl<'a> Attribute<'a> {
    fn new(kind: AttrKind<'a>, span: Span<'a>) -> Self {
        Self { kind, span }
    }
}

fn attribute(input: &str) -> IResult<&str, Attribute> {
    map(
        preceded(
            spaced_tag("Attribute"),
            curlied(tuple((
                struct_field("kind", attr_kind),
                struct_field("id", attr_id),
                struct_field("style", attr_style),
                struct_field("span", span),
            ))),
        ),
        |(kind, _, _, span)| Attribute::new(kind, span),
    )(input)
}

fn attr_id(input: &str) -> IResult<&str, u32> {
    preceded(spaced_tag("NodeId"), parend(complete::u32))(input)
}

fn attr_style(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("Outer"), spaced_tag("Inner")))(input)
}

#[derive(Debug, Clone)]
enum AttrKind<'a> {
    Normal,
    DocComment(CommentKind, &'a str),
}

fn attr_kind(input: &str) -> IResult<&str, AttrKind> {
    alt((
        value(
            AttrKind::Normal,
            preceded(spaced_tag("Normal"), parend(normal_attr)),
        ),
        map(
            preceded(spaced_tag("DocComment"), tuple((comment_kind, string))),
            |(kind, symbol)| AttrKind::DocComment(kind, symbol),
        ),
    ))
}

fn normal_attr(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("NormalAttr"),
            curlied(tuple((
                struct_field("item", attr_item),
                struct_field("tokens", option(tokens)),
            ))),
        ),
    )(input)
}

fn attr_item(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("AttrItem"),
            curlied(tuple((
                struct_field("safety", safety),
                struct_field("path", path),
                struct_field("args", attr_args),
                struct_field("tokens", option(tokens)),
            ))),
        ),
    )(input)
}

fn safety(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Unsafe"), parend(span))),
        value((), preceded(spaced_tag("Safe"), parend(span))),
        value((), spaced_tag("Default")),
    ))(input)
}

fn path(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("Path"),
            curlied(tuple((
                struct_field("span", span),
                struct_field("segments", list(path_segment)),
                struct_field("tokens", option(tokens)),
            ))),
        ),
    )(input)
}

fn path_segment(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("PathSegment"),
            curlied(tuple((
                struct_field("ident", ident),
                struct_field("id", node_id),
                struct_field("args", option(generic_args)),
            ))),
        ),
    )(input)
}

#[derive(Debug, Clone)]
enum CommentKind {
    Line,
    Block,
}

#[derive(Debug, Clone)]
struct Item<'a> {
    attrs: Vec<Attribute<'a>>,
    span: Span<'a>,
    ident: Ident<'a>,
    kind: ItemKind<'a>,
}

impl<'a> Item<'a> {
    fn new(
        attrs: Vec<Attribute<'a>>,
        span: Span<'a>,
        ident: Ident<'a>,
        kind: ItemKind<'a>,
    ) -> Self {
        Self {
            attrs,
            span,
            ident,
            kind,
        }
    }
}

fn item(input: &str) -> IResult<&str, Item> {
    map(
        preceded(
            spaced_tag("Item"),
            curlied(tuple((
                struct_field("attrs", list(attribute)),
                struct_field("id", node_id),
                struct_field("span", span),
                struct_field("vis", visibility),
                struct_field("ident", ident),
                struct_field("kind", item_kind),
                struct_field("tokens", tokens),
            ))),
        ),
        |(attrs, _, span, _, ident, kind, _)| Item::new(attrs, span, ident, kind),
    )(input)
}

#[derive(Debug, Clone)]
struct Span<'a>(&'a str);

fn spaced_string(input: &str) -> IResult<&str, &str> {
    spaced(delimited(
        tag("\""),
        escaped(is_not("\"'"), '\\', one_of("\"'\\")),
        tag("\""),
    ))(input)
}

fn span(input: &str) -> IResult<&str, Span> {
    map(spaced_string, Span)(input)
}

fn modspans(input: &str) -> IResult<&str, Span> {
    map(
        preceded(
            spaced_tag("Item"),
            curlied(tuple((
                struct_field("inner_span", span),
                struct_field("outer_span", span),
            ))),
        ),
        |(span, _)| span,
    )(input)
}

#[derive(Debug, Clone)]
struct Ident<'a>(&'a str);

fn ident(input: &str) -> IResult<&str, Ident> {
    map(spaced_string, Ident)(input)
}

#[derive(Debug, Clone)]
enum ItemKind<'a> {
    Fn(Fn<'a>),
    Mod(Mod<'a>),
    Trait(Trait<'a>),
    Impl(Impl<'a>),
}

#[derive(Debug, Clone)]
struct Fn<'a> {
    body: Option<Block<'a>>,
}

#[derive(Debug, Clone)]
struct Block<'a> {
    span: Span<'a>,
}

#[derive(Debug, Clone)]
struct Mod<'a> {
    items: Vec<Item<'a>>,
    span: Span<'a>,
}

#[derive(Debug, Clone)]
struct Trait<'a> {
    items: Vec<AssocItem<'a>>,
}

#[derive(Debug, Clone)]
struct AssocItem<'a> {
    attrs: Vec<Attribute<'a>>,
    span: Span<'a>,
    ident: Ident<'a>,
    assoc_fn: Fn<'a>,
}

#[derive(Debug, Clone)]
struct Impl<'a> {
    items: Vec<AssocItem<'a>>,
}
