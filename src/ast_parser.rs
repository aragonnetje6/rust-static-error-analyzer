use nom::{
    bytes::complete::tag,
    character::complete::multispace0,
    combinator::map,
    error::ParseError,
    multi::separated_list0,
    sequence::{delimited, pair, preceded, tuple},
    IResult, Parser,
};

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
    preceded(pair(spaced_tag(name), tag(":")), value)
}

fn list<'a, O, E, P>(list_item: P) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    squared(separated_list0(spaced_tag(","), list_item))
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

#[derive(Debug, Clone)]
enum AttrKind<'a> {
    Normal,
    DocComment(CommentKind, &'a str),
}

#[derive(Debug, Clone)]
struct DocComment<'a> {
    kind: CommentKind,
    symbol: &'a str,
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

fn item(input: &str) -> IResult<&str, Attribute> {
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
        |(kind, _, _, span)| Attribute::new(kind, span),
    )(input)
}

#[derive(Debug, Clone)]
struct Span<'a>(&'a str);

#[derive(Debug, Clone)]
struct Ident<'a>(&'a str);

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
