use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag},
    character::complete,
    combinator::{map, value},
    error::ParseError,
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    IResult, Parser,
};

fn spaced<'a, O, E, P>(parser: P) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    preceded(complete::multispace0, parser)
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
    delimited(pair(spaced_tag(name), tag(":")), value, tag(","))
}

fn list<'a, O, E, P>(item: P) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    squared(separated_list0(spaced_tag(","), item))
}

fn hashmap<'a, O1, O2, E, P1, P2>(
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

fn result<'a, O1, O2, E, P1, P2>(
    ok: P1,
    err: P2,
) -> impl FnMut(&'a str) -> IResult<&'a str, Result<O1, O2>, E>
where
    E: ParseError<&'a str>,
    P1: Parser<&'a str, O1, E>,
    P2: Parser<&'a str, O2, E>,
{
    alt((
        map(preceded(spaced_tag("Ok"), parend(ok)), Ok),
        map(preceded(spaced_tag("Err"), parend(err)), Err),
    ))
}

fn unit(input: &str) -> IResult<&str, ()> {
    value((), spaced_tag("()"))(input)
}

fn parser_todo(input: &str) -> IResult<&str, ()> {
    todo!(
        "this parser has not been implemented. tried to parse: {}",
        input
    )
}

fn struct_parser<'a, O, O2, E, P>(
    name: &'a str,
    fields: P,
    transform: fn(O) -> O2,
) -> impl FnMut(&'a str) -> IResult<&'a str, O2, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    map(preceded(spaced_tag(name), curlied(fields)), transform)
}

fn discard<T>(_: T) {}

fn id<T>(x: T) -> T {
    x
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
    struct_parser(
        "Crate",
        tuple((
            struct_field("attrs", list(attribute)),
            struct_field("items", list(item)),
            struct_field("spans", modspans),
            struct_field("id", node_id),
            struct_field("is_placeholder", parse_bool),
        )),
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
    struct_parser(
        "Attribute",
        tuple((
            struct_field("kind", attr_kind),
            struct_field("id", attr_id),
            struct_field("style", attr_style),
            struct_field("span", span),
        )),
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
            preceded(
                spaced_tag("DocComment"),
                parend(separated_pair(comment_kind, spaced_tag(","), spaced_string)),
            ),
            |(kind, symbol)| AttrKind::DocComment(kind, symbol),
        ),
    ))(input)
}

fn normal_attr(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "NormalAttr",
        tuple((
            struct_field("item", attr_item),
            struct_field("tokens", option(lazy_attr_token_stream)),
        )),
        discard,
    )(input)
}

fn attr_item(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "AttrItem",
        tuple((
            struct_field("safety", safety),
            struct_field("path", path),
            struct_field("args", attr_args),
            struct_field("tokens", option(lazy_attr_token_stream)),
        )),
        discard,
    )(input)
}

fn attr_args(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("Empty")),
        value((), preceded(spaced_tag("Delimited"), parend(delim_args))),
        struct_parser(
            "Delimited",
            tuple((struct_field("eq_span", span), struct_field("expr", expr))),
            discard,
        ),
    ))(input)
}

fn safety(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Unsafe"), parend(span))),
        value((), preceded(spaced_tag("Safe"), parend(span))),
        value((), spaced_tag("Default")),
    ))(input)
}

fn path(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "Path",
        tuple((
            struct_field("span", span),
            struct_field("segments", list(path_segment)),
            struct_field("tokens", option(lazy_attr_token_stream)),
        )),
        discard,
    )(input)
}

fn path_segment(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "PathSegment",
        tuple((
            struct_field("ident", ident),
            struct_field("id", node_id),
            struct_field("args", option(generic_args)),
        )),
        discard,
    )(input)
}

fn generic_args(input: &str) -> IResult<&str, ()> {
    alt((
        value(
            (),
            preceded(spaced_tag("AngleBracketed"), parend(angle_bracketed_args)),
        ),
        value(
            (),
            preceded(spaced_tag("Parenthesized"), parend(parenthesized_args)),
        ),
        value(
            (),
            preceded(spaced_tag("ParenthesizedElided"), parend(span)),
        ),
    ))(input)
}

fn parenthesized_args(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "ParenthesizedArgs",
        tuple((
            struct_field("span", span),
            struct_field("inputs", list(ty)),
            struct_field("inputs_span", span),
            struct_field("output", fn_ret_ty),
        )),
        discard,
    )(input)
}

fn angle_bracketed_args(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "AngleBracketedArgs",
        tuple((
            struct_field("span", span),
            struct_field("args", list(angle_bracketed_arg)),
        )),
        discard,
    )(input)
}

fn angle_bracketed_arg(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Arg"), parend(generic_arg))),
        value(
            (),
            preceded(spaced_tag("Constraint"), parend(assoc_item_constraint)),
        ),
    ))(input)
}

fn assoc_item_constraint(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "AssocItemConstraint",
        tuple((
            struct_field("id", node_id),
            struct_field("ident", ident),
            struct_field("gen_args", option(generic_args)),
            struct_field("kind", assoc_item_constraint_kind),
            struct_field("span", span),
        )),
        discard,
    )(input)
}

fn generic_arg(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Lifetime"), parend(lifetime))),
        value((), preceded(spaced_tag("Type"), parend(ty))),
        value((), preceded(spaced_tag("Const"), parend(anon_const))),
    ))(input)
}

fn lifetime(input: &str) -> IResult<&str, ()> {
    value(
        (),
        separated_pair(
            complete::u32,
            spaced_tag(":"),
            pair(spaced_tag("'"), is_not(")")),
        ),
    )(input)
}

fn ty(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "Ty",
        tuple((
            struct_field("id", node_id),
            struct_field("kind", ty_kind),
            struct_field("span", span),
            struct_field("tokens", option(lazy_attr_token_stream)),
        )),
        discard,
    )(input)
}

fn ty_kind(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Slice"), parend(ty))),
        value(
            (),
            preceded(
                spaced_tag("Array"),
                parend(separated_pair(ty, spaced_tag(","), anon_const)),
            ),
        ),
        value((), preceded(spaced_tag("Ptr"), parend(mut_ty))),
        value(
            (),
            preceded(
                spaced_tag("Ref"),
                parend(separated_pair(option(lifetime), spaced_tag(","), mut_ty)),
            ),
        ),
        value(
            (),
            preceded(
                spaced_tag("PinnedRef"),
                parend(separated_pair(option(lifetime), spaced_tag(","), mut_ty)),
            ),
        ),
        value((), preceded(spaced_tag("BareFn"), parend(bare_fn_ty))),
        value(
            (),
            preceded(spaced_tag("UnsafeBinder"), parend(unsafe_binder_ty)),
        ),
        value((), spaced_tag("Never")),
        value((), preceded(spaced_tag("Tup"), parend(list(ty)))),
        value(
            (),
            preceded(
                spaced_tag("Path"),
                parend(separated_pair(option(q_self), spaced_tag(","), path)),
            ),
        ),
        value(
            (),
            preceded(
                spaced_tag("TraitObject"),
                parend(separated_pair(
                    list(generic_bound),
                    spaced_tag(","),
                    trait_object_syntax,
                )),
            ),
        ),
        value(
            (),
            preceded(
                spaced_tag("ImplTrait"),
                parend(separated_pair(node_id, spaced_tag(","), generic_bounds)),
            ),
        ),
        value((), preceded(spaced_tag("Paren"), parend(ty))),
        value((), spaced_tag("Infer")),
        value((), spaced_tag("ImplicitSelf")),
        value((), spaced_tag("CVarArgs")),
        value(
            (),
            preceded(
                spaced_tag("Pat"),
                parend(separated_pair(ty, spaced_tag(","), pat)),
            ),
        ),
        value((), spaced_tag("Dummy")),
    ))(input)
}

fn anon_const(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "AnonConst",
        tuple((struct_field("id", node_id), struct_field("value", expr))),
        discard,
    )(input)
}

fn expr(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "Expr",
        tuple((
            struct_field("id", node_id),
            struct_field("kind", expr_kind),
            struct_field("span", span),
            struct_field("attrs", list(attribute)),
            struct_field("tokens", option(lazy_attr_token_stream)),
        )),
        discard,
    )(input)
}

fn expr_kind(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Array"), parend(list(expr)))),
        value((), preceded(spaced_tag("ConstBlock"), parend(anon_const))),
        value(
            (),
            preceded(
                spaced_tag("Call"),
                parend(separated_pair(expr, spaced_tag(","), list(expr))),
            ),
        ),
        value((), preceded(spaced_tag("MethodCall"), parend(method_call))),
        value((), preceded(spaced_tag("Tup"), parend(list(expr)))),
        value(
            (),
            preceded(
                spaced_tag("Binary"),
                parend(tuple((
                    bin_op,
                    spaced_tag(","),
                    expr,
                    spaced_tag(","),
                    expr,
                ))),
            ),
        ),
        value(
            (),
            preceded(
                spaced_tag("UnOp"),
                parend(separated_pair(un_op, spaced_tag(","), expr)),
            ),
        ),
        value((), preceded(spaced_tag("Lit"), parend(lit))),
        value(
            (),
            preceded(
                spaced_tag("Cast"),
                parend(separated_pair(expr, spaced_tag(","), ty)),
            ),
        ),
        value(
            (),
            preceded(
                spaced_tag("Type"),
                parend(separated_pair(expr, spaced_tag(","), ty)),
            ),
        ),
        value(
            (),
            preceded(
                spaced_tag("Let"),
                parend(tuple((pat, spaced_tag(","), expr, spaced_tag(","), span))),
            ),
        ),
        value(
            (),
            preceded(
                spaced_tag("If"),
                parend(tuple((
                    expr,
                    spaced_tag(","),
                    block,
                    spaced_tag(","),
                    option(expr),
                ))),
            ),
        ),
        value(
            (),
            preceded(
                spaced_tag("While"),
                parend(tuple((
                    expr,
                    spaced_tag(","),
                    block,
                    spaced_tag(","),
                    option(label),
                ))),
            ),
        ),
        struct_parser(
            "ForLoop",
            tuple((
                struct_field("pat", pat),
                struct_field("iter", expr),
                struct_field("body", block),
                struct_field("label", option(label)),
                struct_field("kind", for_loop_kind),
            )),
            discard,
        ),
        value(
            (),
            preceded(
                spaced_tag("Loop"),
                parend(tuple((
                    block,
                    spaced_tag(","),
                    option(label),
                    spaced_tag(","),
                    span,
                ))),
            ),
        ),
        value(
            (),
            preceded(
                spaced_tag("Match"),
                parend(tuple((
                    expr,
                    spaced_tag(","),
                    list(arm),
                    spaced_tag(","),
                    match_kind,
                ))),
            ),
        ),
        value((), preceded(spaced_tag("Closure"), parend(closure))),
        value(
            (),
            preceded(
                spaced_tag("Block"),
                parend(separated_pair(block, spaced_tag(","), option(label))),
            ),
        ),
        value(
            (),
            preceded(
                spaced_tag("Gen"),
                parend(tuple((
                    capture_by,
                    spaced_tag(","),
                    block,
                    spaced_tag(","),
                    gen_block_kind,
                    spaced_tag(","),
                    span,
                ))),
            ),
        ),
        value(
            (),
            preceded(
                spaced_tag("Await"),
                parend(separated_pair(expr, spaced_tag(","), span)),
            ),
        ),
        alt((
            value((), preceded(spaced_tag("TryBlock"), parend(block))),
            value(
                (),
                preceded(
                    spaced_tag("Assign"),
                    parend(tuple((expr, spaced_tag(","), expr, spaced_tag(","), span))),
                ),
            ),
            value(
                (),
                preceded(
                    spaced_tag("AssignOp"),
                    parend(tuple((
                        bin_op,
                        spaced_tag(","),
                        expr,
                        spaced_tag(","),
                        expr,
                    ))),
                ),
            ),
            value(
                (),
                preceded(
                    spaced_tag("Field"),
                    parend(separated_pair(expr, spaced_tag(","), ident)),
                ),
            ),
            value(
                (),
                preceded(
                    spaced_tag("Index"),
                    parend(tuple((expr, spaced_tag(","), expr, spaced_tag(","), span))),
                ),
            ),
            value(
                (),
                preceded(
                    spaced_tag("Range"),
                    parend(tuple((
                        option(expr),
                        spaced_tag(","),
                        option(expr),
                        spaced_tag(","),
                        range_limits,
                    ))),
                ),
            ),
            value((), spaced_tag("Underscore")),
            value(
                (),
                preceded(
                    spaced_tag("Path"),
                    parend(separated_pair(option(q_self), spaced_tag(","), path)),
                ),
            ),
            value(
                (),
                preceded(
                    spaced_tag("AddrOf"),
                    parend(tuple((
                        borrow_kind,
                        spaced_tag(","),
                        mutability,
                        spaced_tag(","),
                        expr,
                    ))),
                ),
            ),
            value(
                (),
                preceded(
                    spaced_tag("Break"),
                    parend(separated_pair(option(label), spaced_tag(","), expr)),
                ),
            ),
            value((), preceded(spaced_tag("Continue"), parend(option(label)))),
            value((), preceded(spaced_tag("InlineAsm"), parend(inline_asm))),
            value(
                (),
                preceded(
                    spaced_tag("OffsetOf"),
                    parend(separated_pair(ty, spaced_tag(","), list(ident))),
                ),
            ),
            value((), preceded(spaced_tag("Struct"), parend(struct_expr))),
            value(
                (),
                preceded(
                    spaced_tag("Repeat"),
                    parend(separated_pair(expr, spaced_tag(","), anon_const)),
                ),
            ),
            value((), preceded(spaced_tag("Paren"), parend(expr))),
            value((), preceded(spaced_tag("Try"), parend(expr))),
            value((), preceded(spaced_tag("Yield"), parend(option(expr)))),
            value((), preceded(spaced_tag("Yeet"), parend(option(expr)))),
            value((), preceded(spaced_tag("Become"), parend(expr))),
            alt((
                value(
                    (),
                    preceded(spaced_tag("FormatArgs"), parend(parse_format_args)),
                ),
                value(
                    (),
                    preceded(
                        spaced_tag("UnsafeBinderCast"),
                        parend(tuple((
                            unsafe_binder_cast_kind,
                            spaced_tag(","),
                            expr,
                            spaced_tag(","),
                            option(ty),
                        ))),
                    ),
                ),
                value((), spaced_tag("Dummy")),
            )),
        )),
    ))(input)
}

fn parse_format_args(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "FormatArgs",
        tuple((
            struct_field("span", span),
            struct_field("template", list(format_args_piece)),
            struct_field("arguments", format_arguments),
            struct_field(
                "uncooked_fmt_str",
                parend(separated_pair(lit_kind, spaced_tag(","), spaced_string)),
            ),
        )),
        discard,
    )(input)
}

fn format_arguments(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "FormatArguments",
        tuple((
            struct_field("arguments", list(format_argument)),
            struct_field("num_unnamed_args", complete::u64),
            struct_field("num_explicit_args", complete::u64),
            struct_field("names", hashmap(spaced_string, complete::u64)),
            struct_field(
                "uncooked_fmt_str",
                parend(separated_pair(lit_kind, spaced_tag(","), spaced_string)),
            ),
        )),
        discard,
    )(input)
}

fn format_argument(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "FormatArgument",
        tuple((
            struct_field("kind", format_argument_kind),
            struct_field("expr", expr),
        )),
        discard,
    )(input)
}

fn format_argument_kind(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("Normal")),
        value((), preceded(spaced_tag("Named"), parend(ident))),
        value((), preceded(spaced_tag("captured"), parend(ident))),
    ))(input)
}

fn format_args_piece(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Literal"), parend(spaced_string))),
        value(
            (),
            preceded(spaced_tag("Placeholder"), parend(format_placeholder)),
        ),
    ))(input)
}

fn format_placeholder(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "FormatPlaceholder",
        tuple((
            struct_field("argument", format_arg_position),
            struct_field("span", span),
            struct_field("format_trait", format_trait),
            struct_field("format_options", format_options),
        )),
        discard,
    )(input)
}

fn format_options(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "FormatOptions",
        tuple((
            struct_field("width", option(format_count)),
            struct_field("precision", option(format_count)),
            struct_field("alignment", option(format_alignment)),
            struct_field(
                "fill",
                option(delimited(
                    spaced_tag("'"),
                    complete::anychar,
                    spaced_tag("'"),
                )),
            ),
            struct_field("sign", option(format_sign)),
            struct_field("alternate", parse_bool),
            struct_field("zero_pad", parse_bool),
            struct_field("debug_hex", option(format_debug_hex)),
        )),
        discard,
    )(input)
}

fn format_count(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Literal"), parend(complete::u64))),
        value(
            (),
            preceded(spaced_tag("Argument"), parend(format_arg_position)),
        ),
    ))(input)
}

fn format_debug_hex(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("Lower"), spaced_tag("Upper")))(input)
}

fn format_sign(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("Plus"), spaced_tag("Minus")))(input)
}

fn format_alignment(input: &str) -> IResult<&str, &str> {
    alt((
        spaced_tag("Left"),
        spaced_tag("Right"),
        spaced_tag("Center"),
    ))(input)
}

fn format_trait(input: &str) -> IResult<&str, &str> {
    alt((
        spaced_tag("Display"),
        spaced_tag("Debug"),
        spaced_tag("LowerExp"),
        spaced_tag("UpperExp"),
        spaced_tag("Octal"),
        spaced_tag("Pointer"),
        spaced_tag("Binary"),
        spaced_tag("LowerHex"),
        spaced_tag("UpperHex"),
    ))(input)
}

fn format_arg_position(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "FormatArgPosition",
        tuple((
            struct_field("index", result(complete::u64, complete::u64)),
            struct_field("kind", format_arg_position_kind),
            struct_field("span", option(span)),
        )),
        discard,
    )(input)
}

fn format_arg_position_kind(input: &str) -> IResult<&str, &str> {
    alt((
        spaced_tag("Implicit"),
        spaced_tag("Number"),
        spaced_tag("Named"),
    ))(input)
}

fn unsafe_binder_cast_kind(_input: &str) -> IResult<&str, ()> {
    todo!()
}

fn inline_asm(_input: &str) -> IResult<&str, ()> {
    todo!()
}

fn struct_expr(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "StructExpr",
        tuple((
            struct_field("qself", option(q_self)),
            struct_field("path", path),
            struct_field("fields", list(expr_field)),
            struct_field("rest", struct_rest),
        )),
        discard,
    )(input)
}

fn struct_rest(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Base"), parend(expr))),
        value((), preceded(spaced_tag("Rest"), parend(span))),
        value((), spaced_tag("None")),
    ))(input)
}

fn expr_field(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "ExprField",
        tuple((
            struct_field("attrs", list(attribute)),
            struct_field("id", node_id),
            struct_field("span", span),
            struct_field("ident", ident),
            struct_field("expr", expr),
            struct_field("is_shorthand", parse_bool),
            struct_field("is_placeholder", parse_bool),
        )),
        discard,
    )(input)
}

fn borrow_kind(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("Ref"), spaced_tag("Raw")))(input)
}

fn range_limits(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("HalfOpen"), spaced_tag("Closed")))(input)
}

fn gen_block_kind(input: &str) -> IResult<&str, &str> {
    alt((
        spaced_tag("Async"),
        spaced_tag("Gen"),
        spaced_tag("AsyncGen"),
    ))(input)
}

fn closure(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "Closure",
        tuple((
            struct_field("binder", closure_binder),
            struct_field("capture_clause", capture_by),
            struct_field("constness", parse_const),
            struct_field("coroutine_kind", option(coroutine_kind)),
            struct_field("movability", movability),
            struct_field("fn_decl", fn_decl),
            struct_field("body", expr),
            struct_field("fn_decl_span", span),
            struct_field("fn_arg_span", span),
        )),
        discard,
    )(input)
}

fn fn_decl(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "FnDecl",
        tuple((
            struct_field("inputs", list(param)),
            struct_field("output", fn_ret_ty),
        )),
        discard,
    )(input)
}

fn fn_ret_ty(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Default"), parend(span))),
        value((), preceded(spaced_tag("Ty"), parend(ty))),
    ))(input)
}

fn param(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "Param",
        tuple((
            struct_field("attrs", list(attribute)),
            struct_field("ty", ty),
            struct_field("pat", pat),
            struct_field("id", node_id),
            struct_field("span", span),
            struct_field("is_placeholder", parse_bool),
        )),
        discard,
    )(input)
}

fn movability(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("Static"), spaced_tag("Movable")))(input)
}

fn coroutine_kind(input: &str) -> IResult<&str, ()> {
    alt((
        struct_parser(
            "Async",
            tuple((
                struct_field("span", span),
                struct_field("closure_id", node_id),
                struct_field("return_impl_trait_id", node_id),
            )),
            discard,
        ),
        struct_parser(
            "Gen",
            tuple((
                struct_field("span", span),
                struct_field("closure_id", node_id),
                struct_field("return_impl_trait_id", node_id),
            )),
            discard,
        ),
        struct_parser(
            "AsyncGen",
            tuple((
                struct_field("span", span),
                struct_field("closure_id", node_id),
                struct_field("return_impl_trait_id", node_id),
            )),
            discard,
        ),
    ))(input)
}

fn parse_const(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("No")),
        value((), preceded(spaced_tag("Yes"), parend(span))),
    ))(input)
}

fn capture_by(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("Ref")),
        struct_parser("Value", struct_field("move_kw", span), discard),
    ))(input)
}

fn closure_binder(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("NotPresent")),
        struct_parser(
            "NotPresent",
            tuple((
                struct_field("span", span),
                struct_field("generic_params", list(generic_param)),
            )),
            discard,
        ),
    ))(input)
}

fn generic_param(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "GenericParam",
        tuple((
            struct_field("id", node_id),
            struct_field("ident", ident),
            struct_field("attrs", list(attribute)),
            struct_field("bounds", list(generic_bound)),
            struct_field("is_placeholder", parse_bool),
            struct_field("kind", generic_param_kind),
            struct_field("colon_span", span),
        )),
        discard,
    )(input)
}

fn generic_param_kind(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("Lifetime")),
        struct_parser("Type", struct_field("default", option(ty)), discard),
        struct_parser(
            "Const",
            tuple((
                struct_field("ty", ty),
                struct_field("kw_span", span),
                struct_field("default", option(anon_const)),
            )),
            discard,
        ),
    ))(input)
}

fn generic_bound(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Trait"), parend(poly_trait_ref))),
        value((), preceded(spaced_tag("Outlives"), parend(lifetime))),
        value(
            (),
            preceded(
                spaced_tag("Use"),
                parend(separated_pair(
                    list(precise_capturing_arg),
                    spaced_tag(","),
                    span,
                )),
            ),
        ),
    ))(input)
}

fn precise_capturing_arg(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Lifetime"), parend(lifetime))),
        value(
            (),
            preceded(
                spaced_tag("Arg"),
                parend(separated_pair(path, spaced_tag(","), node_id)),
            ),
        ),
    ))(input)
}

fn poly_trait_ref(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "PolyTraitRef",
        tuple((
            struct_field("bound_generic_params", list(generic_param)),
            struct_field("modifiers", trait_bound_modifiers),
            struct_field("trait_ref", trait_ref),
            struct_field("span", span),
        )),
        discard,
    )(input)
}

fn trait_bound_modifiers(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "TraitBoundModifiers",
        tuple((
            struct_field("constness", bound_constness),
            struct_field("asyncness", bound_asyncness),
            struct_field("polarity", bound_polarity),
        )),
        discard,
    )(input)
}

fn bound_constness(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("Never")),
        value((), preceded(spaced_tag("Always"), parend(span))),
        value((), preceded(spaced_tag("Maybe"), parend(span))),
    ))(input)
}

fn bound_asyncness(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("Normal")),
        value((), preceded(spaced_tag("Async"), parend(span))),
    ))(input)
}

fn bound_polarity(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("Positive")),
        value((), preceded(spaced_tag("Negative"), parend(span))),
        value((), preceded(spaced_tag("Maybe"), parend(span))),
    ))(input)
}

fn trait_ref(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "TraitRef",
        tuple((struct_field("path", path), struct_field("ref_id", node_id))),
        discard,
    )(input)
}

fn match_kind(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("Prefix"), spaced_tag("Postfix")))(input)
}

fn arm(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "Arm",
        tuple((
            struct_field("attrs", list(attribute)),
            struct_field("pat", pat),
            struct_field("guard", option(expr)),
            struct_field("body", option(expr)),
            struct_field("span", span),
            struct_field("id", node_id),
            struct_field("is_placeholder", parse_bool),
        )),
        discard,
    )(input)
}

fn for_loop_kind(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("For"), spaced_tag("ForAwait")))(input)
}

fn label(input: &str) -> IResult<&str, &str> {
    preceded(spaced_tag("label"), parend(is_not(")")))(input)
}

fn method_call(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "MethodCall",
        tuple((
            struct_field("seg", path_segment),
            struct_field("receiver", expr),
            struct_field("args", list(expr)),
            struct_field("span", span),
        )),
        discard,
    )(input)
}

fn bin_op(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "Spanned",
        tuple((
            struct_field("node", bin_op_kind),
            struct_field("span", span),
        )),
        discard,
    )(input)
}

fn bin_op_kind(input: &str) -> IResult<&str, &str> {
    alt((
        spaced_tag("Add"),
        spaced_tag("Sub"),
        spaced_tag("Mul"),
        spaced_tag("Div"),
        spaced_tag("Rem"),
        spaced_tag("And"),
        spaced_tag("Or"),
        spaced_tag("BitXor"),
        spaced_tag("BitAnd"),
        spaced_tag("BitOr"),
        spaced_tag("Shl"),
        spaced_tag("Shr"),
        spaced_tag("Eq"),
        spaced_tag("Lt"),
        spaced_tag("Le"),
        spaced_tag("Ne"),
        spaced_tag("Ge"),
        spaced_tag("Gt"),
    ))(input)
}

fn un_op(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("Deref"), spaced_tag("Not"), spaced_tag("Neg")))(input)
}

fn lit(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "Lit",
        tuple((
            struct_field("kind", lit_kind),
            struct_field("symbol", spaced_string),
            struct_field("suffix", option(spaced_string)),
        )),
        discard,
    )(input)
}

fn lit_kind(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("Bool")),
        value((), spaced_tag("Byte")),
        value((), spaced_tag("Char")),
        value((), spaced_tag("Integer")),
        value((), spaced_tag("Float")),
        value((), spaced_tag("Str")),
        value((), preceded(spaced_tag("StrRaw"), parend(complete::u8))),
        value((), spaced_tag("ByteStr")),
        value((), preceded(spaced_tag("ByteStrRaw"), parend(complete::u8))),
        value((), spaced_tag("CStr")),
        value((), preceded(spaced_tag("CStrRaw"), parend(complete::u8))),
    ))(input)
}

fn pat(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "Pat",
        tuple((
            struct_field("id", node_id),
            struct_field("kind", pat_kind),
            struct_field("span", span),
            struct_field("tokens", option(lazy_attr_token_stream)),
        )),
        discard,
    )(input)
}

fn pat_kind(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("Wild")),
        value(
            (),
            preceded(
                spaced_tag("Ident"),
                parend(tuple((
                    binding_mode,
                    spaced_tag(","),
                    ident,
                    spaced_tag(","),
                    option(pat),
                ))),
            ),
        ),
        value(
            (),
            preceded(
                spaced_tag("Struct"),
                parend(tuple((
                    option(q_self),
                    spaced_tag(","),
                    path,
                    spaced_tag(","),
                    list(pat_field),
                    spaced_tag(","),
                    pat_fields_rest,
                ))),
            ),
        ),
        value(
            (),
            preceded(
                spaced_tag("TupleStruct"),
                parend(tuple((
                    option(q_self),
                    spaced_tag(","),
                    path,
                    spaced_tag(","),
                    list(pat),
                ))),
            ),
        ),
        value((), preceded(spaced_tag("Or"), parend(list(pat)))),
        value(
            (),
            preceded(
                spaced_tag("Path"),
                parend(separated_pair(option(q_self), spaced_tag(","), path)),
            ),
        ),
        value((), preceded(spaced_tag("Tuple"), parend(list(pat)))),
        value((), preceded(spaced_tag("Box"), parend(pat))),
        value((), preceded(spaced_tag("Deref"), parend(pat))),
        value(
            (),
            preceded(
                spaced_tag("Ref"),
                parend(separated_pair(pat, spaced_tag(","), mutability)),
            ),
        ),
        value((), preceded(spaced_tag("Expr"), parend(expr))),
        value(
            (),
            preceded(
                spaced_tag("Range"),
                parend(tuple((
                    option(expr),
                    spaced_tag(","),
                    option(expr),
                    spaced_tag(","),
                    spanned_range_end,
                ))),
            ),
        ),
        value((), preceded(spaced_tag("Slice"), parend(list(pat)))),
        value((), spaced_tag("Rest")),
        value((), spaced_tag("Never")),
        value(
            (),
            preceded(
                spaced_tag("Guard"),
                parend(separated_pair(pat, spaced_tag(","), expr)),
            ),
        ),
        value((), preceded(spaced_tag("Paren"), parend(pat))),
    ))(input)
}

fn binding_mode(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("BindingMode"),
            parend(separated_pair(by_ref, spaced_tag(","), mutability)),
        ),
    )(input)
}

fn by_ref(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Yes"), parend(mutability))),
        value((), spaced_tag("No")),
    ))(input)
}

fn mutability(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("Not"), spaced_tag("Mut")))(input)
}

fn q_self(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "QSelf",
        tuple((
            struct_field("ty", ty),
            struct_field("path_span", span),
            struct_field("position", complete::u64),
        )),
        discard,
    )(input)
}

fn pat_field(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "PatField",
        tuple((
            struct_field("ident", ident),
            struct_field("pat", pat),
            struct_field("is_shorthand", parse_bool),
            struct_field("attrs", list(attribute)),
            struct_field("id", node_id),
            struct_field("span", span),
            struct_field("is_placeholder", parse_bool),
        )),
        discard,
    )(input)
}

fn pat_fields_rest(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("Rest"), spaced_tag("None")))(input)
}

fn spanned_range_end(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "Spanned",
        tuple((struct_field("node", range_end), struct_field("span", span))),
        discard,
    )(input)
}

fn range_end(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Included"), parend(range_syntax))),
        value((), spaced_tag("Excluded")),
    ))(input)
}

fn range_syntax(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("DotDotDot"), spaced_tag("DotDotEq")))(input)
}

#[derive(Debug, Clone)]
enum CommentKind {
    Line,
    Block,
}

fn comment_kind(input: &str) -> IResult<&str, CommentKind> {
    alt((
        value(CommentKind::Line, spaced_tag("Line")),
        value(CommentKind::Block, spaced_tag("Block")),
    ))(input)
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
    struct_parser(
        "Item",
        tuple((
            struct_field("attrs", list(attribute)),
            struct_field("id", node_id),
            struct_field("span", span),
            struct_field("vis", visibility),
            struct_field("ident", ident),
            struct_field("kind", item_kind),
            struct_field("tokens", option(lazy_attr_token_stream)),
        )),
        |(attrs, _, span, _, ident, kind, _)| Item::new(attrs, span, ident, kind),
    )(input)
}

fn visibility(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "Visibility",
        tuple((
            struct_field("kind", visibility_kind),
            struct_field("span", span),
            struct_field("tokens", option(lazy_attr_token_stream)),
        )),
        discard,
    )(input)
}

fn visibility_kind(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("Public")),
        value((), spaced_tag("Inherited")),
        struct_parser(
            "Restricted",
            tuple((
                struct_field("path", path),
                struct_field("id", node_id),
                struct_field("shorthand", parse_bool),
            )),
            discard,
        ),
    ))(input)
}

#[derive(Debug, Clone)]
struct Span<'a>(&'a str);

fn spaced_string(input: &str) -> IResult<&str, &str> {
    spaced(delimited(
        tag("\""),
        escaped(is_not("\"'"), '\\', complete::one_of("\"'\\")),
        tag("\""),
    ))(input)
}

fn span(input: &str) -> IResult<&str, Span> {
    map(spaced_string, Span)(input)
}

fn modspans(input: &str) -> IResult<&str, Span> {
    struct_parser(
        "Item",
        tuple((
            struct_field("inner_span", span),
            struct_field("outer_span", span),
        )),
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

fn item_kind(input: &str) -> IResult<&str, Option<ItemKind>> {
    alt((
        map(
            alt((
                map(preceded(spaced_tag("Fn"), parend(parse_fn)), ItemKind::Fn),
                map(
                    preceded(spaced_tag("Mod"), parend(parse_mod)),
                    ItemKind::Mod,
                ),
                map(
                    preceded(spaced_tag("Trait"), parend(parse_trait)),
                    ItemKind::Trait,
                ),
                map(
                    preceded(spaced_tag("Impl"), parend(parse_impl)),
                    ItemKind::Impl,
                ),
            )),
            Some,
        ),
        map(
            alt((
                value(
                    (),
                    preceded(spaced_tag("ExternCrate"), parend(option(spaced_string))),
                ),
                value((), preceded(spaced_tag("Use"), parend(use_tree))),
                value((), preceded(spaced_tag("Static"), parend(static_item))),
                value((), preceded(spaced_tag("Const"), parend(const_item))),
                value((), preceded(spaced_tag("ForeignMod"), parend(foreign_mod))),
                value((), preceded(spaced_tag("GlobalAsm"), parend(inline_asm))),
                value((), preceded(spaced_tag("TyAlias"), parend(ty_alias))),
                value(
                    (),
                    preceded(
                        spaced_tag("Enum"),
                        parend(separated_pair(enum_def, spaced_tag(","), generics)),
                    ),
                ),
                value(
                    (),
                    preceded(
                        spaced_tag("Struct"),
                        parend(separated_pair(variant_data, spaced_tag(","), generics)),
                    ),
                ),
                value(
                    (),
                    preceded(
                        spaced_tag("Union"),
                        parend(separated_pair(variant_data, spaced_tag(","), generics)),
                    ),
                ),
                value(
                    (),
                    preceded(
                        spaced_tag("TraitAlias"),
                        parend(separated_pair(
                            generics,
                            spaced_tag(","),
                            list(generic_bound),
                        )),
                    ),
                ),
                value((), preceded(spaced_tag("MacroDef"), parend(macro_def))),
                value((), preceded(spaced_tag("Delegation"), parend(parser_todo))),
            )),
            |_| None,
        ),
    ))(input)
}

fn macro_def(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "MacroDef",
        tuple((
            struct_field("body", delim_args),
            struct_field("macro_rules", parse_bool),
        )),
        discard,
    )(input)
}

fn delim_args(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "DelimArgs",
        tuple((
            struct_field("dspan", delim_span),
            struct_field("delim", delimiter),
            struct_field("tokens", token_stream),
        )),
        discard,
    )(input)
}

fn delim_span(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "DelimSpan",
        tuple((struct_field("open", span), struct_field("close", span))),
        discard,
    )(input)
}

fn delimiter(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("Parenthesis")),
        value((), spaced_tag("Brace")),
        value((), spaced_tag("Bracket")),
        preceded(spaced_tag("Invisible"), parend(invisible_origin)),
    ))(input)
}

fn invisible_origin(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("MetaVar"), parend(meta_var_kind))),
        value((), spaced_tag("ProcMacro")),
        value((), spaced_tag("FlattenToken")),
    ))(input)
}

fn meta_var_kind(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Pat"), parend(nt_pat_kind))),
        struct_parser(
            "Expr",
            tuple((
                struct_field("kind", nt_expr_kind),
                struct_field("can_begin_literal_maybe_minus", parse_bool),
                struct_field("can_begin_string_literal", parse_bool),
            )),
            discard,
        ),
        value((), spaced_tag("Item")),
        value((), spaced_tag("Block")),
        value((), spaced_tag("Stmt")),
        value((), spaced_tag("Ty")),
        value((), spaced_tag("Ident")),
        value((), spaced_tag("Lifetime")),
        value((), spaced_tag("Literal")),
        value((), spaced_tag("Meta")),
        value((), spaced_tag("Path")),
        value((), spaced_tag("Vis")),
        value((), spaced_tag("TT")),
    ))(input)
}

fn nt_expr_kind(input: &str) -> IResult<&str, ()> {
    alt((
        struct_parser("Expr2021", struct_field("inferred", parse_bool), discard),
        value((), spaced_tag("Expr")),
    ))(input)
}

fn nt_pat_kind(input: &str) -> IResult<&str, ()> {
    alt((
        struct_parser("PatParam", struct_field("inferred", parse_bool), discard),
        value((), spaced_tag("PatWithOr")),
    ))(input)
}

fn enum_def(input: &str) -> IResult<&str, ()> {
    struct_parser("EnumDef", struct_field("variants", list(variant)), discard)(input)
}

fn variant(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "Variant",
        tuple((
            struct_field("attrs", list(attribute)),
            struct_field("id", node_id),
            struct_field("span", span),
            struct_field("vis", visibility),
            struct_field("ident", ident),
            struct_field("data", variant_data),
            struct_field("disr_expr", option(anon_const)),
            struct_field("is_placeholder", parse_bool),
        )),
        discard,
    )(input)
}

fn variant_data(input: &str) -> IResult<&str, ()> {
    alt((
        struct_parser(
            "Struct",
            tuple((
                struct_field("fields", list(field_def)),
                struct_field("recovered", spaced_tag("No")),
            )),
            discard,
        ),
        value(
            (),
            preceded(
                spaced_tag("Tuple"),
                parend(separated_pair(list(field_def), spaced_tag(","), node_id)),
            ),
        ),
        value((), preceded(spaced_tag("Unit"), parend(node_id))),
    ))(input)
}

fn field_def(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "FieldDef",
        tuple((
            struct_field("attrs", list(attribute)),
            struct_field("id", node_id),
            struct_field("span", span),
            struct_field("vis", visibility),
            struct_field("safety", safety),
            struct_field("ident", ident),
            struct_field("ty", ty),
            struct_field("default", option(anon_const)),
            struct_field("is_placeholder", parse_bool),
        )),
        discard,
    )(input)
}

fn foreign_item_kind(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Static"), parend(static_item))),
        value((), preceded(spaced_tag("Fn"), parend(parse_fn))),
        value((), preceded(spaced_tag("TyAlias"), parend(ty_alias))),
    ))(input)
}

fn foreign_item(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "Item",
        tuple((
            struct_field("attrs", list(attribute)),
            struct_field("id", node_id),
            struct_field("span", span),
            struct_field("vis", visibility),
            struct_field("ident", ident),
            struct_field("kind", foreign_item_kind),
            struct_field("tokens", option(lazy_attr_token_stream)),
        )),
        discard,
    )(input)
}

fn foreign_mod(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "ForeignMod",
        tuple((
            struct_field("extern_span", span),
            struct_field("safety", safety),
            struct_field("abi", option(str_lit)),
            struct_field("items", list(foreign_item)),
        )),
        discard,
    )(input)
}

fn static_item(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "StaticItem",
        tuple((
            struct_field("ty", ty),
            struct_field("safety", safety),
            struct_field("mutability", mutability),
            struct_field("expr", option(expr)),
        )),
        discard,
    )(input)
}

fn use_tree(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "UseTree",
        tuple((
            struct_field("prefix", path),
            struct_field("kind", use_tree_kind),
            struct_field("span", span),
        )),
        discard,
    )(input)
}

fn use_tree_kind(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Simple"), parend(option(ident)))),
        struct_parser(
            "Nested",
            tuple((
                struct_field(
                    "items",
                    list(parend(separated_pair(use_tree, spaced_tag(","), node_id))),
                ),
                struct_field("span", span),
            )),
            discard,
        ),
        value((), spaced_tag("Glob")),
    ))(input)
}

#[derive(Debug, Clone)]
struct Fn<'a> {
    body: Option<Block<'a>>,
}

impl<'a> Fn<'a> {
    fn new(body: Option<Block<'a>>) -> Self {
        Self { body }
    }
}

fn parse_fn(input: &str) -> IResult<&str, Fn> {
    struct_parser(
        "Fn",
        tuple((
            struct_field("defaultness", defaultness),
            struct_field("generics", generics),
            struct_field("sig", fn_sig),
            struct_field("body", option(block)),
        )),
        |(_, _, _, body)| Fn::new(body),
    )(input)
}

fn fn_sig(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "FnSig",
        tuple((
            struct_field("header", fn_header),
            struct_field("decl", fn_decl),
            struct_field("span", span),
        )),
        discard,
    )(input)
}

fn fn_header(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "FnHeader",
        tuple((
            struct_field("safety", safety),
            struct_field("coroutine_kind", option(coroutine_kind)),
            struct_field("constness", parse_const),
            struct_field("ext", parse_extern),
        )),
        discard,
    )(input)
}

fn parse_extern(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("None")),
        value((), preceded(spaced_tag("Implicit"), parend(span))),
        value(
            (),
            preceded(
                spaced_tag("Explicit"),
                parend(separated_pair(str_lit, spaced_tag(","), span)),
            ),
        ),
    ))(input)
}

fn str_lit(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "StrLit",
        tuple((
            struct_field("symbol", spaced_string),
            struct_field("suffix", option(spaced_string)),
            struct_field("symbol_unescaped", spaced_string),
            struct_field("style", str_style),
            struct_field("span", span),
        )),
        discard,
    )(input)
}

fn str_style(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("Cooked")),
        value((), preceded(spaced_tag("Raw"), parend(complete::u8))),
    ))(input)
}

fn generics(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "Generics",
        tuple((
            struct_field("params", list(generic_param)),
            struct_field("where_clause", where_clause),
            struct_field("span", span),
        )),
        discard,
    )(input)
}

fn where_clause(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "WhereClause",
        tuple((
            struct_field("has_where_token", parse_bool),
            struct_field("predicates", list(where_predicate)),
            struct_field("span", span),
        )),
        discard,
    )(input)
}

fn where_predicate(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "WherePredicate",
        tuple((
            struct_field("kind", where_predicate_kind),
            struct_field("id", node_id),
            struct_field("span", span),
        )),
        discard,
    )(input)
}

fn where_predicate_kind(input: &str) -> IResult<&str, ()> {
    alt((
        preceded(spaced_tag("BoundPredicate"), parend(where_bound_predicate)),
        preceded(
            spaced_tag("RegionPredicate"),
            parend(where_region_predicate),
        ),
    ))(input)
}

fn where_bound_predicate(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "WhereBoundPredicate",
        tuple((
            struct_field("bound_generic_params", list(generic_param)),
            struct_field("bounded_ty", ty),
            struct_field("bounds", list(generic_bound)),
        )),
        discard,
    )(input)
}

fn where_region_predicate(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "WhereRegionPredicate",
        tuple((
            struct_field("lifetime", lifetime),
            struct_field("bounds", list(generic_bound)),
        )),
        discard,
    )(input)
}

fn defaultness(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Default"), parend(span))),
        value((), spaced_tag("Final")),
    ))(input)
}

#[derive(Debug, Clone)]
struct Block<'a> {
    span: Span<'a>,
}

impl<'a> Block<'a> {
    fn new(span: Span<'a>) -> Self {
        Self { span }
    }
}

fn block(input: &str) -> IResult<&str, Block> {
    struct_parser(
        "Block",
        tuple((
            struct_field("stmts", list(stmt)),
            struct_field("id", node_id),
            struct_field("rules", block_check_mode),
            struct_field("span", span),
            struct_field("tokens", option(lazy_attr_token_stream)),
            struct_field("could_be_bare_literal", parse_bool),
        )),
        |(_, _, _, span, _, _)| Block::new(span),
    )(input)
}

fn block_check_mode(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("Default")),
        value((), preceded(spaced_tag("Unsafe"), parend(unsafe_source))),
    ))(input)
}

fn unsafe_source(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("CompilerGenerated"), spaced_tag("UserProvided")))(input)
}

fn stmt(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "Stmt",
        tuple((
            struct_field("id", node_id),
            struct_field("kind", stmt_kind),
            struct_field("span", span),
        )),
        discard,
    )(input)
}

fn stmt_kind(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Let"), parend(local))),
        value((), preceded(spaced_tag("Item"), parend(item))),
        value((), preceded(spaced_tag("Expr"), parend(expr))),
        value((), preceded(spaced_tag("Semi"), parend(expr))),
        value((), spaced_tag("Empty")),
    ))(input)
}

fn local(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "Local",
        tuple((
            struct_field("id", node_id),
            struct_field("pat", pat),
            struct_field("ty", option(ty)),
            struct_field("kind", local_kind),
            struct_field("span", span),
            struct_field("colon_sp", option(span)),
            struct_field("attrs", list(attribute)),
            struct_field("tokens", option(lazy_attr_token_stream)),
        )),
        discard,
    )(input)
}

fn local_kind(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("Decl")),
        value((), preceded(spaced_tag("Init"), parend(expr))),
        value(
            (),
            preceded(
                spaced_tag("InitElse"),
                parend(separated_pair(expr, spaced_tag(","), block)),
            ),
        ),
    ))(input)
}

#[derive(Debug, Clone)]
struct Mod<'a> {
    items: Vec<Item<'a>>,
    span: Span<'a>,
}

impl<'a> Mod<'a> {
    fn new(items: Vec<Item<'a>>, span: Span<'a>) -> Self {
        Self { items, span }
    }
}

fn parse_mod(input: &str) -> IResult<&str, Mod> {
    map(
        preceded(
            spaced_tag("Loaded"),
            parend(tuple((
                list(item),
                spaced_tag(","),
                inline,
                spaced_tag(","),
                modspans,
                spaced_tag(","),
                result(unit, unit),
            ))),
        ),
        |(items, _, _, _, span, _, _)| Mod::new(items, span),
    )(input)
}

fn inline(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("Yes"), spaced_tag("No")))(input)
}

#[derive(Debug, Clone)]
struct Trait<'a> {
    items: Vec<AssocItem<'a>>,
}

impl<'a> Trait<'a> {
    fn new(items: Vec<AssocItem<'a>>) -> Self {
        Self { items }
    }
}

fn parse_trait(input: &str) -> IResult<&str, Trait> {
    struct_parser(
        "Trait",
        tuple((
            struct_field("safety", safety),
            struct_field("is_auto", is_auto),
            struct_field("generics", generics),
            struct_field("bounds", list(generic_bound)),
            struct_field("items", list(assoc_item)),
        )),
        |(_, _, _, _, items)| Trait::new(items),
    )(input)
}

fn is_auto(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("Yes"), spaced_tag("No")))(input)
}

#[derive(Debug, Clone)]
struct AssocItem<'a> {
    attrs: Vec<Attribute<'a>>,
    span: Span<'a>,
    ident: Ident<'a>,
    assoc_fn: Option<Fn<'a>>,
}

impl<'a> AssocItem<'a> {
    fn new(
        attrs: Vec<Attribute<'a>>,
        span: Span<'a>,
        ident: Ident<'a>,
        assoc_fn: Option<Fn<'a>>,
    ) -> Self {
        Self {
            attrs,
            span,
            ident,
            assoc_fn,
        }
    }
}

fn assoc_item(input: &str) -> IResult<&str, AssocItem> {
    struct_parser(
        "Item",
        tuple((
            struct_field("attrs", list(attribute)),
            struct_field("id", node_id),
            struct_field("span", span),
            struct_field("vis", visibility),
            struct_field("ident", ident),
            struct_field("kind", assoc_item_kind),
            struct_field("tokens", option(lazy_attr_token_stream)),
        )),
        |(attrs, _, span, _, ident, kind, _)| AssocItem::new(attrs, span, ident, kind),
    )(input)
}

fn assoc_item_kind<'a>(input: &'a str) -> IResult<&'a str, Option<Fn<'a>>> {
    alt((
        map(preceded(spaced_tag("Fn"), parend(parse_fn)), Some),
        map(
            alt((
                preceded(spaced_tag("Const"), parend(const_item)),
                preceded(spaced_tag("Type"), parend(ty_alias)),
                preceded(spaced_tag("MacCall"), parend(parser_todo)),
                preceded(spaced_tag("Delegation"), parend(parser_todo)),
                preceded(spaced_tag("DelegationMac"), parend(parser_todo)),
            )),
            |_| None,
        ),
    ))(input)
}

fn const_item(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "ConstItem",
        tuple((
            struct_field("defaultness", defaultness),
            struct_field("generics", generics),
            struct_field("ty", ty),
            struct_field("expr", option(expr)),
        )),
        discard,
    )(input)
}

fn ty_alias(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "TyAlias",
        tuple((
            struct_field("defaultness", defaultness),
            struct_field("generics", generics),
            struct_field("where_clauses", ty_alias_where_clauses),
            struct_field("bounds", list(generic_bound)),
            struct_field("ty", option(ty)),
        )),
        discard,
    )(input)
}

fn ty_alias_where_clauses(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "TyAliasWhereClauses",
        tuple((
            struct_field("before", ty_alias_where_clause),
            struct_field("after", ty_alias_where_clause),
            struct_field("split", complete::u64),
        )),
        discard,
    )(input)
}

fn ty_alias_where_clause(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "TyAliasWhereClause",
        tuple((
            struct_field("has_where_token", parse_bool),
            struct_field("span", span),
        )),
        discard,
    )(input)
}

#[derive(Debug, Clone)]
struct Impl<'a> {
    items: Vec<AssocItem<'a>>,
}

impl<'a> Impl<'a> {
    fn new(items: Vec<AssocItem<'a>>) -> Self {
        Self { items }
    }
}

fn parse_impl(input: &str) -> IResult<&str, Impl> {
    struct_parser(
        "Impl",
        tuple((
            struct_field("defaultness", defaultness),
            struct_field("safety", safety),
            struct_field("generics", generics),
            struct_field("constness", parse_const),
            struct_field("polarity", impl_polarity),
            struct_field("of_trait", option(trait_ref)),
            struct_field("self_ty", ty),
            struct_field("items", list(assoc_item)),
        )),
        |(_, _, _, _, _, _, _, items)| Impl::new(items),
    )(input)
}

fn impl_polarity(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("Positive")),
        value((), preceded(spaced_tag("Negative"), parend(span))),
    ))(input)
}
