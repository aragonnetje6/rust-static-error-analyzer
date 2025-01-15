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
    delimited(pair(spaced_tag(name), tag(":")), value, spaced_tag(","))
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
            preceded(
                spaced_tag("DocComment"),
                tuple((comment_kind, spaced_string)),
            ),
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
                struct_field("tokens", tokens),
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
                struct_field("tokens", tokens),
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
                struct_field("tokens", tokens),
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

fn angle_bracketed_args(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("AngleBracketedArgs"),
            curlied(tuple((
                struct_field("span", span),
                struct_field("args", list(angle_bracketed_arg)),
            ))),
        ),
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
    value(
        (),
        preceded(
            spaced_tag("Ty"),
            curlied(tuple((
                struct_field("id", node_id),
                struct_field("kind", ty_kind),
                struct_field("span", span),
                struct_field("tokens", tokens),
            ))),
        ),
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
                    generic_bounds,
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
    value(
        (),
        preceded(
            spaced_tag("AnonConst"),
            curlied(tuple((
                struct_field("id", node_id),
                struct_field("value", expr),
            ))),
        ),
    )(input)
}

fn expr(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("Expr"),
            curlied(tuple((
                struct_field("id", node_id),
                struct_field("kind", expr_kind),
                struct_field("span", span),
                struct_field("attrs", list(attribute)),
                struct_field("tokens", tokens),
            ))),
        ),
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
        value(
            (),
            preceded(
                spaced_tag("ForLoop"),
                curlied(tuple((
                    struct_field("pat", pat),
                    struct_field("iter", expr),
                    struct_field("body", block),
                    struct_field("label", option(label)),
                    struct_field("kind", for_loop_kind),
                ))),
            ),
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
    value(
        (),
        preceded(
            spaced_tag("FormatArgs"),
            curlied(tuple((
                struct_field("span", span),
                struct_field("template", list(format_args_piece)),
                struct_field("arguments", format_arguments),
                struct_field(
                    "uncooked_fmt_str",
                    parend(separated_pair(lit_kind, spaced_tag(","), spaced_string)),
                ),
            ))),
        ),
    )(input)
}

fn format_arguments(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("FormatArguments"),
            curlied(tuple((
                struct_field("arguments", list(format_argument)),
                struct_field("num_unnamed_args", complete::u64),
                struct_field("num_explicit_args", complete::u64),
                struct_field("names", hashmap(spaced_string, complete::u64)),
                struct_field(
                    "uncooked_fmt_str",
                    parend(separated_pair(lit_kind, spaced_tag(","), spaced_string)),
                ),
            ))),
        ),
    )(input)
}

fn format_argument(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("FormatArgument"),
            curlied(tuple((
                struct_field("kind", format_argument_kind),
                struct_field("expr", expr),
            ))),
        ),
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
    value(
        (),
        preceded(
            spaced_tag("FormatPlaceholder"),
            curlied(tuple((
                struct_field("argument", format_arg_position),
                struct_field("span", span),
                struct_field("format_trait", format_trait),
                struct_field("format_options", format_options),
            ))),
        ),
    )(input)
}

fn format_options(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("FormatOptions"),
            curlied(tuple((
                struct_field("width", option(format_count)),
                struct_field("precision", option(format_count)),
                struct_field("alignment", option(format_alignment)),
                struct_field("fill", option(complete::anychar)),
                struct_field("sign", option(format_sign)),
                struct_field("alternate", parse_bool),
                struct_field("zero_pad", parse_bool),
                struct_field("debug_hex", option(format_debug_hex)),
            ))),
        ),
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
    value(
        (),
        preceded(
            spaced_tag("FormatArgPosition"),
            curlied(tuple((
                struct_field("index", result(complete::u64, complete::u64)),
                struct_field("kind", format_arg_position_kind),
                struct_field("span", option(span)),
            ))),
        ),
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
    value(
        (),
        preceded(
            spaced_tag("StructExpr"),
            curlied(tuple((
                struct_field("qself", option(q_self)),
                struct_field("path", path),
                struct_field("fields", list(expr_field)),
                struct_field("rest", struct_rest),
            ))),
        ),
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
    value(
        (),
        preceded(
            spaced_tag("ExprField"),
            curlied(tuple((
                struct_field("attrs", list(attribute)),
                struct_field("id", node_id),
                struct_field("span", span),
                struct_field("ident", ident),
                struct_field("expr", expr),
                struct_field("is_shorthand", parse_bool),
                struct_field("is_placeholder", parse_bool),
            ))),
        ),
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
    value(
        (),
        preceded(
            spaced_tag("Closure"),
            curlied(tuple((
                struct_field("binder", closure_binder),
                struct_field("capture_clause", capture_by),
                struct_field("constness", parse_const),
                struct_field("coroutine_kind", option(coroutine_kind)),
                struct_field("movability", movability),
                struct_field("fn_decl", fn_decl),
                struct_field("body", expr),
                struct_field("fn_decl_span", span),
                struct_field("fn_arg_span", span),
            ))),
        ),
    )(input)
}

fn fn_decl(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("FnDecl"),
            curlied(tuple((
                struct_field("inputs", list(param)),
                struct_field("output", fn_ret_ty),
            ))),
        ),
    )(input)
}

fn fn_ret_ty(input: &str) -> IResult<&str, ()> {
    alt((
        value((), preceded(spaced_tag("Default"), parend(span))),
        value((), preceded(spaced_tag("Ty"), parend(ty))),
    ))(input)
}

fn param(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("Param"),
            curlied(tuple((
                struct_field("attrs", list(attribute)),
                struct_field("ty", ty),
                struct_field("pat", pat),
                struct_field("id", node_id),
                struct_field("span", span),
                struct_field("is_placeholder", parse_bool),
            ))),
        ),
    )(input)
}

fn movability(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("Static"), spaced_tag("Movable")))(input)
}

fn coroutine_kind(input: &str) -> IResult<&str, ()> {
    alt((
        value(
            (),
            preceded(
                spaced_tag("Async"),
                curlied(tuple((
                    struct_field("span", span),
                    struct_field("closure_id", node_id),
                    struct_field("return_impl_trait_id", node_id),
                ))),
            ),
        ),
        value(
            (),
            preceded(
                spaced_tag("Gen"),
                curlied(tuple((
                    struct_field("span", span),
                    struct_field("closure_id", node_id),
                    struct_field("return_impl_trait_id", node_id),
                ))),
            ),
        ),
        value(
            (),
            preceded(
                spaced_tag("AsyncGen"),
                curlied(tuple((
                    struct_field("span", span),
                    struct_field("closure_id", node_id),
                    struct_field("return_impl_trait_id", node_id),
                ))),
            ),
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
        value(
            (),
            preceded(spaced_tag("Value"), curlied(struct_field("move_kw", span))),
        ),
    ))(input)
}

fn closure_binder(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("NotPresent")),
        value(
            (),
            preceded(
                spaced_tag("NotPresent"),
                curlied(tuple((
                    struct_field("span", span),
                    struct_field("generic_params", list(generic_param)),
                ))),
            ),
        ),
    ))(input)
}

fn generic_param(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("GenericParam"),
            curlied(tuple((
                struct_field("id", node_id),
                struct_field("ident", ident),
                struct_field("attrs", list(attribute)),
                struct_field("bounds", list(generic_bound)),
                struct_field("is_placeholder", parse_bool),
                struct_field("kind", generic_param_kind),
                struct_field("colon_span", span),
            ))),
        ),
    )(input)
}

fn generic_param_kind(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("Lifetime")),
        value(
            (),
            preceded(
                spaced_tag("Type"),
                curlied(struct_field("default", option(ty))),
            ),
        ),
        value(
            (),
            preceded(
                spaced_tag("Type"),
                curlied(tuple((
                    struct_field("ty", ty),
                    struct_field("kw_span", span),
                    struct_field("default", option(anon_const)),
                ))),
            ),
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
    value(
        (),
        preceded(
            spaced_tag("PolyTraitRef"),
            curlied(tuple((
                struct_field("bound_generic_params", list(generic_param)),
                struct_field("modifiers", trait_bound_modifiers),
                struct_field("trait_ref", trait_ref),
                struct_field("span", span),
            ))),
        ),
    )(input)
}

fn trait_bound_modifiers(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("TraitBoundModifiers"),
            curlied(tuple((
                struct_field("constness", bound_constness),
                struct_field("asyncness", bound_asyncness),
                struct_field("polarity", bound_polarity),
            ))),
        ),
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
    value(
        (),
        preceded(
            spaced_tag("TraitRef"),
            curlied(tuple((
                struct_field("path", path),
                struct_field("ref_id", node_id),
            ))),
        ),
    )(input)
}

fn match_kind(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("Prefix"), spaced_tag("Postfix")))(input)
}

fn arm(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("Arm"),
            curlied(tuple((
                struct_field("attrs", list(attribute)),
                struct_field("pat", pat),
                struct_field("guard", option(expr)),
                struct_field("body", option(expr)),
                struct_field("span", span),
                struct_field("id", node_id),
                struct_field("is_placeholder", parse_bool),
            ))),
        ),
    )(input)
}

fn for_loop_kind(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("For"), spaced_tag("ForAwait")))(input)
}

fn label(input: &str) -> IResult<&str, &str> {
    preceded(spaced_tag("label"), parend(is_not(")")))(input)
}

fn method_call(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("MethodCall"),
            curlied(tuple((
                struct_field("seg", path_segment),
                struct_field("receiver", expr),
                struct_field("args", list(expr)),
                struct_field("span", span),
            ))),
        ),
    )(input)
}

fn bin_op(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("Spanned"),
            curlied(tuple((
                struct_field("node", bin_op_kind),
                struct_field("span", span),
            ))),
        ),
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
    value(
        (),
        preceded(
            spaced_tag("Lit"),
            curlied(tuple((
                struct_field("kind", lit_kind),
                struct_field("symbol", spaced_string),
                struct_field("suffix", option(spaced_string)),
            ))),
        ),
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
    value(
        (),
        preceded(
            spaced_tag("Pat"),
            curlied(tuple((
                struct_field("id", node_id),
                struct_field("kind", pat_kind),
                struct_field("span", span),
                struct_field("tokens", tokens),
            ))),
        ),
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
    value(
        (),
        preceded(
            spaced_tag("QSelf"),
            curlied(tuple((
                struct_field("ty", ty),
                struct_field("path_span", span),
                struct_field("position", complete::u64),
            ))),
        ),
    )(input)
}

fn pat_field(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("PatField"),
            curlied(tuple((
                struct_field("ident", ident),
                struct_field("pat", pat),
                struct_field("is_shorthand", parse_bool),
                struct_field("attrs", list(attribute)),
                struct_field("id", node_id),
                struct_field("span", span),
                struct_field("is_placeholder", parse_bool),
            ))),
        ),
    )(input)
}

fn pat_fields_rest(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("Rest"), spaced_tag("None")))(input)
}

fn spanned_range_end(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("Spanned"),
            curlied(tuple((
                struct_field("node", range_end),
                struct_field("span", span),
            ))),
        ),
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

fn visibility(input: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            spaced_tag("Visibility"),
            curlied(tuple((
                struct_field("kind", visibility_kind),
                struct_field("span", span),
                struct_field("tokens", tokens),
            ))),
        ),
    )
}

fn visibility_kind(input: &str) -> IResult<&str, ()> {
    alt((
        value((), spaced_tag("Public")),
        value((), spaced_tag("Inherited")),
        value(
            (),
            preceded(
                spaced_tag("Restricted"),
                curlied(tuple((
                    struct_field("path", path),
                    struct_field("id", node_id),
                    struct_field("shorthand", parse_bool),
                ))),
            ),
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

impl<'a> Block<'a> {
    fn new(span: Span<'a>) -> Self {
        Self { span }
    }
}

fn block(input: &str) -> IResult<&str, Block> {
    map(
        preceded(
            spaced_tag("Block"),
            curlied(tuple((
                struct_field("stmts", list(stmt)),
                struct_field("id", node_id),
                struct_field("rules", block_check_mode),
                struct_field("span", span),
                struct_field("tokens", tokens),
                struct_field("could_be_bare_literal", parse_bool),
            ))),
        ),
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
    value(
        (),
        preceded(
            spaced_tag("Stmt"),
            curlied(tuple((
                struct_field("id", node_id),
                struct_field("kind", stmt_kind),
                struct_field("span", span),
            ))),
        ),
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
    value(
        (),
        preceded(
            spaced_tag("Local"),
            curlied(tuple((
                struct_field("id", node_id),
                struct_field("pat", pat),
                struct_field("ty", option(ty)),
                struct_field("kind", local_kind),
                struct_field("span", span),
                struct_field("colon_sp", option(span)),
                struct_field("attrs", list(attribute)),
                struct_field("tokens", tokens),
            ))),
        ),
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
