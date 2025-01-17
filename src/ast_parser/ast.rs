use nom::{
    branch::alt,
    bytes::complete::is_not,
    character::complete,
    combinator::{map, value},
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    IResult,
};

use super::{
    tokens::{
        comment_kind, delim_span, delimiter, lazy_attr_token_stream, lit, lit_kind, token_stream,
        CommentKind,
    },
    utils::{
        discard, field, hashmap, id, list, option, parend, parse_bool, parser_todo, result,
        spaced_string, spaced_tag, struct_field, struct_parser, tuple_parser, tuple_struct_parser,
        unit, unit_struct_parser,
    },
};

pub fn parse(input: &str) -> IResult<&str, Crate> {
    krate(input)
}

#[derive(Debug, Clone)]
pub struct Crate<'a> {
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
        |(attrs, items, _, _, _)| Crate::new(attrs, items.into_iter().flatten().collect()),
    )(input)
}

fn node_id(input: &str) -> IResult<&str, u32> {
    tuple_struct_parser("NodeId", complete::u32, id)(input)
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
    tuple_struct_parser("NodeId", field(complete::u32), id)(input)
}

pub(crate) fn attr_style(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("Outer"), spaced_tag("Inner")))(input)
}

#[derive(Debug, Clone)]
enum AttrKind<'a> {
    Normal,
    DocComment(CommentKind, &'a str),
}

fn attr_kind(input: &str) -> IResult<&str, AttrKind> {
    alt((
        tuple_struct_parser("Normal", field(normal_attr), |_| AttrKind::Normal),
        tuple_struct_parser(
            "DocComment",
            tuple((field(comment_kind), field(spaced_string))),
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
        unit_struct_parser("Empty", ()),
        tuple_struct_parser("Delimited", field(delim_args), discard),
        struct_parser(
            "Delimited",
            tuple((struct_field("eq_span", span), struct_field("expr", expr))),
            discard,
        ),
    ))(input)
}

fn safety(input: &str) -> IResult<&str, ()> {
    alt((
        tuple_struct_parser("Unsafe", field(span), discard),
        tuple_struct_parser("Safe", field(span), discard),
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
        tuple_struct_parser("AngleBracketed", field(angle_bracketed_args), discard),
        tuple_struct_parser("Parenthesized", field(parenthesized_args), discard),
        tuple_struct_parser("ParenthesizedElided", field(span), discard),
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
        tuple_struct_parser("Arg", field(generic_arg), discard),
        tuple_struct_parser("Constraint", field(assoc_item_constraint), discard),
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

fn assoc_item_constraint_kind(input: &str) -> IResult<&str, ()> {
    alt((
        struct_parser("Equality", struct_field("term", term), discard),
        struct_parser(
            "Bound",
            struct_field("bounds", list(generic_bound)),
            discard,
        ),
    ))(input)
}

fn term(input: &str) -> IResult<&str, ()> {
    alt((
        tuple_struct_parser("Ty", field(ty), discard),
        tuple_struct_parser("Const", field(anon_const), discard),
    ))(input)
}

fn generic_arg(input: &str) -> IResult<&str, ()> {
    alt((
        tuple_struct_parser("Lifetime", field(lifetime), discard),
        tuple_struct_parser("Type", field(ty), discard),
        tuple_struct_parser("Const", field(anon_const), discard),
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
        tuple_struct_parser("Slice", field(ty), discard),
        tuple_struct_parser("Array", tuple((field(ty), field(anon_const))), discard),
        tuple_struct_parser("Ptr", field(mut_ty), discard),
        tuple_struct_parser(
            "Ref",
            tuple((field(option(lifetime)), field(mut_ty))),
            discard,
        ),
        tuple_struct_parser(
            "PinnedRef",
            tuple((field(option(lifetime)), field(mut_ty))),
            discard,
        ),
        tuple_struct_parser("BareFn", field(bare_fn_ty), discard),
        tuple_struct_parser("UnsafeBinder", field(unsafe_binder_ty), discard),
        unit_struct_parser("Never", ()),
        tuple_struct_parser("Tup", field(list(ty)), discard),
        tuple_struct_parser("Path", tuple((field(option(q_self)), field(path))), discard),
        tuple_struct_parser(
            "TraitObject",
            tuple((field(list(generic_bound)), field(trait_object_syntax))),
            discard,
        ),
        tuple_struct_parser(
            "ImplTrait",
            tuple((field(node_id), field(list(generic_bound)))),
            discard,
        ),
        tuple_struct_parser("Paren", field(ty), discard),
        unit_struct_parser("Infer", ()),
        unit_struct_parser("ImplicitSelf", ()),
        unit_struct_parser("CVarArgs", ()),
        tuple_struct_parser("Pat", tuple((field(ty), field(pat))), discard),
        unit_struct_parser("Dummy", ()),
    ))(input)
}

fn trait_object_syntax(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("Dyn"), spaced_tag("DynStar"), spaced_tag("None")))(input)
}

fn unsafe_binder_ty(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "UnsafeBinderTy",
        tuple((
            struct_field("generic_params", list(generic_param)),
            struct_field("inner_ty", ty),
        )),
        discard,
    )(input)
}

fn bare_fn_ty(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "BareFnTy",
        tuple((
            struct_field("safety", safety),
            struct_field("ext", parse_extern),
            struct_field("generic_params", list(generic_param)),
            struct_field("decl", fn_decl),
            struct_field("decl_span", span),
        )),
        discard,
    )(input)
}

fn mut_ty(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "MutTy",
        tuple((struct_field("ty", ty), struct_field("mutbl", mutability))),
        discard,
    )(input)
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
        tuple_struct_parser("Array", field(list(expr)), discard),
        tuple_struct_parser("ConstBlock", field(anon_const), discard),
        tuple_struct_parser("Call", tuple((field(expr), field(list(expr)))), discard),
        tuple_struct_parser("MethodCall", field(method_call), discard),
        tuple_struct_parser("Tup", field(list(expr)), discard),
        tuple_struct_parser(
            "Binary",
            tuple((field(bin_op), field(expr), field(expr))),
            discard,
        ),
        tuple_struct_parser("UnOp", tuple((field(un_op), field(expr))), discard),
        tuple_struct_parser("Lit", field(lit), discard),
        tuple_struct_parser("Cast", tuple((field(expr), field(ty))), discard),
        tuple_struct_parser("Type", tuple((field(expr), field(ty))), discard),
        tuple_struct_parser(
            "Let",
            tuple((field(pat), field(expr), field(span))),
            discard,
        ),
        tuple_struct_parser(
            "If",
            tuple((field(expr), field(block), field(option(expr)))),
            discard,
        ),
        tuple_struct_parser(
            "While",
            tuple((field(expr), field(block), field(option(label)))),
            discard,
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
        tuple_struct_parser(
            "Loop",
            tuple((field(block), field(option(label)), field(span))),
            discard,
        ),
        tuple_struct_parser(
            "Match",
            tuple((field(expr), field(list(arm)), field(match_kind))),
            discard,
        ),
        tuple_struct_parser("Closure", field(closure), discard),
        tuple_struct_parser(
            "Block",
            tuple((field(block), field(option(label)))),
            discard,
        ),
        tuple_struct_parser(
            "Gen",
            tuple((
                field(capture_by),
                field(block),
                field(gen_block_kind),
                field(span),
            )),
            discard,
        ),
        tuple_struct_parser("Await", tuple((field(expr), field(span))), discard),
        alt((
            tuple_struct_parser("TryBlock", field(block), discard),
            tuple_struct_parser(
                "Assign",
                tuple((field(expr), field(expr), field(span))),
                discard,
            ),
            tuple_struct_parser(
                "AssignOp",
                tuple((field(bin_op), field(expr), field(expr))),
                discard,
            ),
            tuple_struct_parser("Field", tuple((field(expr), field(ident))), discard),
            tuple_struct_parser(
                "Index",
                tuple((field(expr), field(expr), field(span))),
                discard,
            ),
            tuple_struct_parser(
                "Range",
                tuple((
                    field(option(expr)),
                    field(option(expr)),
                    field(range_limits),
                )),
                discard,
            ),
            unit_struct_parser("Underscore", ()),
            tuple_struct_parser("Path", tuple((field(option(q_self)), field(path))), discard),
            tuple_struct_parser(
                "AddrOf",
                tuple((field(borrow_kind), field(mutability), field(expr))),
                discard,
            ),
            tuple_struct_parser("Break", tuple((field(option(label)), field(expr))), discard),
            tuple_struct_parser("Continue", field(option(label)), discard),
            tuple_struct_parser("InlineAsm", field(inline_asm), discard),
            tuple_struct_parser("OffsetOf", tuple((field(ty), field(list(ident)))), discard),
            tuple_struct_parser("Struct", field(struct_expr), discard),
            tuple_struct_parser("Repeat", tuple((field(expr), field(anon_const))), discard),
            tuple_struct_parser("Paren", field(expr), discard),
            tuple_struct_parser("Try", field(expr), discard),
            tuple_struct_parser("Yield", field(option(expr)), discard),
            tuple_struct_parser("Yeet", field(option(expr)), discard),
            tuple_struct_parser("Become", field(expr), discard),
            alt((
                tuple_struct_parser("FormatArgs", field(parse_format_args), discard),
                tuple_struct_parser(
                    "UnsafeBinderCast",
                    tuple((
                        field(unsafe_binder_cast_kind),
                        field(expr),
                        field(option(ty)),
                    )),
                    discard,
                ),
                unit_struct_parser("Dummy", ()),
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
                tuple_parser(tuple((field(lit_kind), field(spaced_string)))),
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
        unit_struct_parser("Normal", ()),
        tuple_struct_parser("Named", field(ident), discard),
        tuple_struct_parser("captured", field(ident), discard),
    ))(input)
}

fn format_args_piece(input: &str) -> IResult<&str, ()> {
    alt((
        tuple_struct_parser("Literal", field(spaced_string), discard),
        tuple_struct_parser("Placeholder", field(format_placeholder), discard),
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
        tuple_struct_parser("Literal", field(complete::u64), discard),
        tuple_struct_parser("Argument", field(format_arg_position), discard),
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
        tuple_struct_parser("Base", field(expr), discard),
        tuple_struct_parser("Rest", field(span), discard),
        unit_struct_parser("None", ()),
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
        tuple_struct_parser("Default", field(span), discard),
        tuple_struct_parser("Ty", field(ty), discard),
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
        unit_struct_parser("No", ()),
        tuple_struct_parser("Yes", field(span), discard),
    ))(input)
}

fn capture_by(input: &str) -> IResult<&str, ()> {
    alt((
        unit_struct_parser("Ref", ()),
        struct_parser("Value", struct_field("move_kw", span), discard),
    ))(input)
}

fn closure_binder(input: &str) -> IResult<&str, ()> {
    alt((
        unit_struct_parser("NotPresent", ()),
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
        unit_struct_parser("Lifetime", ()),
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
        tuple_struct_parser("Trait", field(poly_trait_ref), discard),
        tuple_struct_parser("Outlives", field(lifetime), discard),
        tuple_struct_parser(
            "Use",
            tuple((field(list(precise_capturing_arg)), field(span))),
            discard,
        ),
    ))(input)
}

fn precise_capturing_arg(input: &str) -> IResult<&str, ()> {
    alt((
        tuple_struct_parser("Lifetime", field(lifetime), discard),
        tuple_struct_parser("Arg", tuple((field(path), field(node_id))), discard),
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
        unit_struct_parser("Never", ()),
        tuple_struct_parser("Always", field(span), discard),
        tuple_struct_parser("Maybe", field(span), discard),
    ))(input)
}

fn bound_asyncness(input: &str) -> IResult<&str, ()> {
    alt((
        unit_struct_parser("Normal", ()),
        tuple_struct_parser("Async", field(span), discard),
    ))(input)
}

fn bound_polarity(input: &str) -> IResult<&str, ()> {
    alt((
        unit_struct_parser("Positive", ()),
        tuple_struct_parser("Negative", field(span), discard),
        tuple_struct_parser("Maybe", field(span), discard),
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
        unit_struct_parser("Wild", ()),
        tuple_struct_parser(
            "Ident",
            tuple((field(binding_mode), field(ident), field(option(pat)))),
            discard,
        ),
        tuple_struct_parser(
            "Struct",
            tuple((
                field(option(q_self)),
                field(path),
                field(list(pat_field)),
                field(pat_fields_rest),
            )),
            discard,
        ),
        tuple_struct_parser(
            "TupleStruct",
            tuple((field(option(q_self)), field(path), field(list(pat)))),
            discard,
        ),
        tuple_struct_parser("Or", field(list(pat)), discard),
        tuple_struct_parser("Path", tuple((field(option(q_self)), field(path))), discard),
        tuple_struct_parser("Tuple", field(list(pat)), discard),
        tuple_struct_parser("Box", field(pat), discard),
        tuple_struct_parser("Deref", field(pat), discard),
        tuple_struct_parser("Ref", tuple((field(pat), field(mutability))), discard),
        tuple_struct_parser("Expr", field(expr), discard),
        tuple_struct_parser(
            "Range",
            tuple((
                field(option(expr)),
                field(option(expr)),
                field(spanned_range_end),
            )),
            discard,
        ),
        tuple_struct_parser("Slice", field(list(pat)), discard),
        unit_struct_parser("Rest", ()),
        unit_struct_parser("Never", ()),
        tuple_struct_parser("Guard", tuple((field(pat), field(expr))), discard),
        tuple_struct_parser("Paren", field(pat), discard),
    ))(input)
}

fn binding_mode(input: &str) -> IResult<&str, ()> {
    tuple_struct_parser(
        "BindingMode",
        tuple((field(by_ref), field(mutability))),
        discard,
    )(input)
}

fn by_ref(input: &str) -> IResult<&str, ()> {
    alt((
        tuple_struct_parser("Yes", field(mutability), discard),
        unit_struct_parser("No", ()),
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
        tuple_struct_parser("Included", field(range_syntax), discard),
        unit_struct_parser("Excluded", ()),
    ))(input)
}

fn range_syntax(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("DotDotDot"), spaced_tag("DotDotEq")))(input)
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

fn item(input: &str) -> IResult<&str, Option<Item>> {
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
        |(attrs, _, span, _, ident, kind, _)| Some(Item::new(attrs, span, ident, kind?)),
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
pub(crate) struct Span<'a>(&'a str);

pub(crate) fn span(input: &str) -> IResult<&str, Span> {
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
                tuple_struct_parser("Fn", field(parse_fn), ItemKind::Fn),
                tuple_struct_parser("Mod", field(parse_mod), ItemKind::Mod),
                tuple_struct_parser("Trait", field(parse_trait), ItemKind::Trait),
                tuple_struct_parser("Impl", field(parse_impl), ItemKind::Impl),
            )),
            Some,
        ),
        map(
            alt((
                tuple_struct_parser("ExternCrate", field(option(spaced_string)), discard),
                tuple_struct_parser("Use", field(use_tree), discard),
                tuple_struct_parser("Static", field(static_item), discard),
                tuple_struct_parser("Const", field(const_item), discard),
                tuple_struct_parser("ForeignMod", field(foreign_mod), discard),
                tuple_struct_parser("GlobalAsm", field(inline_asm), discard),
                tuple_struct_parser("TyAlias", field(ty_alias), discard),
                tuple_struct_parser("Enum", tuple((field(enum_def), field(generics))), discard),
                tuple_struct_parser(
                    "Struct",
                    tuple((field(variant_data), field(generics))),
                    discard,
                ),
                tuple_struct_parser(
                    "Union",
                    tuple((field(variant_data), field(generics))),
                    discard,
                ),
                tuple_struct_parser(
                    "TraitAlias",
                    tuple((field(generics), field(list(generic_bound)))),
                    discard,
                ),
                tuple_struct_parser("MacroDef", field(macro_def), discard),
                tuple_struct_parser("Delegation", field(parser_todo), discard),
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
        tuple_struct_parser(
            "Tuple",
            tuple((field(list(field_def)), field(node_id))),
            discard,
        ),
        tuple_struct_parser("Unit", field(node_id), discard),
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
        tuple_struct_parser("Static", field(static_item), discard),
        tuple_struct_parser("Fn", field(parse_fn), discard),
        tuple_struct_parser("TyAlias", field(ty_alias), discard),
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
        tuple_struct_parser("Simple", field(option(ident)), discard),
        struct_parser(
            "Nested",
            tuple((
                struct_field(
                    "items",
                    list(tuple_parser(tuple((field(use_tree), field(node_id))))),
                ),
                struct_field("span", span),
            )),
            discard,
        ),
        unit_struct_parser("Glob", ()),
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
        unit_struct_parser("None", ()),
        tuple_struct_parser("Implicit", field(span), discard),
        tuple_struct_parser("Explicit", tuple((field(str_lit), field(span))), discard),
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
        unit_struct_parser("Cooked", ()),
        tuple_struct_parser("Raw", field(complete::u8), discard),
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
        tuple_struct_parser("BoundPredicate", field(where_bound_predicate), discard),
        tuple_struct_parser("RegionPredicate", field(where_region_predicate), discard),
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
        tuple_struct_parser("Default", field(span), discard),
        unit_struct_parser("Final", ()),
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
        unit_struct_parser("Default", ()),
        tuple_struct_parser("Unsafe", field(unsafe_source), discard),
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
        tuple_struct_parser("Let", field(local), discard),
        tuple_struct_parser("Item", field(item), discard),
        tuple_struct_parser("Expr", field(expr), discard),
        tuple_struct_parser("Semi", field(expr), discard),
        unit_struct_parser("Empty", ()),
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
        unit_struct_parser("Decl", ()),
        tuple_struct_parser("Init", field(expr), discard),
        tuple_struct_parser("InitElse", tuple((field(expr), field(block))), discard),
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
    tuple_struct_parser(
        "Loaded",
        tuple((
            field(list(item)),
            field(inline),
            field(modspans),
            field(result(unit, unit)),
        )),
        |(items, _, span, _)| Mod::new(items.into_iter().flatten().collect(), span),
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
        tuple_struct_parser("Fn", field(parse_fn), Some),
        map(
            alt((
                tuple_struct_parser("Const", field(const_item), discard),
                tuple_struct_parser("Type", field(ty_alias), discard),
                tuple_struct_parser("MacCall", field(parser_todo), discard),
                tuple_struct_parser("Delegation", field(parser_todo), discard),
                tuple_struct_parser("DelegationMac", field(parser_todo), discard),
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
        unit_struct_parser("Positive", ()),
        tuple_struct_parser("Negative", field(span), discard),
    ))(input)
}
