use nom::{
    branch::alt,
    bytes::complete::is_not,
    character::complete,
    combinator::{map, value},
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    IResult,
};

use crate::{parse_enum, parse_struct};

use super::{
    tokens::{
        comment_kind, delim_span, delimiter, lazy_attr_token_stream, lit, lit_kind, token_stream,
    },
    utils::{
        discard, field, first, hashmap, list, none, option, parend, parse_bool, parser_todo,
        result, spaced, spaced_string, spaced_tag, struct_field, struct_parser, tuple_parser,
        tuple_struct_parser, unit, unit_struct_parser,
    },
};

pub fn parse(input: &str) -> IResult<&str, Crate> {
    krate(input)
}

#[derive(Debug, Clone)]
pub struct Crate<'a> {
    items: Vec<Item<'a>>,
}

impl<'a> Crate<'a> {
    fn new(items: Vec<Item<'a>>) -> Self {
        Self { items }
    }

    pub(crate) fn find_fn_attrs_for_span(&self, span: &str) -> Option<&[Attribute]> {
        self.items
            .iter()
            .find_map(|item| item.get_fn_attrs_for_span(span))
    }
}

fn krate(input: &str) -> IResult<&str, Crate> {
    parse_struct!((
        "Crate",
        {
            ("attrs", list(attribute)),
            ("items", list(item)),
            ("spans", modspans),
            ("id", node_id),
            ("is_placeholder", parse_bool),
        },
        |(_, items, _, _, _)| Crate::new(items.into_iter().flatten().collect())
    ))(input)
}

fn node_id(input: &str) -> IResult<&str, u32> {
    parse_struct!(("NodeId", (complete::u32,), first))(input)
}

#[derive(Debug, Clone)]
pub struct Attribute<'a> {
    kind: AttrKind<'a>,
}

impl<'a> Attribute<'a> {
    fn new(kind: AttrKind<'a>) -> Self {
        Self { kind }
    }

    pub(crate) fn contains_panic(&self) -> bool {
        if let AttrKind::DocComment(content) = self.kind {
            content.contains("# Panics")
        } else {
            false
        }
    }
}

fn attribute(input: &str) -> IResult<&str, Attribute> {
    parse_struct!((
        "Attribute",
        {
            ("kind", attr_kind),
            ("id", attr_id),
            ("style", attr_style),
            ("span", span),
        },
        |(kind, _, _, _)| Attribute::new(kind)
    ))(input)
}

fn attr_id(input: &str) -> IResult<&str, u32> {
    parse_struct!(("AttrId", (spaced(complete::u32),), first))(input)
}

pub(crate) fn attr_style(input: &str) -> IResult<&str, &str> {
    parse_enum!("Outer", "Inner",)(input)
}

#[derive(Debug, Clone)]
enum AttrKind<'a> {
    Normal,
    DocComment(&'a str),
}

fn attr_kind(input: &str) -> IResult<&str, AttrKind> {
    parse_enum!(
        ("Normal", (normal_attr,), |_| AttrKind::Normal),
        (
            "DocComment",
            (comment_kind, spaced_string,),
            |((), symbol)| AttrKind::DocComment(symbol)
        ),
    )(input)
}

fn normal_attr(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "NormalAttr",
        {
            ("item", attr_item),
            ("tokens", option(lazy_attr_token_stream)),
        },
        discard
    ))(input)
}

fn attr_item(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "AttrItem",
        {
            ("unsafety", safety),
            ("path", path),
            ("args", attr_args),
            ("tokens", option(lazy_attr_token_stream)),
        },
        discard
    ))(input)
}

fn attr_args(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Empty", [()]),
        ("Delimited", (delim_args,), discard),
        (
            "Eq",
            {("eq_span", span), ("expr", expr),},
            discard
        ),
    )(input)
}

fn safety(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Unsafe", (span,), discard),
        ("Safe", (span,), discard),
        ("Default", [()]),
    )(input)
}

fn path(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "Path",
        {
            ("span", span),
            ("segments", list(path_segment)),
            ("tokens", option(lazy_attr_token_stream)),
        },
        discard
    ))(input)
}

fn path_segment(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "PathSegment",
        {
            ("ident", ident),
            ("id", node_id),
            ("args", option(generic_args)),
        },
        discard
    ))(input)
}

fn generic_args(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("AngleBracketed", (angle_bracketed_args,), discard),
        ("Parenthesized", (parenthesized_args,), discard),
        ("ParenthesizedElided", (span,), discard),
    )(input)
}

fn parenthesized_args(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "ParenthesizedArgs",
        {
            ("span", span),
            ("inputs", list(ty)),
            ("inputs_span", span),
            ("output", fn_ret_ty),
        },
        discard
    ))(input)
}

fn angle_bracketed_args(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "AngleBracketedArgs",
        {("span", span), ("args", list(angle_bracketed_arg)),},
        discard
    ))(input)
}

fn angle_bracketed_arg(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Arg", (generic_arg,), discard),
        ("Constraint", (assoc_item_constraint,), discard),
    )(input)
}

fn assoc_item_constraint(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "AssocItemConstraint",
        {
            ("id", node_id),
            ("ident", ident),
            ("gen_args", option(generic_args)),
            ("kind", assoc_item_constraint_kind),
            ("span", span),
        },
        discard
    ))(input)
}

fn assoc_item_constraint_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Equality", {("term", term),}, discard),
        ("Bound", {("bounds", list(generic_bound)),}, discard),
    )(input)
}

fn term(input: &str) -> IResult<&str, ()> {
    parse_enum!(("Ty", (ty,), discard), ("Const", (anon_const,), discard),)(input)
}

fn generic_arg(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Lifetime", (annotated_lifetime,), discard),
        ("Type", (ty,), discard),
        ("Const", (anon_const,), discard),
    )(input)
}

fn annotated_lifetime(input: &str) -> IResult<&str, ()> {
    parse_struct!(("lifetime", (lifetime,), discard))(input)
}

fn lifetime(input: &str) -> IResult<&str, ()> {
    value(
        (),
        separated_pair(
            spaced(complete::digit1),
            spaced_tag(":"),
            pair(spaced_tag("'"), is_not(")")),
        ),
    )(input)
}

fn ty(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "Ty",
        {
            ("id", node_id),
            ("kind", ty_kind),
            ("span", span),
            ("tokens", option(lazy_attr_token_stream)),
        },
        discard
    ))(input)
}

fn ty_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Slice", (ty,), discard),
        ("Array", (ty, anon_const,), discard),
        ("Ptr", (mut_ty,), discard),
        ("Ref", (option(annotated_lifetime), mut_ty,), discard),
        ("PinnedRef", (option(annotated_lifetime), mut_ty,), discard),
        ("BareFn", (bare_fn_ty,), discard),
        ("UnsafeBinder", (unsafe_binder_ty,), discard),
        ("Never", [()]),
        ("Tup", (list(ty),), discard),
        ("Path", (option(q_self), path,), discard),
        (
            "TraitObject",
            (list(generic_bound), trait_object_syntax,),
            discard
        ),
        ("ImplTrait", (node_id, list(generic_bound),), discard),
        ("Paren", (ty,), discard),
        ("Infer", [()]),
        ("ImplicitSelf", [()]),
        ("CVarArgs", [()]),
        ("Pat", (ty, pat,), discard),
        ("Dummy", [()]),
    )(input)
}

fn trait_object_syntax(input: &str) -> IResult<&str, &str> {
    parse_enum!("Dyn", "DynStar", "None",)(input)
}

fn unsafe_binder_ty(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "UnsafeBinderTy",
        {("generic_params", list(generic_param)), ("inner_ty", ty),},
        discard
    ))(input)
}

fn bare_fn_ty(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "BareFnTy",
        {
            ("safety", safety),
            ("ext", parse_extern),
            ("generic_params", list(generic_param)),
            ("decl", fn_decl),
            ("decl_span", span),
        },
        discard
    ))(input)
}

fn mut_ty(input: &str) -> IResult<&str, ()> {
    parse_struct!(("MutTy", {("ty", ty), ("mutbl", mutability),}, discard))(input)
}

fn anon_const(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "AnonConst",
        {("id", node_id), ("value", expr),},
        discard
    ))(input)
}

fn expr(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "Expr",
        {
            ("id", node_id),
            ("kind", expr_kind),
            ("span", span),
            ("attrs", list(attribute)),
            ("tokens", option(lazy_attr_token_stream)),
        },
        discard
    ))(input)
}

#[allow(clippy::too_many_lines)]
fn expr_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Array", (list(expr),), discard),
        ("ConstBlock", (anon_const,), discard),
        ("Call", (expr, list(expr),), discard),
        ("MethodCall", (method_call,), discard),
        ("Tup", (list(expr),), discard),
        ("Binary", (bin_op, expr, expr,), discard),
        ("Unary", (un_op, expr,), discard),
        ("Lit", (lit,), discard),
        ("Cast", (expr, ty,), discard),
        ("Type", (expr, ty,), discard),
        ("Let", (pat, expr, span, recovered,), discard),
        ("If", (expr, block, option(expr),), discard),
        ("While", (expr, block, option(label),), discard),
        (
            "ForLoop",
            {
                ("pat", pat),
                ("iter", expr),
                ("body", block),
                ("label", option(label)),
                ("kind", for_loop_kind),
            },
            discard
        ),
        ("Loop", (block, option(label), span,), discard),
        ("Match", (expr, list(arm), match_kind,), discard),
        ("Closure", (closure,), discard),
        ("Block", (block, option(label),), discard),
        ("Gen", (capture_by, block, gen_block_kind, span,), discard),
        ("Await", (expr, span,), discard),
        ("TryBlock", (block,), discard),
        ("Assign", (expr, expr, span,), discard),
        ("AssignOp", (bin_op, expr, expr,), discard),
        ("Field", (expr, ident,), discard),
        ("Index", (expr, expr, span,), discard),
        ("Range", (option(expr), option(expr), range_limits,), discard),
        ("Underscore", [()]),
        ("Path", (option(q_self), path,), discard),
        ("AddrOf", (borrow_kind, mutability, expr,), discard),
        ("Break", (option(label), option(expr),), discard),
        ("Continue", (option(label),), discard),
        ("InlineAsm", (inline_asm,), discard),
        ("OffsetOf", (ty, list(ident),), discard),
        ("Struct", (struct_expr,), discard),
        ("Repeat", (expr, anon_const,), discard),
        ("Paren", (expr,), discard),
        ("Try", (expr,), discard),
        ("Yield", (option(expr),), discard),
        ("Yeet", (option(expr),), discard),
        ("Become", (expr,), discard),
        ("FormatArgs", (parse_format_args,), discard),
        ("IncludedBytes", (list(spaced(complete::digit1)),), discard),
        ("Ret", (option(expr),), discard),
        (
            "UnsafeBinderCast",
            (unsafe_binder_cast_kind, expr, option(ty),),
            discard
        ),
        ("Dummy", [()]),
    )(input)
}

fn recovered(input: &str) -> IResult<&str, &str> {
    spaced_tag("No")(input)
}

fn parse_format_args(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "FormatArgs",
        {
            ("span", span),
            ("template", list(format_args_piece)),
            ("arguments", format_arguments),
            (
                "uncooked_fmt_str",
                tuple_parser(tuple((field(lit_kind), field(spaced_string))))
            ),
        },
        discard
    ))(input)
}

fn format_arguments(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "FormatArguments",
        {
            ("arguments", list(format_argument)),
            ("num_unnamed_args", spaced(complete::digit1)),
            ("num_explicit_args", spaced(complete::digit1)),
            ("names", hashmap(spaced_string, spaced(complete::digit1))),
        },
        discard
    ))(input)
}

fn format_argument(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "FormatArgument",
        {("kind", format_argument_kind), ("expr", expr),},
        discard
    ))(input)
}

fn format_argument_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Normal", [()]),
        ("Named", (ident,), discard),
        ("Captured", (ident,), discard),
    )(input)
}

fn format_args_piece(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Literal", (spaced_string,), discard),
        ("Placeholder", (format_placeholder,), discard),
    )(input)
}

fn format_placeholder(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "FormatPlaceholder",
        {
            ("argument", format_arg_position),
            ("span", option(span)),
            ("format_trait", format_trait),
            ("format_options", format_options),
        },
        discard
    ))(input)
}

fn format_options(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "FormatOptions",
        {
            ("width", option(format_count)),
            ("precision", option(format_count)),
            ("alignment", option(format_alignment)),
            (
                "fill",
                option(delimited(
                    spaced_tag("'"),
                    complete::anychar,
                    spaced_tag("'"),
                ))
            ),
            ("sign", option(format_sign)),
            ("alternate", parse_bool),
            ("zero_pad", parse_bool),
            ("debug_hex", option(format_debug_hex)),
        },
        discard
    ))(input)
}

fn format_count(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Literal", (spaced(complete::digit1),), discard),
        ("Argument", (format_arg_position,), discard),
    )(input)
}

fn format_debug_hex(input: &str) -> IResult<&str, &str> {
    parse_enum!("Lower", "Upper",)(input)
}

fn format_sign(input: &str) -> IResult<&str, &str> {
    parse_enum!("Plus", "Minus",)(input)
}

fn format_alignment(input: &str) -> IResult<&str, &str> {
    parse_enum!("Left", "Right", "Center",)(input)
}

fn format_trait(input: &str) -> IResult<&str, &str> {
    parse_enum!(
        "Display", "Debug", "LowerExp", "UpperExp", "Octal", "Pointer", "Binary", "LowerHex",
        "UpperHex",
    )(input)
}

fn format_arg_position(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "FormatArgPosition",
        {
            (
                "index",
                result(spaced(complete::digit1), spaced(complete::digit1))
            ),
            ("kind", format_arg_position_kind),
            ("span", option(span)),
        },
        discard
    ))(input)
}

fn format_arg_position_kind(input: &str) -> IResult<&str, &str> {
    parse_enum!("Implicit", "Number", "Named",)(input)
}

fn unsafe_binder_cast_kind(_input: &str) -> IResult<&str, ()> {
    todo!()
}

fn inline_asm(_input: &str) -> IResult<&str, ()> {
    todo!()
}

fn struct_expr(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "StructExpr",
        {
            ("qself", option(q_self)),
            ("path", path),
            ("fields", list(expr_field)),
            ("rest", struct_rest),
        },
        discard
    ))(input)
}

fn struct_rest(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Base", (expr,), discard),
        ("Rest", (span,), discard),
        ("None", [()]),
    )(input)
}

fn expr_field(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "ExprField",
        {
            ("attrs", list(attribute)),
            ("id", node_id),
            ("span", span),
            ("ident", ident),
            ("expr", expr),
            ("is_shorthand", parse_bool),
            ("is_placeholder", parse_bool),
        },
        discard
    ))(input)
}

fn borrow_kind(input: &str) -> IResult<&str, &str> {
    parse_enum!("Ref", "Raw",)(input)
}

fn range_limits(input: &str) -> IResult<&str, &str> {
    parse_enum!("HalfOpen", "Closed",)(input)
}

fn gen_block_kind(input: &str) -> IResult<&str, &str> {
    parse_enum!("Async", "Gen", "AsyncGen",)(input)
}

fn closure(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "Closure",
        {
            ("binder", closure_binder),
            ("capture_clause", capture_by),
            ("constness", parse_const),
            ("coroutine_kind", option(coroutine_kind)),
            ("movability", movability),
            ("fn_decl", fn_decl),
            ("body", expr),
            ("fn_decl_span", span),
            ("fn_arg_span", span),
        },
        discard
    ))(input)
}

fn fn_decl(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "FnDecl",
        {("inputs", list(param)), ("output", fn_ret_ty),},
        discard
    ))(input)
}

fn fn_ret_ty(input: &str) -> IResult<&str, ()> {
    parse_enum!(("Default", (span,), discard), ("Ty", (ty,), discard),)(input)
}

fn param(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "Param",
        {
            ("attrs", list(attribute)),
            ("ty", ty),
            ("pat", pat),
            ("id", node_id),
            ("span", span),
            ("is_placeholder", parse_bool),
        },
        discard
    ))(input)
}

fn movability(input: &str) -> IResult<&str, &str> {
    parse_enum!("Static", "Movable",)(input)
}

fn coroutine_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        (
            "Async",
            {
                ("span", span),
                ("closure_id", node_id),
                ("return_impl_trait_id", node_id),
            },
            discard
        ),
        (
            "Gen",
            {
                ("span", span),
                ("closure_id", node_id),
                ("return_impl_trait_id", node_id),
            },
            discard
        ),
        (
            "AsyncGen",
            {
                ("span", span),
                ("closure_id", node_id),
                ("return_impl_trait_id", node_id),
            },
            discard
        ),
    )(input)
}

fn parse_const(input: &str) -> IResult<&str, ()> {
    parse_enum!(("No", [()]), ("Yes", (span,), discard),)(input)
}

fn capture_by(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Ref", [()]),
        ("Value", {("move_kw", span),}, discard),
    )(input)
}

fn closure_binder(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("NotPresent", [()]),
        (
            "NotPresent",
            {("span", span), ("generic_params", list(generic_param)),},
            discard
        ),
    )(input)
}

fn generic_param(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "GenericParam",
        {
            ("id", node_id),
            ("ident", ident),
            ("attrs", list(attribute)),
            ("bounds", list(generic_bound)),
            ("is_placeholder", parse_bool),
            ("kind", generic_param_kind),
            ("colon_span", option(span)),
        },
        discard
    ))(input)
}

fn generic_param_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Lifetime", [()]),
        ("Type", {("default", option(ty)),}, discard),
        (
            "Const",
            {
                ("ty", ty),
                ("kw_span", span),
                ("default", option(anon_const)),
            },
            discard
        ),
    )(input)
}

fn generic_bound(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Trait", (poly_trait_ref,), discard),
        ("Outlives", (annotated_lifetime,), discard),
        ("Use", (list(precise_capturing_arg), span,), discard),
    )(input)
}

fn precise_capturing_arg(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Lifetime", (annotated_lifetime,), discard),
        ("Arg", (path, node_id,), discard),
    )(input)
}

fn poly_trait_ref(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "PolyTraitRef",
        {
            ("bound_generic_params", list(generic_param)),
            ("modifiers", trait_bound_modifiers),
            ("trait_ref", trait_ref),
            ("span", span),
        },
        discard
    ))(input)
}

fn trait_bound_modifiers(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "TraitBoundModifiers",
        {
            ("constness", bound_constness),
            ("asyncness", bound_asyncness),
            ("polarity", bound_polarity),
        },
        discard
    ))(input)
}

fn bound_constness(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Never", [()]),
        ("Always", (span,), discard),
        ("Maybe", (span,), discard),
    )(input)
}

fn bound_asyncness(input: &str) -> IResult<&str, ()> {
    parse_enum!(("Normal", [()]), ("Async", (span,), discard),)(input)
}

fn bound_polarity(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Positive", [()]),
        ("Negative", (span,), discard),
        ("Maybe", (span,), discard),
    )(input)
}

fn trait_ref(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "TraitRef",
        {("path", path), ("ref_id", node_id),},
        discard
    ))(input)
}

fn match_kind(input: &str) -> IResult<&str, &str> {
    parse_enum!("Prefix", "Postfix",)(input)
}

fn arm(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "Arm",
        {
            ("attrs", list(attribute)),
            ("pat", pat),
            ("guard", option(expr)),
            ("body", option(expr)),
            ("span", span),
            ("id", node_id),
            ("is_placeholder", parse_bool),
        },
        discard
    ))(input)
}

fn for_loop_kind(input: &str) -> IResult<&str, &str> {
    parse_enum!("For", "ForAwait",)(input)
}

fn label(input: &str) -> IResult<&str, &str> {
    preceded(spaced_tag("label"), parend(is_not(")")))(input)
}

fn method_call(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "MethodCall",
        {
            ("seg", path_segment),
            ("receiver", expr),
            ("args", list(expr)),
            ("span", span),
        },
        discard
    ))(input)
}

fn bin_op(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "Spanned",
        {("node", bin_op_kind), ("span", span),},
        discard
    ))(input)
}

fn bin_op_kind(input: &str) -> IResult<&str, &str> {
    parse_enum!(
        "Add", "Sub", "Mul", "Div", "Rem", "And", "Or", "BitXor", "BitAnd", "BitOr", "Shl", "Shr",
        "Eq", "Lt", "Le", "Ne", "Ge", "Gt",
    )(input)
}

fn un_op(input: &str) -> IResult<&str, &str> {
    parse_enum!("Deref", "Not", "Neg",)(input)
}

fn pat(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "Pat",
        {
            ("id", node_id),
            ("kind", pat_kind),
            ("span", span),
            ("tokens", option(lazy_attr_token_stream)),
        },
        discard
    ))(input)
}

fn pat_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Wild", [()]),
        ("Ident", (binding_mode, ident, option(pat),), discard,),
        (
            "Struct",
            (option(q_self), path, list(pat_field), pat_fields_rest,),
            discard,
        ),
        ("TupleStruct", (option(q_self), path, list(pat),), discard,),
        ("Or", (list(pat),), discard),
        ("Path", (option(q_self), path,), discard),
        ("Tuple", (list(pat),), discard),
        ("Box", (pat,), discard),
        ("Deref", (pat,), discard),
        ("Ref", (pat, mutability,), discard),
        ("Expr", (expr,), discard),
        (
            "Range",
            (option(expr), option(expr), spanned_range_end,),
            discard,
        ),
        ("Slice", (list(pat),), discard),
        ("Rest", [()]),
        ("Never", [()]),
        ("Guard", (pat, expr,), discard),
        ("Paren", (pat,), discard),
    )(input)
}

fn binding_mode(input: &str) -> IResult<&str, ()> {
    parse_struct!(("BindingMode", (by_ref, mutability,), discard))(input)
}

fn by_ref(input: &str) -> IResult<&str, ()> {
    parse_enum!(("Yes", (mutability,), discard), ("No", [()]),)(input)
}

fn mutability(input: &str) -> IResult<&str, &str> {
    parse_enum!("Not", "Mut",)(input)
}

fn q_self(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "QSelf",
        {
            ("ty", ty),
            ("path_span", span),
            ("position", spaced(complete::digit1)),
        },
        discard
    ))(input)
}

fn pat_field(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "PatField",
        {
            ("ident", ident),
            ("pat", pat),
            ("is_shorthand", parse_bool),
            ("attrs", list(attribute)),
            ("id", node_id),
            ("span", span),
            ("is_placeholder", parse_bool),
        },
        discard
    ))(input)
}

fn pat_fields_rest(input: &str) -> IResult<&str, &str> {
    parse_enum!("Rest", "None",)(input)
}

fn spanned_range_end(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "Spanned",
        {("node", range_end), ("span", span),},
        discard
    ))(input)
}

fn range_end(input: &str) -> IResult<&str, ()> {
    parse_enum!(("Included", (range_syntax,), discard), ("Excluded", [()]),)(input)
}

fn range_syntax(input: &str) -> IResult<&str, &str> {
    parse_enum!("DotDotDot", "DotDotEq",)(input)
}

#[derive(Debug, Clone)]
struct Item<'a> {
    attrs: Vec<Attribute<'a>>,
    kind: ItemKind<'a>,
}

impl<'a> Item<'a> {
    fn new(attrs: Vec<Attribute<'a>>, kind: ItemKind<'a>) -> Self {
        Self { attrs, kind }
    }

    fn get_fn_attrs_for_span(&self, span: &str) -> Option<&[Attribute]> {
        match &self.kind {
            ItemKind::Fn(fun) => fun.matches_span(span).then_some(&self.attrs),
            ItemKind::Mod(module) => module
                .items
                .iter()
                .find_map(|item| item.get_fn_attrs_for_span(span)),
            ItemKind::Trait(trait_item) => trait_item
                .items
                .iter()
                .find_map(|assoc_item| assoc_item.get_fn_attrs_for_span(span)),
            ItemKind::Impl(impl_item) => impl_item
                .items
                .iter()
                .find_map(|assoc_item| assoc_item.get_fn_attrs_for_span(span)),
        }
    }
}

fn item(input: &str) -> IResult<&str, Option<Item>> {
    parse_struct!((
        "Item",
        {
            ("attrs", list(attribute)),
            ("id", node_id),
            ("span", span),
            ("vis", visibility),
            ("ident", ident),
            ("kind", item_kind),
            ("tokens", option(lazy_attr_token_stream)),
        },
        |(attrs, _, _, (), _, kind, _)| Some(Item::new(attrs, kind?))
    ))(input)
}

fn visibility(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "Visibility",
        {
            ("kind", visibility_kind),
            ("span", span),
            ("tokens", option(lazy_attr_token_stream)),
        },
        discard
    ))(input)
}

fn visibility_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Public", [()]),
        ("Inherited", [()]),
        (
            "Restricted",
            {("path", path), ("id", node_id), ("shorthand", parse_bool),},
            discard
        ),
    )(input)
}

#[derive(Debug, Clone)]
pub(crate) struct Span<'a>(&'a str);

pub(crate) fn span(input: &str) -> IResult<&str, Span> {
    map(is_not(",}"), |text: &str| {
        Span(
            text.trim_start()
                .rsplit_once(' ')
                .expect("malformed span")
                .0,
        )
    })(input)
}

fn modspans(input: &str) -> IResult<&str, Span> {
    parse_struct!((
        "ModSpans",
        {("inner_span", span), ("inject_use_span", span),},
        |(span, _)| span
    ))(input)
}

pub(crate) fn ident(input: &str) -> IResult<&str, &str> {
    is_not("),")(input)
}

#[derive(Debug, Clone)]
enum ItemKind<'a> {
    Fn(Fn<'a>),
    Mod(Mod<'a>),
    Trait(Trait<'a>),
    Impl(Impl<'a>),
}

fn item_kind(input: &str) -> IResult<&str, Option<ItemKind>> {
    parse_enum!(
        ("Fn", (parse_fn,), |(x,)| Some(ItemKind::Fn(x))),
        ("Mod", (safety, parse_mod,), |((), x)| Some(ItemKind::Mod(
            x
        ))),
        ("Trait", (parse_trait,), |(x,)| Some(ItemKind::Trait(x))),
        ("Impl", (parse_impl,), |(x,)| Some(ItemKind::Impl(x))),
        ("ExternCrate", (option(spaced_string),), none),
        ("Use", (use_tree,), none),
        ("Static", (static_item,), none),
        ("Const", (const_item,), none),
        ("ForeignMod", (foreign_mod,), none),
        ("GlobalAsm", (inline_asm,), none),
        ("TyAlias", (ty_alias,), none),
        ("Enum", (enum_def, generics,), none),
        ("Struct", (variant_data, generics,), none),
        ("Union", (variant_data, generics,), none),
        ("TraitAlias", (generics, list(generic_bound),), none),
        ("MacroDef", (macro_def,), none),
        ("Delegation", (parser_todo,), none),
    )(input)
}

fn macro_def(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "MacroDef",
        {("body", delim_args), ("macro_rules", parse_bool),},
        discard
    ))(input)
}

fn delim_args(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "DelimArgs",
        {
            ("dspan", delim_span),
            ("delim", delimiter),
            ("tokens", token_stream),
        },
        discard
    ))(input)
}

fn enum_def(input: &str) -> IResult<&str, ()> {
    parse_struct!(("EnumDef", {("variants", list(variant)),}, discard))(input)
}

fn variant(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "Variant",
        {
            ("attrs", list(attribute)),
            ("id", node_id),
            ("span", span),
            ("vis", visibility),
            ("ident", ident),
            ("data", variant_data),
            ("disr_expr", option(anon_const)),
            ("is_placeholder", parse_bool),
        },
        discard
    ))(input)
}

fn variant_data(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        (
            "Struct",
            {("fields", list(field_def)), ("recovered", spaced_tag("No")),},
            discard
        ),
        ("Tuple", (list(field_def), node_id,), discard),
        ("Unit", (node_id,), discard),
    )(input)
}

fn field_def(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "FieldDef",
        {
            ("attrs", list(attribute)),
            ("id", node_id),
            ("span", span),
            ("vis", visibility),
            ("safety", safety),
            ("ident", option(ident)),
            ("ty", ty),
            ("default", option(anon_const)),
            ("is_placeholder", parse_bool),
        },
        discard
    ))(input)
}

fn foreign_item_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Static", (static_item,), discard),
        ("Fn", (parse_fn,), discard),
        ("TyAlias", (ty_alias,), discard),
    )(input)
}

fn foreign_item(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "Item",
        {
            ("attrs", list(attribute)),
            ("id", node_id),
            ("span", span),
            ("vis", visibility),
            ("ident", ident),
            ("kind", foreign_item_kind),
            ("tokens", option(lazy_attr_token_stream)),
        },
        discard
    ))(input)
}

fn foreign_mod(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "ForeignMod",
        {
            ("extern_span", span),
            ("safety", safety),
            ("abi", option(str_lit)),
            ("items", list(foreign_item)),
        },
        discard
    ))(input)
}

fn static_item(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "StaticItem",
        {
            ("ty", ty),
            ("safety", safety),
            ("mutability", mutability),
            ("expr", option(expr)),
        },
        discard
    ))(input)
}

fn use_tree(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "UseTree",
        {("prefix", path), ("kind", use_tree_kind), ("span", span),},
        discard
    ))(input)
}

fn use_tree_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Simple", (option(ident),), discard),
        (
            "Nested",
            {
                (
                    "items",
                    list(tuple_parser(tuple((field(use_tree,), field(node_id,)))))
                ),
                ("span", span),
            },
            discard
        ),
        ("Glob", [()]),
    )(input)
}

#[derive(Debug, Clone)]
struct Fn<'a> {
    body: Option<Block<'a>>,
}

impl<'a> Fn<'a> {
    fn new(body: Option<Block<'a>>) -> Self {
        Self { body }
    }

    fn matches_span(&self, span: &str) -> bool {
        self.body
            .as_ref()
            .is_some_and(|body| span.contains(body.span.0))
    }
}

fn parse_fn(input: &str) -> IResult<&str, Fn> {
    parse_struct!((
        "Fn",
        {
            ("defaultness", defaultness),
            ("generics", generics),
            ("sig", fn_sig),
            ("body", option(block)),
        },
        |((), (), (), body)| Fn::new(body)
    ))(input)
}

fn fn_sig(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "FnSig",
        {("header", fn_header), ("decl", fn_decl), ("span", span),},
        discard
    ))(input)
}

fn fn_header(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "FnHeader",
        {
            ("safety", safety),
            ("coroutine_kind", option(coroutine_kind)),
            ("constness", parse_const),
            ("ext", parse_extern),
        },
        discard
    ))(input)
}

fn parse_extern(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("None", [()]),
        ("Implicit", (span,), discard),
        ("Explicit", (str_lit, span,), discard),
    )(input)
}

fn str_lit(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "StrLit",
        {
            ("symbol", spaced_string),
            ("suffix", option(spaced_string)),
            ("symbol_unescaped", spaced_string),
            ("style", str_style),
            ("span", span),
        },
        discard
    ))(input)
}

fn str_style(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Cooked", [()]),
        ("Raw", (spaced(complete::digit1),), discard),
    )(input)
}

fn generics(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "Generics",
        {
            ("params", list(generic_param)),
            ("where_clause", where_clause),
            ("span", span),
        },
        discard
    ))(input)
}

fn where_clause(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "WhereClause",
        {
            ("has_where_token", parse_bool),
            ("predicates", list(where_predicate)),
            ("span", span),
        },
        discard
    ))(input)
}

fn where_predicate(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "WherePredicate",
        {
            ("kind", where_predicate_kind),
            ("id", node_id),
            ("span", span),
        },
        discard
    ))(input)
}

fn where_predicate_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("BoundPredicate", (where_bound_predicate,), discard),
        ("RegionPredicate", (where_region_predicate,), discard),
    )(input)
}

fn where_bound_predicate(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "WhereBoundPredicate",
        {
            ("bound_generic_params", list(generic_param)),
            ("bounded_ty", ty),
            ("bounds", list(generic_bound)),
        },
        discard
    ))(input)
}

fn where_region_predicate(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "WhereRegionPredicate",
        {
            ("lifetime", annotated_lifetime),
            ("bounds", list(generic_bound)),
        },
        discard
    ))(input)
}

fn defaultness(input: &str) -> IResult<&str, ()> {
    parse_enum!(("Default", (span,), discard), ("Final", [()]),)(input)
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
    parse_struct!((
        "Block",
        {
            ("stmts", list(stmt)),
            ("id", node_id),
            ("rules", block_check_mode),
            ("span", span),
            ("tokens", option(lazy_attr_token_stream)),
            ("could_be_bare_literal", parse_bool),
        },
        |(_, _, (), span, _, _)| Block::new(span)
    ))(input)
}

fn block_check_mode(input: &str) -> IResult<&str, ()> {
    parse_enum!(("Default", [()]), ("Unsafe", (unsafe_source,), discard),)(input)
}

fn unsafe_source(input: &str) -> IResult<&str, &str> {
    parse_enum!("CompilerGenerated", "UserProvided",)(input)
}

fn stmt(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "Stmt",
        {("id", node_id), ("kind", stmt_kind), ("span", span),},
        discard
    ))(input)
}

fn stmt_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Let", (local,), discard),
        ("Item", (item,), discard),
        ("Expr", (expr,), discard),
        ("Semi", (expr,), discard),
        ("Empty", [()]),
    )(input)
}

fn local(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "Local",
        {
            ("id", node_id),
            ("pat", pat),
            ("ty", option(ty)),
            ("kind", local_kind),
            ("span", span),
            ("colon_sp", option(span)),
            ("attrs", list(attribute)),
            ("tokens", option(lazy_attr_token_stream)),
        },
        discard
    ))(input)
}

fn local_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Decl", [()]),
        ("Init", (expr,), discard),
        ("InitElse", (expr, block,), discard),
    )(input)
}

#[derive(Debug, Clone)]
struct Mod<'a> {
    items: Vec<Item<'a>>,
}

impl<'a> Mod<'a> {
    fn new(items: Vec<Item<'a>>) -> Self {
        Self { items }
    }
}

fn parse_mod(input: &str) -> IResult<&str, Mod> {
    parse_struct!((
        "Loaded",
        (list(item), inline, modspans, result(unit, unit),),
        |(items, _, _, _)| Mod::new(items.into_iter().flatten().collect()),
    ))(input)
}

fn inline(input: &str) -> IResult<&str, &str> {
    parse_enum!("Yes", "No",)(input)
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
    parse_struct!((
        "Trait",
        {
            ("safety", safety),
            ("is_auto", is_auto),
            ("generics", generics),
            ("bounds", list(generic_bound)),
            ("items", list(assoc_item)),
        },
        |((), _, (), _, items)| Trait::new(items)
    ))(input)
}

fn is_auto(input: &str) -> IResult<&str, &str> {
    parse_enum!("Yes", "No",)(input)
}

#[derive(Debug, Clone)]
struct AssocItem<'a> {
    attrs: Vec<Attribute<'a>>,
    assoc_fn: Option<Fn<'a>>,
}

impl<'a> AssocItem<'a> {
    fn new(attrs: Vec<Attribute<'a>>, assoc_fn: Option<Fn<'a>>) -> Self {
        Self { attrs, assoc_fn }
    }

    fn get_fn_attrs_for_span(&self, span: &str) -> Option<&[Attribute<'_>]> {
        self.assoc_fn
            .as_ref()
            .is_some_and(|assoc_fn| assoc_fn.matches_span(span))
            .then_some(&self.attrs)
    }
}

fn assoc_item(input: &str) -> IResult<&str, AssocItem> {
    parse_struct!((
        "Item",
        {
            ("attrs", list(attribute)),
            ("id", node_id),
            ("span", span),
            ("vis", visibility),
            ("ident", ident),
            ("kind", assoc_item_kind),
            ("tokens", option(lazy_attr_token_stream)),
        },
        |(attrs, _, _, (), _, kind, _)| AssocItem::new(attrs, kind)
    ))(input)
}

fn assoc_item_kind(input: &str) -> IResult<&str, Option<Fn<'_>>> {
    parse_enum!(
        ("Fn", (parse_fn,), |(x,)| Some(x)),
        ("Const", (const_item,), none),
        ("Type", (ty_alias,), none),
        ("MacCall", (parser_todo,), none),
        ("Delegation", (parser_todo,), none),
        ("DelegationMac", (parser_todo,), none),
    )(input)
}

fn const_item(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "ConstItem",
        {
            ("defaultness", defaultness),
            ("generics", generics),
            ("ty", ty),
            ("expr", option(expr)),
        },
        discard
    ))(input)
}

fn ty_alias(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "TyAlias",
        {
            ("defaultness", defaultness),
            ("generics", generics),
            ("where_clauses", ty_alias_where_clauses),
            ("bounds", list(generic_bound)),
            ("ty", option(ty)),
        },
        discard
    ))(input)
}

fn ty_alias_where_clauses(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "TyAliasWhereClauses",
        {
            ("before", ty_alias_where_clause),
            ("after", ty_alias_where_clause),
            ("split", spaced(complete::digit1)),
        },
        discard
    ))(input)
}

fn ty_alias_where_clause(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "TyAliasWhereClause",
        {("has_where_token", parse_bool), ("span", span),},
        discard
    ))(input)
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
    parse_struct!((
        "Impl",
        {
            ("defaultness", defaultness),
            ("safety", safety),
            ("generics", generics),
            ("constness", parse_const),
            ("polarity", impl_polarity),
            ("of_trait", option(trait_ref)),
            ("self_ty", ty),
            ("items", list(assoc_item)),
        },
        |((), (), (), (), (), _, (), items)| Impl::new(items)
    ))(input)
}

fn impl_polarity(input: &str) -> IResult<&str, ()> {
    parse_enum!(("\"positive\"", [()]), ("Negative", (span,), discard),)(input)
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod test {
    use super::*;

    #[test]
    fn test_lifetime() {
        annotated_lifetime("lifetime(63: 'a)").unwrap();
    }
}
