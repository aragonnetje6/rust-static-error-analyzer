use nom::{branch::alt, character::complete::digit0, sequence::tuple, IResult};

use super::{
    ast::{attr_style, ident, span},
    utils::{
        discard, field, list, option, parse_bool, parser_todo, spaced, spaced_string, spaced_tag,
        struct_field, struct_parser, tuple_struct_parser, unit_struct_parser,
    },
};

pub(crate) fn lazy_attr_token_stream(input: &str) -> IResult<&str, ()> {
    tuple_struct_parser("LazyAttrTokenStream", attr_token_stream, discard)(input)
}

fn attr_token_stream(input: &str) -> IResult<&str, ()> {
    tuple_struct_parser("AttrTokenStream", list(attr_token_tree), discard)(input)
}

fn attr_token_tree(input: &str) -> IResult<&str, ()> {
    alt((
        tuple_struct_parser("Token", tuple((field(token), field(spacing))), discard),
        tuple_struct_parser(
            "Delimited",
            tuple((
                field(delim_span),
                field(delim_spacing),
                field(delimiter),
                field(attr_token_stream),
            )),
            discard,
        ),
        tuple_struct_parser("AttrsTarget", field(parser_todo), discard),
    ))(input)
}

fn delim_spacing(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "DelimSpacing",
        tuple((
            struct_field("open", spacing),
            struct_field("close", spacing),
        )),
        discard,
    )(input)
}

fn spacing(input: &str) -> IResult<&str, &str> {
    alt((
        spaced_tag("Alone"),
        spaced_tag("JointHidden"),
        spaced_tag("Joint"),
    ))(input)
}

fn token(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "Token",
        tuple((struct_field("kind", token_kind), struct_field("span", span))),
        discard,
    )(input)
}

fn token_kind(input: &str) -> IResult<&str, ()> {
    alt((
        unit_struct_parser("Lt", ()),
        unit_struct_parser("Le", ()),
        unit_struct_parser("EqEq", ()),
        unit_struct_parser("Ne", ()),
        unit_struct_parser("Eq", ()),
        unit_struct_parser("Ge", ()),
        unit_struct_parser("Gt", ()),
        unit_struct_parser("AndAnd", ()),
        unit_struct_parser("OrOr", ()),
        unit_struct_parser("Not", ()),
        unit_struct_parser("Tilde", ()),
        tuple_struct_parser("BinOpEq", field(bin_op_token), discard),
        tuple_struct_parser("BinOp", field(bin_op_token), discard),
        unit_struct_parser("At", ()),
        unit_struct_parser("DotDotDot", ()),
        unit_struct_parser("DotDotEq", ()),
        unit_struct_parser("DotDot", ()),
        unit_struct_parser("Dot", ()),
        unit_struct_parser("Comma", ()),
        unit_struct_parser("Semi", ()),
        alt((
            unit_struct_parser("Colon", ()),
            unit_struct_parser("PathSep", ()),
            unit_struct_parser("RArrow", ()),
            unit_struct_parser("LArrow", ()),
            unit_struct_parser("FatArrow", ()),
            unit_struct_parser("Pound", ()),
            unit_struct_parser("Dollar", ()),
            unit_struct_parser("Question", ()),
            unit_struct_parser("SingleQuote", ()),
            tuple_struct_parser("OpenDelim", field(delimiter), discard),
            tuple_struct_parser("CloseDelim", field(delimiter), discard),
            tuple_struct_parser("Literal", field(lit), discard),
            tuple_struct_parser(
                "Ident",
                tuple((field(spaced_string), field(ident_is_raw))),
                discard,
            ),
            tuple_struct_parser(
                "NtIdent",
                tuple((field(ident), field(ident_is_raw))),
                discard,
            ),
            tuple_struct_parser(
                "Lifetime",
                tuple((field(spaced_string), field(ident_is_raw))),
                discard,
            ),
            tuple_struct_parser(
                "NtLifetime",
                tuple((field(ident), field(ident_is_raw))),
                discard,
            ),
            tuple_struct_parser("Interpolated", field(nonterminal), discard),
            tuple_struct_parser(
                "DocComment",
                tuple((field(comment_kind), field(attr_style), field(spaced_string))),
                discard,
            ),
            unit_struct_parser("Eof", ()),
        )),
    ))(input)
}

fn nonterminal(input: &str) -> IResult<&str, ()> {
    alt((
        tuple_struct_parser("NtItem", dotdot, discard),
        tuple_struct_parser("NtBlock", dotdot, discard),
        tuple_struct_parser("NtStmt", dotdot, discard),
        tuple_struct_parser("NtPat", dotdot, discard),
        tuple_struct_parser("NtExpr", dotdot, discard),
        tuple_struct_parser("NtTy", dotdot, discard),
        tuple_struct_parser("NtLiteral", dotdot, discard),
        tuple_struct_parser("NtMeta", dotdot, discard),
        tuple_struct_parser("NtPath", dotdot, discard),
        tuple_struct_parser("NtVis", dotdot, discard),
    ))(input)
}

fn dotdot(input: &str) -> IResult<&str, &str> {
    spaced_tag("..")(input)
}

fn ident_is_raw(input: &str) -> IResult<&str, &str> {
    alt((spaced_tag("No"), spaced_tag("Yes")))(input)
}

fn bin_op_token(input: &str) -> IResult<&str, &str> {
    alt((
        spaced_tag("Plus"),
        spaced_tag("Minus"),
        spaced_tag("Star"),
        spaced_tag("Slash"),
        spaced_tag("Percent"),
        spaced_tag("Caret"),
        spaced_tag("And"),
        spaced_tag("Or"),
        spaced_tag("Shl"),
        spaced_tag("Shr"),
    ))(input)
}

pub(crate) fn delim_span(input: &str) -> IResult<&str, ()> {
    struct_parser(
        "DelimSpan",
        tuple((struct_field("open", span), struct_field("close", span))),
        discard,
    )(input)
}

pub(crate) fn delimiter(input: &str) -> IResult<&str, ()> {
    alt((
        unit_struct_parser("Parenthesis", ()),
        unit_struct_parser("Brace", ()),
        unit_struct_parser("Bracket", ()),
        tuple_struct_parser("Invisible", field(invisible_origin), discard),
    ))(input)
}

fn invisible_origin(input: &str) -> IResult<&str, ()> {
    alt((
        tuple_struct_parser("MetaVar", field(meta_var_kind), discard),
        unit_struct_parser("ProcMacro", ()),
        unit_struct_parser("FlattenToken", ()),
    ))(input)
}

fn meta_var_kind(input: &str) -> IResult<&str, ()> {
    alt((
        tuple_struct_parser("Pat", field(nt_pat_kind), discard),
        struct_parser(
            "Expr",
            tuple((
                struct_field("kind", nt_expr_kind),
                struct_field("can_begin_literal_maybe_minus", parse_bool),
                struct_field("can_begin_string_literal", parse_bool),
            )),
            discard,
        ),
        unit_struct_parser("Item", ()),
        unit_struct_parser("Block", ()),
        unit_struct_parser("Stmt", ()),
        unit_struct_parser("Ty", ()),
        unit_struct_parser("Ident", ()),
        unit_struct_parser("Lifetime", ()),
        unit_struct_parser("Literal", ()),
        unit_struct_parser("Meta", ()),
        unit_struct_parser("Path", ()),
        unit_struct_parser("Vis", ()),
        unit_struct_parser("TT", ()),
    ))(input)
}

fn nt_expr_kind(input: &str) -> IResult<&str, ()> {
    alt((
        struct_parser("Expr2021", struct_field("inferred", parse_bool), discard),
        unit_struct_parser("Expr", ()),
    ))(input)
}

fn nt_pat_kind(input: &str) -> IResult<&str, ()> {
    alt((
        struct_parser("PatParam", struct_field("inferred", parse_bool), discard),
        unit_struct_parser("PatWithOr", ()),
    ))(input)
}

pub(crate) fn comment_kind(input: &str) -> IResult<&str, ()> {
    alt((
        unit_struct_parser("Line", ()),
        unit_struct_parser("Block", ()),
    ))(input)
}

pub(crate) fn lit(input: &str) -> IResult<&str, ()> {
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

pub(crate) fn lit_kind(input: &str) -> IResult<&str, ()> {
    alt((
        unit_struct_parser("Bool", ()),
        unit_struct_parser("Char", ()),
        unit_struct_parser("Integer", ()),
        unit_struct_parser("Float", ()),
        tuple_struct_parser("StrRaw", field(spaced(digit0)), discard),
        tuple_struct_parser("ByteStrRaw", field(spaced(digit0)), discard),
        unit_struct_parser("ByteStr", ()),
        unit_struct_parser("Str", ()),
        tuple_struct_parser("CStrRaw", field(spaced(digit0)), discard),
        unit_struct_parser("CStr", ()),
        unit_struct_parser("Byte", ()),
    ))(input)
}

pub(crate) fn token_stream(input: &str) -> IResult<&str, ()> {
    tuple_struct_parser("TokenStream", field(list(token_tree)), discard)(input)
}

fn token_tree(input: &str) -> IResult<&str, ()> {
    alt((
        tuple_struct_parser("Token", tuple((field(token), field(spacing))), discard),
        tuple_struct_parser(
            "Delimited",
            tuple((
                field(delim_span),
                field(delim_spacing),
                field(delimiter),
                field(token_stream),
            )),
            discard,
        ),
    ))(input)
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod test {
    use super::*;

    #[test]
    fn test_token() {
        token(
r#"Token{kind:Literal(Lit{kind:Str,symbol:"https://docs.rs/syn/2.0.96",suffix:None,},),span:src/lib.rs:252:24:252:52(#0),},"#,        )
        .unwrap();
        token(r"Token { kind: Pound, span: src/lib.rs:252:1: 252:2 (#0) }").unwrap();
    }

    #[test]
    fn test_token_tree() {
        token_tree(
            r#"Token(Token { kind: Literal(Lit { kind: Str, symbol: "https://docs.rs/syn/2.0.96", suffix: None, },),span: src/lib.rs:252:24: 252:52 (#0),},JointHidden,)"#,
        )
        .unwrap();
        token_tree(r"Token(Token { kind: Pound, span: src/lib.rs:252:1: 252:2 (#0) }, Joint)")
            .unwrap();
    }

    #[test]
    fn test_spacing() {
        field(spacing)("Joint").unwrap();
        field(spacing)("JointHidden").unwrap();
        field(spacing)("Joint)").unwrap();
    }
}
