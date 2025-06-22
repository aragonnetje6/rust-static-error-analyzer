use nom::{branch::alt, character::complete::digit0, sequence::tuple, IResult};

use crate::{parse_enum, parse_struct};

use super::{
    ast::{attr_style, ident, span},
    utils::{
        discard, field, list, option, parse_bool, parser_todo, spaced, spaced_string, spaced_tag,
        struct_field, struct_parser, tuple_struct_parser, unit_struct_parser,
    },
};

pub(crate) fn lazy_attr_token_stream(input: &str) -> IResult<&str, ()> {
    parse_struct!(("LazyAttrTokenStream", (attr_token_stream,), discard))(input)
}

fn attr_token_stream(input: &str) -> IResult<&str, ()> {
    parse_struct!(("AttrTokenStream", (list(attr_token_tree),), discard))(input)
}

fn attr_token_tree(input: &str) -> IResult<&str, ()> {
    alt((
        parse_struct!(("Token", (token, spacing,), discard)),
        parse_struct!((
            "Delimited",
            (delim_span, delim_spacing, delimiter, attr_token_stream,),
            discard
        )),
        parse_struct!(("AttrsTarget", (parser_todo,), discard)),
    ))(input)
}

fn delim_spacing(input: &str) -> IResult<&str, ()> {
    parse_struct!(("DelimSpacing", {("open", spacing), ("close", spacing),}, discard))(input)
}

fn spacing(input: &str) -> IResult<&str, &str> {
    parse_enum!("Alone", "JointHidden", "Joint",)(input)
}

fn token(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "Token",
        {
            ("kind", token_kind),
            ("span", span),
        },
        discard
    ))(input)
}

fn token_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Lt", [()]),
        ("Le", [()]),
        ("EqEq", [()]),
        ("Ne", [()]),
        ("Eq", [()]),
        ("Ge", [()]),
        ("Gt", [()]),
        ("AndAnd", [()]),
        ("OrOr", [()]),
        ("Not", [()]),
        ("Tilde", [()]),
        ("BinOpEq", (bin_op_token,), discard),
        ("BinOp", (bin_op_token,), discard),
        ("At", [()]),
        ("DotDotDot", [()]),
        ("DotDotEq", [()]),
        ("DotDot", [()]),
        ("Dot", [()]),
        ("Comma", [()]),
        ("Semi", [()]),
        ("Colon", [()]),
        ("PathSep", [()]),
        ("RArrow", [()]),
        ("LArrow", [()]),
        ("FatArrow", [()]),
        ("Pound", [()]),
        ("Dollar", [()]),
        ("Question", [()]),
        ("SingleQuote", [()]),
        ("OpenDelim", (delimiter,), discard),
        ("CloseDelim", (delimiter,), discard),
        ("Literal", (lit,), discard),
        ("Ident", (spaced_string, ident_is_raw,), discard),
        ("NtIdent", (ident, ident_is_raw,), discard),
        ("Lifetime", (spaced_string, ident_is_raw,), discard),
        ("NtLifetime", (ident, ident_is_raw,), discard),
        ("Interpolated", (nonterminal,), discard),
        (
            "DocComment",
            (comment_kind, attr_style, spaced_string,),
            discard
        ),
        ("Eof", [()]),
    )(input)
}

fn nonterminal(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("NtItem", (dotdot,), discard),
        ("NtBlock", (dotdot,), discard),
        ("NtStmt", (dotdot,), discard),
        ("NtPat", (dotdot,), discard),
        ("NtExpr", (dotdot,), discard),
        ("NtTy", (dotdot,), discard),
        ("NtLiteral", (dotdot,), discard),
        ("NtMeta", (dotdot,), discard),
        ("NtPath", (dotdot,), discard),
        ("NtVis", (dotdot,), discard),
    )(input)
}

fn dotdot(input: &str) -> IResult<&str, &str> {
    spaced_tag("..")(input)
}

fn ident_is_raw(input: &str) -> IResult<&str, &str> {
    parse_enum!("No", "Yes",)(input)
}

fn bin_op_token(input: &str) -> IResult<&str, &str> {
    parse_enum!("Plus", "Minus", "Star", "Slash", "Percent", "Caret", "And", "Or", "Shl", "Shr",)(
        input,
    )
}

pub(crate) fn delim_span(input: &str) -> IResult<&str, ()> {
    parse_struct!(("DelimSpan", {("open", span), ("close", span),}, discard))(input)
}

pub(crate) fn delimiter(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Parenthesis", [()]),
        ("Brace", [()]),
        ("Bracket", [()]),
        ("Invisible", (invisible_origin,), discard),
    )(input)
}

fn invisible_origin(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("MetaVar", (meta_var_kind,), discard),
        ("ProcMacro", [()]),
        ("FlattenToken", [()]),
    )(input)
}

fn meta_var_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Pat", (nt_pat_kind,), discard),
        (
            "Expr",
            {
                ("kind", nt_expr_kind),
                ("can_begin_literal_maybe_minus", parse_bool),
                ("can_begin_string_literal", parse_bool),
            },
            discard
        ),
        ("Item", [()]),
        ("Block", [()]),
        ("Stmt", [()]),
        ("Ty", [()]),
        ("Ident", [()]),
        ("Lifetime", [()]),
        ("Literal", [()]),
        ("Meta", [()]),
        ("Path", [()]),
        ("Vis", [()]),
        ("TT", [()]),
    )(input)
}

fn nt_expr_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Expr2021", { ("inferred", parse_bool), }, discard),
        ("Expr", [()]),
    )(input)
}

fn nt_pat_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("PatParam", {("inferred", parse_bool),}, discard),
        ("PatWithOr", [()]),
    )(input)
}

pub(crate) fn comment_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(("Line", [()]), ("Block", [()]),)(input)
}

pub(crate) fn lit(input: &str) -> IResult<&str, ()> {
    parse_struct!((
        "Lit",
        {
            ("kind", lit_kind),
            ("symbol", spaced_string),
            ("suffix", option(spaced_string)),
        },
        discard
    ))(input)
}

pub(crate) fn lit_kind(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Bool", [()]),
        ("Char", [()]),
        ("Integer", [()]),
        ("Float", [()]),
        ("StrRaw", (spaced(digit0),), discard),
        ("ByteStrRaw", (spaced(digit0),), discard),
        ("ByteStr", [()]),
        ("Str", [()]),
        ("CStrRaw", (spaced(digit0),), discard),
        ("CStr", [()]),
        ("Byte", [()]),
    )(input)
}

pub(crate) fn token_stream(input: &str) -> IResult<&str, ()> {
    parse_struct!(("TokenStream", (list(token_tree),), discard))(input)
}

fn token_tree(input: &str) -> IResult<&str, ()> {
    parse_enum!(
        ("Token", (token, spacing,), discard),
        (
            "Delimited",
            (delim_span, delim_spacing, delimiter, token_stream,),
            discard
        ),
    )(input)
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod test {
    use super::*;

    #[test]
    fn test_token() {
        token(
r#"Token{kind:Literal(Lit{kind:Str,symbol:"https://docs.rs/syn/2.0.96",suffix:None,},),span: src/lib.rs:252:24: 252:52(#0),},"#,        )
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
