use rustc_hir::{Block, BodyId, Expr, ExprKind, LetExpr, LetStmt, QPath, StmtKind, StructTailExpr};
use rustc_middle::ty::TyCtxt;

use crate::graphs::PanicInfo;

use super::create_graph::get_node_kind_from_path;

pub fn get_panic_info_local(context: TyCtxt<'_>, body_id: BodyId) -> PanicInfo {
    let outer_expr = context.hir().body(body_id).value;
    let explicit_invocation = find_panic_in_expr(context, outer_expr);
    let catches = find_catches_in_expr(context, outer_expr);
    PanicInfo::new(explicit_invocation, false, catches)
}

fn find_catches_in_expr(context: TyCtxt<'_>, outer_expr: &Expr<'_>) -> Vec<String> {
    match outer_expr.kind {
        ExprKind::Call(expr, exprs) => {
            if expr_is_catch(expr) {
                exprs
                    .iter()
                    .filter_map(|expr| get_label(context, expr))
                    .collect()
            } else {
                exprs
                    .iter()
                    .chain([expr])
                    .flat_map(|expr| find_catches_in_expr(context, expr))
                    .collect()
            }
        }
        ExprKind::MethodCall(_, _, exprs, _) | ExprKind::Array(exprs) | ExprKind::Tup(exprs) => {
            exprs
                .iter()
                .flat_map(|expr| find_catches_in_expr(context, expr))
                .collect()
        }
        ExprKind::If(expr, expr1, Some(expr2)) => [expr, expr1, expr2]
            .iter()
            .flat_map(|expr| find_catches_in_expr(context, expr))
            .collect(),
        ExprKind::Match(expr, arms, _) => arms
            .iter()
            .map(|arm| arm.body)
            .chain([expr])
            .flat_map(|expr| find_catches_in_expr(context, expr))
            .collect(),
        ExprKind::Binary(_, expr, expr1)
        | ExprKind::If(expr, expr1, None)
        | ExprKind::Assign(expr, expr1, _)
        | ExprKind::AssignOp(_, expr, expr1)
        | ExprKind::Index(expr, expr1, _) => [expr, expr1]
            .iter()
            .flat_map(|expr| find_catches_in_expr(context, expr))
            .collect(),
        ExprKind::Loop(block, _, _, _) | ExprKind::Block(block, _) => {
            find_catches_in_block(context, block)
        }
        ExprKind::Struct(_, expr_fields, StructTailExpr::Base(tail_expr)) => expr_fields
            .iter()
            .map(|expr_field| expr_field.expr)
            .chain([tail_expr])
            .flat_map(|expr| find_catches_in_expr(context, expr))
            .collect(),
        ExprKind::Struct(
            _,
            expr_fields,
            StructTailExpr::None | StructTailExpr::DefaultFields(..),
        ) => expr_fields
            .iter()
            .flat_map(|expr_field| find_catches_in_expr(context, expr_field.expr))
            .collect(),
        ExprKind::Unary(_, expr)
        | ExprKind::Cast(expr, _)
        | ExprKind::Type(expr, _)
        | ExprKind::DropTemps(expr)
        | ExprKind::Let(&LetExpr { init: expr, .. })
        | ExprKind::Field(expr, _)
        | ExprKind::AddrOf(_, _, expr)
        | ExprKind::Break(_, Some(expr))
        | ExprKind::Ret(Some(expr))
        | ExprKind::Become(expr)
        | ExprKind::Repeat(expr, _)
        | ExprKind::Yield(expr, _) => find_catches_in_expr(context, expr),
        ExprKind::Path(..)
        | ExprKind::Closure(..)
        | ExprKind::ConstBlock(..)
        | ExprKind::Lit(..)
        | ExprKind::Break(_, None)
        | ExprKind::Continue(..)
        | ExprKind::Ret(None)
        | ExprKind::InlineAsm(..)
        | ExprKind::OffsetOf(..)
        | ExprKind::UnsafeBinderCast(..)
        | ExprKind::Err(..) => Vec::new(),
    }
}

fn get_label(context: TyCtxt<'_>, expr: &Expr<'_>) -> Option<String> {
    match expr.kind {
        ExprKind::Path(qpath) => {
            get_node_kind_from_path(context, qpath).map(|(node_kind, _)| node_kind.def_id())
        }
        ExprKind::Closure(closure) => Some(closure.def_id.to_def_id()),
        ExprKind::ConstBlock(..)
        | ExprKind::Array(..)
        | ExprKind::Call(..)
        | ExprKind::MethodCall(..)
        | ExprKind::Tup(..)
        | ExprKind::Binary(..)
        | ExprKind::Unary(..)
        | ExprKind::Lit(..)
        | ExprKind::Cast(..)
        | ExprKind::Type(..)
        | ExprKind::DropTemps(..)
        | ExprKind::Let(..)
        | ExprKind::If(..)
        | ExprKind::Loop(..)
        | ExprKind::Match(..)
        | ExprKind::Block(..)
        | ExprKind::Assign(..)
        | ExprKind::AssignOp(..)
        | ExprKind::Field(..)
        | ExprKind::Index(..)
        | ExprKind::AddrOf(..)
        | ExprKind::Break(..)
        | ExprKind::Continue(..)
        | ExprKind::Ret(..)
        | ExprKind::Become(..)
        | ExprKind::InlineAsm(..)
        | ExprKind::OffsetOf(..)
        | ExprKind::Struct(..)
        | ExprKind::Repeat(..)
        | ExprKind::Yield(..)
        | ExprKind::UnsafeBinderCast(..)
        | ExprKind::Err(..) => None,
    }
    .map(|def_id| context.def_path_str(def_id))
}

fn find_catches_in_block(context: TyCtxt<'_>, block: &Block<'_>) -> Vec<String> {
    block
        .stmts
        .iter()
        .flat_map(|statement| match statement.kind {
            StmtKind::Let(&LetStmt { init: None, .. }) | StmtKind::Item(..) => Vec::new(),
            StmtKind::Let(&LetStmt {
                init: Some(expr), ..
            })
            | StmtKind::Expr(expr)
            | StmtKind::Semi(expr) => find_catches_in_expr(context, expr),
        })
        .chain(if let Some(expr) = block.expr {
            find_catches_in_expr(context, expr)
        } else {
            Vec::new()
        })
        .collect()
}

fn find_panic_in_expr(context: TyCtxt<'_>, outer_expr: &Expr<'_>) -> bool {
    match outer_expr.kind {
        ExprKind::Path(qpath) => is_panic(qpath),
        ExprKind::Call(expr, exprs) => exprs
            .iter()
            .chain([expr])
            .any(|expr| find_panic_in_expr(context, expr)),
        ExprKind::MethodCall(_, _, exprs, _) | ExprKind::Array(exprs) | ExprKind::Tup(exprs) => {
            exprs.iter().any(|expr| find_panic_in_expr(context, expr))
        }
        ExprKind::If(expr, expr1, Some(expr2)) => {
            find_panic_in_expr(context, expr)
                || find_panic_in_expr(context, expr1)
                || find_panic_in_expr(context, expr2)
        }
        ExprKind::Match(expr, arms, _) => arms
            .iter()
            .map(|arm| arm.body)
            .chain([expr])
            .any(|expr| find_panic_in_expr(context, expr)),
        ExprKind::Binary(_, expr, expr1)
        | ExprKind::If(expr, expr1, None)
        | ExprKind::Assign(expr, expr1, _)
        | ExprKind::AssignOp(_, expr, expr1)
        | ExprKind::Index(expr, expr1, _) => {
            find_panic_in_expr(context, expr) || find_panic_in_expr(context, expr1)
        }
        ExprKind::Loop(block, _, _, _) | ExprKind::Block(block, _) => {
            find_panic_in_block(context, block)
        }
        ExprKind::Struct(_, expr_fields, StructTailExpr::Base(tail_expr)) => expr_fields
            .iter()
            .map(|expr_field| expr_field.expr)
            .chain([tail_expr])
            .any(|expr| find_panic_in_expr(context, expr)),
        ExprKind::Struct(
            _,
            expr_fields,
            StructTailExpr::None | StructTailExpr::DefaultFields(..),
        ) => expr_fields
            .iter()
            .map(|expr_field| expr_field.expr)
            .any(|expr| find_panic_in_expr(context, expr)),
        ExprKind::Unary(_, expr)
        | ExprKind::Cast(expr, _)
        | ExprKind::Type(expr, _)
        | ExprKind::DropTemps(expr)
        | ExprKind::Let(&LetExpr { init: expr, .. })
        | ExprKind::Field(expr, _)
        | ExprKind::AddrOf(_, _, expr)
        | ExprKind::Break(_, Some(expr))
        | ExprKind::Ret(Some(expr))
        | ExprKind::Become(expr)
        | ExprKind::Repeat(expr, _)
        | ExprKind::Yield(expr, _) => find_panic_in_expr(context, expr),
        ExprKind::Closure(..)
        | ExprKind::ConstBlock(..)
        | ExprKind::Lit(..)
        | ExprKind::Break(_, None)
        | ExprKind::Continue(..)
        | ExprKind::Ret(None)
        | ExprKind::InlineAsm(..)
        | ExprKind::OffsetOf(..)
        | ExprKind::UnsafeBinderCast(..)
        | ExprKind::Err(..) => false,
    }
}

fn is_panic(qpath: QPath) -> bool {
    if let QPath::Resolved(None, path) = qpath {
        if path
            .segments
            .get(path.segments.len().saturating_sub(2))
            .is_some_and(|segment| segment.ident.as_str().matches("panicking").next().is_some())
        {
            return true;
        }
    }
    false
}

fn expr_is_catch(expr: &Expr<'_>) -> bool {
    if let ExprKind::Path(qpath) = expr.kind {
        is_catch(qpath)
    } else {
        false
    }
}

fn is_catch(qpath: QPath) -> bool {
    if let QPath::Resolved(None, path) = qpath {
        if path.segments.last().is_some_and(|segment| {
            segment
                .ident
                .as_str()
                .matches("catch_unwind")
                .next()
                .is_some()
        }) {
            return true;
        }
    }
    false
}

fn find_panic_in_block(context: TyCtxt<'_>, block: &Block<'_>) -> bool {
    for statement in block.stmts {
        match statement.kind {
            StmtKind::Let(&LetStmt { init: None, .. }) | StmtKind::Item(..) => {}
            StmtKind::Let(&LetStmt {
                init: Some(expr), ..
            })
            | StmtKind::Expr(expr)
            | StmtKind::Semi(expr) => {
                if find_panic_in_expr(context, expr) {
                    return true;
                }
            }
        }
    }
    if let Some(expr) = block.expr {
        return find_panic_in_expr(context, expr);
    }
    false
}
