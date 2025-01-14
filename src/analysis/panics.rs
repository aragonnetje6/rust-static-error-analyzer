use rustc_hir::{Block, BodyId, Expr, ExprKind, LetExpr, LetStmt, QPath, StmtKind, StructTailExpr};
use rustc_middle::ty::TyCtxt;

use crate::graph::PanicInfo;

pub fn get_panic_info_local(context: TyCtxt<'_>, body_id: BodyId) -> PanicInfo {
    let explicit_invocation = find_panic_in_expr(context, context.hir().body(body_id).value);
    PanicInfo::new(explicit_invocation, false)
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
            .is_some_and(|segment| segment.ident.as_str() == "panicking")
        {
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
