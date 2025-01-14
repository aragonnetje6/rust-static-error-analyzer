use rustc_hir::{Block, BodyId, Expr, ExprKind, LetExpr, StructTailExpr};
use rustc_middle::ty::TyCtxt;

use crate::graph::PanicInfo;

pub fn get_panic_info_local(context: TyCtxt<'_>, body_id: BodyId) -> PanicInfo {
    let explicit_invocation = find_panic_in_expr(context, context.hir().body(body_id).value);
    PanicInfo::new(explicit_invocation, false)
}

fn find_panic_in_expr(context: TyCtxt<'_>, outer_expr: &Expr<'_>) -> bool {
    match outer_expr.kind {
        ExprKind::Call(expr, exprs) => {
            is_panic(context, expr) || exprs.iter().any(|expr| find_panic_in_expr(context, expr))
        }
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
        | ExprKind::Path(..)
        | ExprKind::Break(_, None)
        | ExprKind::Continue(..)
        | ExprKind::Ret(None)
        | ExprKind::InlineAsm(..)
        | ExprKind::OffsetOf(..)
        | ExprKind::UnsafeBinderCast(..)
        | ExprKind::Err(..) => false,
    }
}

fn is_panic(context: TyCtxt<'_>, expr: &Expr<'_>) -> bool {
    todo!()
}

fn find_panic_in_block(context: TyCtxt<'_>, outer_block: &Block<'_>) -> bool {
    todo!()
}
