use rustc_hir::{
    def::{DefKind, Res},
    def_id::DefId,
    Block, BodyId, Expr, ExprKind, ImplItem, ImplItemKind, Item, ItemKind, LetStmt, MatchSource,
    Node, Pat, PatExpr, PatExprKind, PatKind, QPath, StmtKind, StructTailExpr, TraitFn, TraitItem,
    TraitItemKind, TyKind,
};
use rustc_middle::ty::TyCtxt;

use crate::graphs::{CallEdge, CallGraph, CallNodeKind, PanicInfo};

use super::panics;

/// Create a call graph starting from the provided root node.
pub fn update_call_graph_with_node(context: TyCtxt, graph: &mut CallGraph, node: Node) {
    match node {
        Node::Item(item) => update_call_graph_with_item(context, graph, item),
        Node::ImplItem(impl_item) => update_call_graph_with_impl_item(context, graph, impl_item),
        Node::TraitItem(trait_item) => {
            update_call_graph_with_trait_item(context, graph, trait_item);
        }
        Node::ForeignItem(..)
        | Node::Variant(..)
        | Node::Field(..)
        | Node::AnonConst(..)
        | Node::ConstBlock(..)
        | Node::ConstArg(..)
        | Node::Expr(..)
        | Node::ExprField(..)
        | Node::Stmt(..)
        | Node::PathSegment(..)
        | Node::Ty(..)
        | Node::AssocItemConstraint(..)
        | Node::TraitRef(..)
        | Node::OpaqueTy(..)
        | Node::Pat(..)
        | Node::PatField(..)
        | Node::PatExpr(..)
        | Node::Arm(..)
        | Node::Block(..)
        | Node::LetStmt(..)
        | Node::Ctor(..)
        | Node::Lifetime(..)
        | Node::GenericParam(..)
        | Node::Crate(..)
        | Node::Infer(..)
        | Node::WherePredicate(..)
        | Node::PreciseCapturingNonLifetimeArg(..)
        | Node::Synthetic
        | Node::Err(..)
        | Node::Param(..) => unreachable!(),
    }
}

fn update_call_graph_with_trait_item(
    context: TyCtxt<'_>,
    graph: &mut CallGraph,
    trait_item: &TraitItem<'_>,
) {
    match trait_item.kind {
        TraitItemKind::Fn(_, TraitFn::Provided(body_id)) => {
            let panics = panics::get_panic_info_local(context, body_id);
            let span = Some(span_from_body_id(context, body_id));
            let node_kind =
                CallNodeKind::local_fn(trait_item.hir_id().owner.to_def_id(), trait_item.hir_id());
            let node_id = graph.add_node(
                context.def_path_str(node_kind.def_id()),
                node_kind,
                panics,
                span,
            );

            // Add edges/nodes for all functions called from within this function (and recursively do it for those functions as well)
            add_calls_from_body(context, node_id, body_id, graph);
        }
        TraitItemKind::Fn(..) | TraitItemKind::Const(..) | TraitItemKind::Type(..) => {}
    }
}

fn span_from_body_id(context: TyCtxt<'_>, body_id: BodyId) -> String {
    format!("{:?}", context.hir().body(body_id).value.span)
        .rsplit_once(' ')
        .expect("spans always have spaces")
        .0
        .to_owned()
}

fn update_call_graph_with_impl_item(
    context: TyCtxt<'_>,
    graph: &mut CallGraph,
    impl_item: &ImplItem,
) {
    match impl_item.kind {
        ImplItemKind::Fn(_, body_id) => {
            let node_kind =
                CallNodeKind::local_fn(impl_item.hir_id().owner.to_def_id(), impl_item.hir_id());
            let panics = panics::get_panic_info_local(context, body_id);
            let span = Some(span_from_body_id(context, body_id));
            let node_id = graph.add_node(
                context.def_path_str(node_kind.def_id()),
                node_kind,
                panics,
                span,
            );

            // Add edges/nodes for all functions called from within this function (and recursively do it for those functions as well)
            add_calls_from_body(context, node_id, body_id, graph);
        }
        ImplItemKind::Const(..) | ImplItemKind::Type(..) => {}
    }
}

fn update_call_graph_with_item(context: TyCtxt<'_>, graph: &mut CallGraph, item: &Item<'_>) {
    match item.kind {
        ItemKind::Fn { body, .. } => {
            let node_kind = CallNodeKind::local_fn(item.hir_id().owner.to_def_id(), item.hir_id());
            let panics = panics::get_panic_info_local(context, body);
            let span = Some(span_from_body_id(context, body));
            let node_id = graph.add_node(
                context.def_path_str(node_kind.def_id()),
                node_kind,
                panics,
                span,
            );

            // Add edges/nodes for all functions called from within this function (and recursively do it for those functions as well)
            add_calls_from_body(context, node_id, body, graph);
        }
        ItemKind::ExternCrate(..)
        | ItemKind::Use(..)
        | ItemKind::Static(..)
        | ItemKind::Const(..)
        | ItemKind::Macro(..)
        | ItemKind::Mod(..)
        | ItemKind::ForeignMod { .. }
        | ItemKind::GlobalAsm(..)
        | ItemKind::TyAlias(..)
        | ItemKind::Enum(..)
        | ItemKind::Struct(..)
        | ItemKind::Union(..)
        | ItemKind::Trait(..)
        | ItemKind::TraitAlias(..)
        | ItemKind::Impl(..) => {}
    }
}

/// Retrieve all function calls within a function, and add the nodes and edges to the graph.
fn add_calls_from_body(context: TyCtxt, from_node: usize, body_id: BodyId, graph: &mut CallGraph) {
    add_calls_from_expr(
        context,
        from_node,
        context.hir().body(body_id).value,
        body_id,
        graph,
    );
}

fn add_calls_from_expr(
    context: TyCtxt,
    from_node: usize,
    expr: &Expr,
    body_id: BodyId,
    graph: &mut CallGraph,
) {
    // Get the function calls from within this block
    let calls = get_function_calls_in_expression(context, expr, body_id);

    // Add edges for all function calls
    add_calls_to_graph(context, from_node, graph, calls);
}

fn add_calls_to_graph(
    context: TyCtxt<'_>,
    from: usize,
    graph: &mut CallGraph,
    calls: Vec<FunctionCallInfo>,
) {
    for FunctionCallInfo {
        node_kind,
        add_edge,
        propagates,
    } in calls
    {
        match node_kind {
            CallNodeKind::LocalFn(def_id, hir_id) => {
                if let Some(node) = graph.find_local_fn_node(hir_id) {
                    // We have already encountered this local function, so just add the edge
                    if add_edge {
                        graph.add_edge(CallEdge::new_untyped(from, node.id(), propagates));
                    }
                } else {
                    // We have not yet explored this local function, so add new node and edge,
                    // and explore it.
                    let panics = context
                        .hir()
                        .maybe_body_owned_by(def_id.as_local().expect("nonlocal"))
                        .map(|x| panics::get_panic_info_local(context, x.id()))
                        .unwrap_or_default();
                    let span = context
                        .hir()
                        .maybe_body_owned_by(def_id.as_local().expect("nonlocal"))
                        .map(|body| span_from_body_id(context, body.id()));
                    let id = graph.add_node(context.def_path_str(def_id), node_kind, panics, span);

                    if add_edge {
                        graph.add_edge(CallEdge::new_untyped(from, id, propagates));
                    }

                    if let Some(body) = context.hir().maybe_body_owned_by(def_id.expect_local()) {
                        add_calls_from_body(context, id, body.id(), graph);
                    }
                }
            }
            CallNodeKind::NonLocalFn(def_id) => {
                if let Some(node) = graph.find_non_local_fn_node(def_id) {
                    // We have already encountered this non-local function, so just add the edge
                    if add_edge {
                        graph.add_edge(CallEdge::new_untyped(from, node.id(), propagates));
                    }
                } else {
                    // We have not yet explored this non-local function, so add new node and edge
                    let id = graph.add_node(
                        context.def_path_str(node_kind.def_id()),
                        node_kind,
                        PanicInfo::default(),
                        None,
                    );

                    if add_edge {
                        graph.add_edge(CallEdge::new_untyped(from, id, propagates));
                    }
                }
            }
        }
    }
}

/// Retrieve a vec of all function calls made within the body of a block.
fn get_function_calls_in_block(
    context: TyCtxt,
    block: &Block,
    is_fn: bool,
    body_id: BodyId,
) -> Vec<FunctionCallInfo> {
    let mut res: Vec<FunctionCallInfo> = vec![];

    // If the block has an ending expression add calls from there
    // If this block is that of a function, this is a return statement
    if let Some(expr) = block.expr {
        if let ExprKind::DropTemps(ex) = expr.kind {
            if let ExprKind::Block(b, _lbl) = ex.kind {
                return get_function_calls_in_block(context, b, is_fn, body_id);
            }
        } else if is_fn {
            for info in get_function_calls_in_expression(context, expr, body_id) {
                res.push(FunctionCallInfo {
                    propagates: true,
                    ..info
                });
            }
        } else {
            res.extend(get_function_calls_in_expression(context, expr, body_id));
        }
    }

    // Go over all statements in the block
    for statement in block.stmts {
        // Match the kind of statement
        match statement.kind {
            StmtKind::Expr(exp)
            | StmtKind::Semi(exp)
            | StmtKind::Let(&LetStmt {
                init: Some(exp), ..
            }) => {
                res.extend(get_function_calls_in_expression(context, exp, body_id));
            }
            StmtKind::Let(&LetStmt { init: None, .. }) | StmtKind::Item(..) => {
                // No function calls here
            }
        }
    }

    res
}

struct FunctionCallInfo {
    node_kind: CallNodeKind,
    add_edge: bool,
    propagates: bool,
}

/// Retrieve a vec of all function calls made within an expression.
#[allow(clippy::too_many_lines)]
fn get_function_calls_in_expression(
    context: TyCtxt,
    expr: &Expr,
    body_id: BodyId,
) -> Vec<FunctionCallInfo> {
    let mut res: Vec<FunctionCallInfo> = vec![];

    // Match the kind of expression
    match expr.kind {
        ExprKind::Call(func, args) => {
            if let Some(def_id) = get_call_def_id(context, func, body_id) {
                let node_kind = get_node_kind_from_def_id(context, def_id);
                res.push(FunctionCallInfo {
                    node_kind,
                    add_edge: true,
                    propagates: false,
                });
            } else if let ExprKind::Path(qpath) = func.kind {
                if let Some((node_kind, _add_edge)) = get_node_kind_from_path(context, qpath) {
                    res.push(FunctionCallInfo {
                        node_kind,
                        add_edge: true,
                        propagates: false,
                    });
                }
            }
            for exp in args {
                res.extend(get_function_calls_in_expression(context, exp, body_id));
            }
        }
        ExprKind::MethodCall(_path, func, args, _span) => {
            if let Some(def_id) = get_call_def_id(context, func, body_id) {
                let node_kind = get_node_kind_from_def_id(context, def_id);
                res.push(FunctionCallInfo {
                    node_kind,
                    add_edge: true,
                    propagates: false,
                });
            } else if let Some(def_id) = context
                .typeck(expr.hir_id.owner.def_id)
                .type_dependent_def_id(expr.hir_id)
            {
                if let Some(local_id) = def_id.as_local() {
                    res.push(FunctionCallInfo {
                        node_kind: CallNodeKind::local_fn(
                            def_id,
                            context.local_def_id_to_hir_id(local_id),
                        ),
                        add_edge: true,
                        propagates: false,
                    });
                } else {
                    res.push(FunctionCallInfo {
                        node_kind: CallNodeKind::non_local_fn(def_id),
                        add_edge: true,
                        propagates: false,
                    });
                }
            }
            res.extend(get_function_calls_in_expression(context, func, body_id));
            for exp in args {
                res.extend(get_function_calls_in_expression(context, exp, body_id));
            }
        }
        ExprKind::Match(exp, arms, src) => {
            // Wrong but not part of my project
            if let MatchSource::TryDesugar(_hir) = src {
                for info in get_function_calls_in_expression(context, exp, body_id) {
                    res.push(FunctionCallInfo {
                        propagates: true,
                        ..info
                    });
                }

                return res;
            }
            res.extend(get_function_calls_in_expression(context, exp, body_id));
            for arm in arms {
                res.extend(get_function_calls_in_expression(context, arm.body, body_id));
                if let Some(guard) = arm.guard {
                    res.extend(get_function_calls_in_expression(context, guard, body_id));
                }
                res.extend(get_function_calls_in_pattern(context, arm.pat, body_id));
            }
        }
        ExprKind::Closure(closure) => {
            let node_kind = CallNodeKind::local_fn(
                closure.def_id.to_def_id(),
                context.local_def_id_to_hir_id(closure.def_id),
            );
            res.push(FunctionCallInfo {
                node_kind,
                add_edge: true,
                propagates: false,
            });
        }
        ExprKind::ConstBlock(const_block) => {
            res.extend(get_function_calls_in_body(context, const_block.body));
        }
        ExprKind::Array(args) | ExprKind::Tup(args) => {
            for exp in args {
                res.extend(get_function_calls_in_expression(context, exp, body_id));
            }
        }
        ExprKind::Binary(_op, a, b) => {
            res.extend(get_function_calls_in_expression(context, a, body_id));
            res.extend(get_function_calls_in_expression(context, b, body_id));
        }
        ExprKind::Unary(_op, exp) => {
            res.extend(get_function_calls_in_expression(context, exp, body_id));
        }
        ExprKind::Cast(exp, _ty) | ExprKind::Type(exp, _ty) => {
            res.extend(get_function_calls_in_expression(context, exp, body_id));
        }
        ExprKind::DropTemps(exp) | ExprKind::Become(exp) => {
            res.extend(get_function_calls_in_expression(context, exp, body_id));
        }
        ExprKind::Let(exp) => {
            res.extend(get_function_calls_in_expression(context, exp.init, body_id));
        }
        ExprKind::If(a, b, c) => {
            res.extend(get_function_calls_in_expression(context, a, body_id));
            res.extend(get_function_calls_in_expression(context, b, body_id));
            if let Some(exp) = c {
                res.extend(get_function_calls_in_expression(context, exp, body_id));
            }
        }
        ExprKind::Loop(block, _lbl, _src, _span) => {
            res.extend(get_function_calls_in_block(context, block, false, body_id));
        }
        ExprKind::Block(block, _lbl) => {
            res.extend(get_function_calls_in_block(context, block, false, body_id));
        }
        ExprKind::Assign(a, b, _span) => {
            res.extend(get_function_calls_in_expression(context, a, body_id));
            res.extend(get_function_calls_in_expression(context, b, body_id));
        }
        ExprKind::AssignOp(_op, a, b) => {
            res.extend(get_function_calls_in_expression(context, a, body_id));
            res.extend(get_function_calls_in_expression(context, b, body_id));
        }
        ExprKind::Field(exp, _ident) => {
            res.extend(get_function_calls_in_expression(context, exp, body_id));
        }
        ExprKind::Index(a, b, _span) => {
            res.extend(get_function_calls_in_expression(context, a, body_id));
            res.extend(get_function_calls_in_expression(context, b, body_id));
        }
        ExprKind::Path(path) => {
            if let Some((node_kind, add_edge)) = get_node_kind_from_path(context, path) {
                res.push(FunctionCallInfo {
                    node_kind,
                    add_edge,
                    propagates: false,
                });
            }
        }
        ExprKind::AddrOf(_borrow, _mut, exp) => {
            res.extend(get_function_calls_in_expression(context, exp, body_id));
        }
        ExprKind::Break(_dest, opt) => {
            if let Some(exp) = opt {
                res.extend(get_function_calls_in_expression(context, exp, body_id));
            }
        }
        ExprKind::Ret(opt) => {
            if let Some(exp) = opt {
                for info in get_function_calls_in_expression(context, exp, body_id) {
                    res.push(FunctionCallInfo {
                        propagates: true,
                        ..info
                    });
                }
            }
        }
        ExprKind::Struct(_path, args, base) => {
            for exp in args {
                res.extend(get_function_calls_in_expression(context, exp.expr, body_id));
            }
            if let StructTailExpr::Base(expr) = base {
                res.extend(get_function_calls_in_expression(context, expr, body_id));
            }
        }
        ExprKind::Repeat(expr, _len) => {
            res.extend(get_function_calls_in_expression(context, expr, body_id));
        }
        ExprKind::Yield(expr, _src) => {
            res.extend(get_function_calls_in_expression(context, expr, body_id));
        }
        ExprKind::UnsafeBinderCast(_unsafe_binder_cast_kind, expr, ..) => {
            res.extend(get_function_calls_in_expression(context, expr, body_id));
        }
        ExprKind::Continue(..)
        | ExprKind::Err(..)
        | ExprKind::InlineAsm(..)
        | ExprKind::OffsetOf(..)
        | ExprKind::Lit(..) => {
            // No function calls here
        }
    }

    res
}

fn get_function_calls_in_body(context: TyCtxt<'_>, body_id: BodyId) -> Vec<FunctionCallInfo> {
    get_function_calls_in_expression(context, context.hir().body(body_id).value, body_id)
}

/// Retrieve a vec of all function calls made from within a pattern (although I think it can never contain one).
fn get_function_calls_in_pattern(
    context: TyCtxt,
    pat: &Pat,
    body_id: BodyId,
) -> Vec<FunctionCallInfo> {
    match pat.kind {
        PatKind::Binding(_, _, _, opt_pat) => {
            opt_pat.map(|pat| get_function_calls_in_pattern(context, pat, body_id))
        }
        PatKind::Struct(_, fields, _) => Some(
            fields
                .iter()
                .flat_map(|field| get_function_calls_in_pattern(context, field.pat, body_id))
                .collect(),
        ),
        PatKind::TupleStruct(_, pats, _) | PatKind::Or(pats) | PatKind::Tuple(pats, _) => Some(
            pats.iter()
                .flat_map(|pat| get_function_calls_in_pattern(context, pat, body_id))
                .collect(),
        ),
        PatKind::Box(pat) | PatKind::Deref(pat) | PatKind::Ref(pat, _) => {
            Some(get_function_calls_in_pattern(context, pat, body_id))
        }
        PatKind::Range(a, b, _) => Some(
            a.into_iter()
                .chain(b)
                .flat_map(|pat_exp| {
                    get_function_calls_in_pattern_expression(context, pat_exp, body_id)
                })
                .collect(),
        ),
        PatKind::Slice(pats1, opt_pat, pats2) => Some(
            pats1
                .iter()
                .chain(opt_pat)
                .chain(pats2)
                .flat_map(|pat| get_function_calls_in_pattern(context, pat, body_id))
                .collect(),
        ),
        PatKind::Expr(pat_expr) => Some(get_function_calls_in_pattern_expression(
            context, pat_expr, body_id,
        )),
        PatKind::Guard(pat, expr) => Some(
            get_function_calls_in_pattern(context, pat, body_id)
                .into_iter()
                .chain(get_function_calls_in_expression(context, expr, body_id))
                .collect(),
        ),
        PatKind::Wild | PatKind::Never | PatKind::Path(..) | PatKind::Err(..) => None,
    }
    .unwrap_or_default()
}

fn get_function_calls_in_pattern_expression(
    context: TyCtxt<'_>,
    exp: &PatExpr<'_>,
    body_id: BodyId,
) -> Vec<FunctionCallInfo> {
    if let PatExprKind::ConstBlock(const_block) = exp.kind {
        get_function_calls_in_block(
            context,
            context.hir_node(const_block.hir_id).expect_block(),
            false,
            body_id,
        )
    } else {
        vec![]
    }
}

/// Get the node kind from a given `QPath`.
pub(crate) fn get_node_kind_from_path(
    context: TyCtxt,
    qpath: QPath,
) -> Option<(CallNodeKind, bool)> {
    match qpath {
        QPath::Resolved(_, path) => {
            if let Res::Def(kind, id) = path.res {
                let add_edge: bool = matches!(
                    kind,
                    DefKind::Fn | DefKind::Ctor(_, _) | DefKind::AssocFn | DefKind::Closure
                );
                return Some((get_node_kind_from_def_id(context, id), add_edge));
            }
        }
        QPath::TypeRelative(ty, _) => {
            if let TyKind::Path(path) = ty.kind {
                return get_node_kind_from_path(context, path);
            }
        }
        QPath::LangItem(..) => {}
    }

    None
}

/// Get the `CallNodeKind` from a given `DefId`.
fn get_node_kind_from_def_id(context: TyCtxt, def_id: DefId) -> CallNodeKind {
    if let Some(local_id) = def_id.as_local() {
        let hir_id = context.local_def_id_to_hir_id(local_id);
        CallNodeKind::local_fn(def_id, hir_id)
    } else {
        CallNodeKind::non_local_fn(def_id)
    }
}

/// Get the `DefId` of the called function using the `HirId` of the call.
pub fn get_call_def_id(context: TyCtxt, call: &Expr, body_id: BodyId) -> Option<DefId> {
    if let ExprKind::Path(qpath) = call.kind {
        match context.typeck_body(body_id).qpath_res(&qpath, call.hir_id) {
            Res::Def(_, def_id) => Some(def_id),
            Res::PrimTy(..)
            | Res::SelfTyParam { .. }
            | Res::SelfTyAlias { .. }
            | Res::SelfCtor(..)
            | Res::Local(..)
            | Res::ToolMod
            | Res::NonMacroAttr(..)
            | Res::Err => None,
        }
    } else {
        None
    }
}
