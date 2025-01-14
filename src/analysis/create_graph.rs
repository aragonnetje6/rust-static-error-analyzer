use crate::graph::{CallEdge, CallGraph, CallNodeKind, PanicInfo};
use rustc_hir::def::{DefKind, Res};
use rustc_hir::def_id::DefId;
use rustc_hir::{
    Block, BodyId, Expr, ExprKind, HirId, ImplItem, ImplItemKind, Item, ItemKind, LetStmt,
    MatchSource, Node, Pat, PatExpr, PatExprKind, PatKind, QPath, StmtKind, StructTailExpr,
    TraitFn, TraitItem, TraitItemKind, TyKind,
};
use rustc_middle::mir::TerminatorKind;
use rustc_middle::ty::TyCtxt;

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
            let panics = get_panic_info_local(context, body_id);
            let node_kind =
                CallNodeKind::local_fn(trait_item.hir_id().owner.to_def_id(), trait_item.hir_id());
            let node_id =
                graph.add_node(context.def_path_str(node_kind.def_id()), node_kind, panics);

            // Add edges/nodes for all functions called from within this function (and recursively do it for those functions as well)
            add_calls_from_function(context, node_id, body_id.hir_id, graph);
        }
        TraitItemKind::Fn(..) | TraitItemKind::Const(..) | TraitItemKind::Type(..) => {}
    }
}

fn get_panic_info_local(_context: TyCtxt<'_>, _body_id: BodyId) -> PanicInfo {
    // TODO: actually search for panics
    PanicInfo::default()
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
            let panics = get_panic_info_local(context, body_id);
            let node_id =
                graph.add_node(context.def_path_str(node_kind.def_id()), node_kind, panics);

            // Add edges/nodes for all functions called from within this function (and recursively do it for those functions as well)
            add_calls_from_function(context, node_id, body_id.hir_id, graph);
        }
        ImplItemKind::Const(..) | ImplItemKind::Type(..) => {}
    }
}

fn update_call_graph_with_item(context: TyCtxt<'_>, graph: &mut CallGraph, item: &Item<'_>) {
    match item.kind {
        ItemKind::Fn { body, .. } => {
            let node_kind = CallNodeKind::local_fn(item.hir_id().owner.to_def_id(), item.hir_id());
            let panics = get_panic_info_local(context, body);
            let node_id =
                graph.add_node(context.def_path_str(node_kind.def_id()), node_kind, panics);

            // Add edges/nodes for all functions called from within this function (and recursively do it for those functions as well)
            add_calls_from_function(context, node_id, body.hir_id, graph);
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
fn add_calls_from_function(context: TyCtxt, from_node: usize, fn_id: HirId, graph: &mut CallGraph) {
    let node = context.hir_node(fn_id);

    // Access the code block of the function
    match node {
        rustc_hir::Node::Expr(expr) => {
            if let ExprKind::Block(block, _) = expr.kind {
                add_calls_from_block(context, from_node, block, graph);
            } else if let ExprKind::Closure(closure) = expr.kind {
                add_calls_from_function(context, from_node, closure.body.hir_id, graph);
            }
        }
        rustc_hir::Node::Block(block) => {
            add_calls_from_block(context, from_node, block, graph);
        }
        rustc_hir::Node::Item(item) => {
            if let ItemKind::Fn { body, .. } = item.kind {
                add_calls_from_function(context, from_node, body.hir_id, graph);
            }
        }
        rustc_hir::Node::ImplItem(item) => {
            if let ImplItemKind::Fn(_sig, id) = item.kind {
                add_calls_from_function(context, from_node, id.hir_id, graph);
            }
        }
        rustc_hir::Node::TraitItem(item) => {
            if let TraitItemKind::Fn(_sig, rustc_hir::TraitFn::Provided(id)) = item.kind {
                add_calls_from_function(context, from_node, id.hir_id, graph);
            }
        }
        Node::Param(..)
        | Node::ForeignItem(..)
        | Node::Variant(..)
        | Node::Field(..)
        | Node::AnonConst(..)
        | Node::ConstBlock(..)
        | Node::ConstArg(..)
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
        | Node::LetStmt(..)
        | Node::Ctor(..)
        | Node::Lifetime(..)
        | Node::GenericParam(..)
        | Node::Crate(..)
        | Node::Infer(..)
        | Node::WherePredicate(..)
        | Node::PreciseCapturingNonLifetimeArg(..)
        | Node::Synthetic
        | Node::Err(..) => {}
    }
}

/// Retrieve all function calls within a block, and add the nodes and edges to the graph.
fn add_calls_from_block(context: TyCtxt, from: usize, block: &Block, graph: &mut CallGraph) {
    // Get the function calls from within this block
    let calls = get_function_calls_in_block(context, block, true);

    // Add edges for all function calls
    for FunctionCallInfo {
        node_kind,
        hir_id,
        add_edge,
        propagates,
    } in calls
    {
        match node_kind {
            CallNodeKind::LocalFn(def_id, hir_id) => {
                if let Some(node) = graph.find_local_fn_node(hir_id) {
                    // We have already encountered this local function, so just add the edge
                    if add_edge {
                        graph.add_edge(CallEdge::new_untyped(from, node.id(), hir_id, propagates));
                    }
                } else {
                    // We have not yet explored this local function, so add new node and edge,
                    // and explore it.
                    let panics = get_panic_info_local(context, BodyId { hir_id });
                    let id = graph.add_node(context.def_path_str(def_id), node_kind, panics);

                    if add_edge {
                        graph.add_edge(CallEdge::new_untyped(from, id, hir_id, propagates));
                    }

                    add_calls_from_function(context, id, hir_id, graph);
                }
            }
            CallNodeKind::NonLocalFn(def_id) => {
                if let Some(node) = graph.find_non_local_fn_node(def_id) {
                    // We have already encountered this non-local function, so just add the edge
                    if add_edge {
                        graph.add_edge(CallEdge::new_untyped(from, node.id(), hir_id, propagates));
                    }
                } else {
                    // We have not yet explored this non-local function, so add new node and edge
                    let id = graph.add_node(
                        context.def_path_str(node_kind.def_id()),
                        node_kind,
                        PanicInfo::default(),
                    );

                    if add_edge {
                        graph.add_edge(CallEdge::new_untyped(from, id, hir_id, propagates));
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
) -> Vec<FunctionCallInfo> {
    let mut res: Vec<FunctionCallInfo> = vec![];

    // If the block has an ending expression add calls from there
    // If this block is that of a function, this is a return statement
    if let Some(exp) = block.expr {
        if let ExprKind::DropTemps(ex) = exp.kind {
            if let ExprKind::Block(b, _lbl) = ex.kind {
                return get_function_calls_in_block(context, b, is_fn);
            }
        } else if is_fn {
            for FunctionCallInfo {
                node_kind,
                hir_id,
                add_edge,
                propagates: _,
            } in get_function_calls_in_expression(context, exp)
            {
                res.push(FunctionCallInfo {
                    node_kind,
                    hir_id,
                    add_edge,
                    propagates: true,
                });
            }
        } else {
            res.extend(get_function_calls_in_expression(context, exp));
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
                res.extend(get_function_calls_in_expression(context, exp));
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
    hir_id: HirId,
    add_edge: bool,
    propagates: bool,
}

/// Retrieve a vec of all function calls made within an expression.
#[allow(clippy::too_many_lines)]
fn get_function_calls_in_expression(context: TyCtxt, expr: &Expr) -> Vec<FunctionCallInfo> {
    let mut res: Vec<FunctionCallInfo> = vec![];

    // Match the kind of expression
    match expr.kind {
        ExprKind::Call(func, args) => {
            if let Some(def_id) = get_call_def_id(context, expr.hir_id) {
                let node_kind = get_node_kind_from_def_id(context, def_id);
                res.push(FunctionCallInfo {
                    node_kind,
                    hir_id: expr.hir_id,
                    add_edge: true,
                    propagates: false,
                });
            } else if let ExprKind::Path(qpath) = func.kind {
                if let Some((node_kind, _add_edge)) = get_node_kind_from_path(context, qpath) {
                    res.push(FunctionCallInfo {
                        node_kind,
                        hir_id: expr.hir_id,
                        add_edge: true,
                        propagates: false,
                    });
                }
            }
            for exp in args {
                res.extend(get_function_calls_in_expression(context, exp));
            }
        }
        ExprKind::MethodCall(_path, exp, args, _span) => {
            if let Some(def_id) = get_call_def_id(context, expr.hir_id) {
                let node_kind = get_node_kind_from_def_id(context, def_id);
                res.push(FunctionCallInfo {
                    node_kind,
                    hir_id: expr.hir_id,
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
                        hir_id: expr.hir_id,
                        add_edge: true,
                        propagates: false,
                    });
                } else {
                    res.push(FunctionCallInfo {
                        node_kind: CallNodeKind::non_local_fn(def_id),
                        hir_id: expr.hir_id,
                        add_edge: true,
                        propagates: false,
                    });
                }
            }
            res.extend(get_function_calls_in_expression(context, exp));
            for exp in args {
                res.extend(get_function_calls_in_expression(context, exp));
            }
        }
        ExprKind::Match(exp, arms, src) => {
            if let MatchSource::TryDesugar(_hir) = src {
                for FunctionCallInfo {
                    node_kind: kind,
                    hir_id: id,
                    add_edge,
                    propagates: _,
                } in get_function_calls_in_expression(context, exp)
                {
                    res.push(FunctionCallInfo {
                        node_kind: kind,
                        hir_id: id,
                        add_edge,
                        propagates: true,
                    });
                }

                return res;
            }
            res.extend(get_function_calls_in_expression(context, exp));
            for arm in arms {
                res.extend(get_function_calls_in_expression(context, arm.body));
                if let Some(guard) = arm.guard {
                    res.extend(get_function_calls_in_expression(context, guard));
                }
                res.extend(get_function_calls_in_pattern(context, arm.pat));
            }
        }
        ExprKind::Closure(closure) => {
            let node_kind = CallNodeKind::local_fn(
                closure.def_id.to_def_id(),
                context.local_def_id_to_hir_id(closure.def_id),
            );
            res.push(FunctionCallInfo {
                node_kind,
                hir_id: expr.hir_id,
                add_edge: false,
                propagates: false,
            });
        }
        ExprKind::ConstBlock(block) => {
            let node = context.hir_node(block.hir_id);
            res.extend(get_function_calls_in_block(
                context,
                node.expect_block(),
                false,
            ));
        }
        ExprKind::Array(args) | ExprKind::Tup(args) => {
            for exp in args {
                res.extend(get_function_calls_in_expression(context, exp));
            }
        }
        ExprKind::Binary(_op, a, b) => {
            res.extend(get_function_calls_in_expression(context, a));
            res.extend(get_function_calls_in_expression(context, b));
        }
        ExprKind::Unary(_op, exp) => {
            res.extend(get_function_calls_in_expression(context, exp));
        }
        ExprKind::Cast(exp, _ty) | ExprKind::Type(exp, _ty) => {
            res.extend(get_function_calls_in_expression(context, exp));
        }
        ExprKind::DropTemps(exp) | ExprKind::Become(exp) => {
            res.extend(get_function_calls_in_expression(context, exp));
        }
        ExprKind::Let(exp) => {
            res.extend(get_function_calls_in_expression(context, exp.init));
        }
        ExprKind::If(a, b, c) => {
            res.extend(get_function_calls_in_expression(context, a));
            res.extend(get_function_calls_in_expression(context, b));
            if let Some(exp) = c {
                res.extend(get_function_calls_in_expression(context, exp));
            }
        }
        ExprKind::Loop(block, _lbl, _src, _span) => {
            res.extend(get_function_calls_in_block(context, block, false));
        }
        ExprKind::Block(block, _lbl) => {
            res.extend(get_function_calls_in_block(context, block, false));
        }
        ExprKind::Assign(a, b, _span) => {
            res.extend(get_function_calls_in_expression(context, a));
            res.extend(get_function_calls_in_expression(context, b));
        }
        ExprKind::AssignOp(_op, a, b) => {
            res.extend(get_function_calls_in_expression(context, a));
            res.extend(get_function_calls_in_expression(context, b));
        }
        ExprKind::Field(exp, _ident) => {
            res.extend(get_function_calls_in_expression(context, exp));
        }
        ExprKind::Index(a, b, _span) => {
            res.extend(get_function_calls_in_expression(context, a));
            res.extend(get_function_calls_in_expression(context, b));
        }
        ExprKind::Path(path) => {
            if let Some((node_kind, add_edge)) = get_node_kind_from_path(context, path) {
                res.push(FunctionCallInfo {
                    node_kind,
                    hir_id: expr.hir_id,
                    add_edge,
                    propagates: false,
                });
            }
        }
        ExprKind::AddrOf(_borrow, _mut, exp) => {
            res.extend(get_function_calls_in_expression(context, exp));
        }
        ExprKind::Break(_dest, opt) => {
            if let Some(exp) = opt {
                res.extend(get_function_calls_in_expression(context, exp));
            }
        }
        ExprKind::Ret(opt) => {
            if let Some(exp) = opt {
                for FunctionCallInfo {
                    node_kind: kind,
                    hir_id: id,
                    add_edge,
                    propagates: _,
                } in get_function_calls_in_expression(context, exp)
                {
                    res.push(FunctionCallInfo {
                        node_kind: kind,
                        hir_id: id,
                        add_edge,
                        propagates: true,
                    });
                }
            }
        }
        ExprKind::Struct(_path, args, base) => {
            for exp in args {
                res.extend(get_function_calls_in_expression(context, exp.expr));
            }
            if let StructTailExpr::Base(exp) = base {
                res.extend(get_function_calls_in_expression(context, exp));
            }
        }
        ExprKind::Repeat(exp, _len) => {
            res.extend(get_function_calls_in_expression(context, exp));
        }
        ExprKind::Yield(exp, _src) => {
            res.extend(get_function_calls_in_expression(context, exp));
        }
        ExprKind::UnsafeBinderCast(_unsafe_binder_cast_kind, expr, ..) => {
            res.extend(get_function_calls_in_expression(context, expr));
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

/// Retrieve a vec of all function calls made from within a pattern (although I think it can never contain one).
fn get_function_calls_in_pattern(context: TyCtxt, pat: &Pat) -> Vec<FunctionCallInfo> {
    match pat.kind {
        PatKind::Binding(_, _, _, opt_pat) => {
            opt_pat.map(|pat| get_function_calls_in_pattern(context, pat))
        }
        PatKind::Struct(_, fields, _) => Some(
            fields
                .iter()
                .flat_map(|field| get_function_calls_in_pattern(context, field.pat))
                .collect(),
        ),
        PatKind::TupleStruct(_, pats, _) | PatKind::Or(pats) | PatKind::Tuple(pats, _) => Some(
            pats.iter()
                .flat_map(|pat| get_function_calls_in_pattern(context, pat))
                .collect(),
        ),
        PatKind::Box(pat) | PatKind::Deref(pat) | PatKind::Ref(pat, _) => {
            Some(get_function_calls_in_pattern(context, pat))
        }
        PatKind::Range(a, b, _) => Some(
            a.into_iter()
                .chain(b)
                .flat_map(|pat_exp| get_function_calls_in_pattern_expression(context, pat_exp))
                .collect(),
        ),
        PatKind::Slice(pats1, opt_pat, pats2) => Some(
            pats1
                .iter()
                .chain(opt_pat)
                .chain(pats2)
                .flat_map(|pat| get_function_calls_in_pattern(context, pat))
                .collect(),
        ),
        PatKind::Expr(pat_expr) => {
            Some(get_function_calls_in_pattern_expression(context, pat_expr))
        }
        PatKind::Guard(pat, expr) => Some(
            get_function_calls_in_pattern(context, pat)
                .into_iter()
                .chain(get_function_calls_in_expression(context, expr))
                .collect(),
        ),
        PatKind::Wild | PatKind::Never | PatKind::Path(..) | PatKind::Err(..) => None,
    }
    .unwrap_or_default()
}

fn get_function_calls_in_pattern_expression(
    context: TyCtxt<'_>,
    exp: &PatExpr<'_>,
) -> Vec<FunctionCallInfo> {
    if let PatExprKind::ConstBlock(const_block) = exp.kind {
        get_function_calls_in_block(
            context,
            context.hir_node(const_block.hir_id).expect_block(),
            false,
        )
    } else {
        vec![]
    }
}

/// Get the node kind from a given `QPath`.
fn get_node_kind_from_path(context: TyCtxt, qpath: QPath) -> Option<(CallNodeKind, bool)> {
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
pub fn get_call_def_id(context: TyCtxt, call_id: HirId) -> Option<DefId> {
    if !context.is_mir_available(call_id.owner.to_def_id()) {
        return None;
    }

    let mir = context.optimized_mir(call_id.owner.to_def_id());

    for block in mir.basic_blocks.iter() {
        if let Some(terminator) = &block.terminator {
            if let TerminatorKind::Call { func, fn_span, .. } = &terminator.kind {
                if context.hir_node(call_id).expect_expr().span.hi() == fn_span.hi() {
                    if let Some((def_id, _)) = func.const_fn_def() {
                        return Some(def_id);
                    }
                }
            }
        }
    }

    None
}
