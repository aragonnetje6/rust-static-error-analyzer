pub mod calls_to_chains;
mod create_graph;
mod types;

use crate::graph::CallGraph;
use rustc_middle::ty::TyCtxt;

/// Analysis steps:
///
/// Step 1: Create call graph
/// Step 1.1: Node for each function
/// Step 1.2: Edge for each function call
/// Step 1.3: Add function call information (e.g. whether it propagates using the try op)
///
/// Step 2: Attach return type info to functions in call graph
/// Step 2.1: Loop over each edge in call graph
/// Step 2.2: Label edge with type info extracted from MIR
///
/// Step 3: Attach panic info to functions in call graph
/// NOTE: skipped due to lack of time
///
/// Step 4: Parse the output graph to show individual propagation chains
pub fn analyze(context: TyCtxt, call_graph: &mut CallGraph) {
    // Get the entry point(s) of the program
    let nodes = get_entry_node(context);

    for node in nodes {
        create_graph::update_call_graph_with_node(context, call_graph, node);
    }

    // Attach return type info
    for edge in &mut call_graph.edges {
        if edge.ty.is_none() {
            let (ty, error) = types::get_error_or_type(
                context,
                edge.hir_id,
                call_graph.nodes[edge.from].kind.def_id(),
                call_graph.nodes[edge.to].kind.def_id(),
            );
            edge.ty = Some(ty);
            edge.is_error = error;
        }
    }
}

/// Retrieve the entry node (aka main function) from the type context.
fn get_entry_node(context: TyCtxt) -> Vec<rustc_hir::Node> {
    context
        .hir()
        .body_owners()
        .map(|local_def_id| {
            context
                .hir()
                .get_if_local(local_def_id.into())
                .expect("guaranteed locals")
        })
        .filter(|node| {
            matches!(
                node,
                rustc_hir::Node::Item(_)
                    | rustc_hir::Node::ForeignItem(_)
                    | rustc_hir::Node::ImplItem(_)
                    | rustc_hir::Node::TraitItem(_)
            )
        })
        .collect::<Vec<_>>()
}
