use crate::graphs::{
    CallEdge, CallGraph, CallNode, EdgePanicInfo, ErrorChainGraph, PanicChainEdge, PanicChainGraph,
    PanicChainNode,
};
use std::collections::{hash_map::Entry, BTreeMap, HashMap};

pub fn to_error_chains(call_graph: &CallGraph) -> ErrorChainGraph {
    let mut new_graph = ErrorChainGraph::new(call_graph.crate_name.clone());

    let mut count: usize = 0;
    let mut max_size: usize = 0;
    let mut total_size: usize = 0;
    let mut max_depth: usize = 0;
    // Loop over all edges (e.g. function calls)
    for edge in &call_graph.edges {
        // Start of a chain
        if edge.is_error && !edge.propagates {
            let mut node_map: HashMap<usize, usize> = HashMap::new();

            let (mut calls, depth) = get_error_chain_from_edge(call_graph, edge, &mut vec![], 1);
            calls.push(edge.clone());

            count += 1;
            let size = calls.len();
            total_size += size;
            if size > max_size {
                max_size = size;
            }
            if depth > max_depth {
                max_depth = depth;
            }

            for call in calls {
                // If we've already added the node to the new graph, refer to that, otherwise, add a new node
                let from = match node_map.entry(call.from) {
                    Entry::Occupied(e) => *e.get(),
                    Entry::Vacant(e) => *e.insert(
                        new_graph.add_node_from_call_node(call_graph.nodes[call.from].clone()),
                    ),
                };
                // Ditto
                let to = match node_map.entry(call.to) {
                    Entry::Occupied(e) => *e.get(),
                    Entry::Vacant(e) => *e.insert(
                        new_graph.add_node_from_call_node(call_graph.nodes[call.to].clone()),
                    ),
                };
                // Add the edge
                new_graph.add_edge(from, to, call.ty);
            }
        }
    }

    let average_size = (total_size as f64) / (count as f64);

    println!();
    println!("# Result-errors:");
    println!("There are {count} error propagation chains in this program.");
    println!("The biggest chain consists of {max_size} function calls.");
    println!("The longest error path consists of {max_depth} chained function calls.");
    println!("The average chain consists of {average_size} function calls.");
    println!();

    new_graph
}

fn get_error_chain_from_edge(
    graph: &CallGraph,
    from: &CallEdge,
    explored: &mut Vec<usize>,
    depth: usize,
) -> (Vec<CallEdge>, usize) {
    let mut res = vec![];
    let mut max_depth = depth;

    explored.push(from.to);

    // Add all outgoing propagating error edges from the 'to' node to the list
    // And do the same once for each node this edge calls to
    for edge in graph.get_outgoing_edges(from.to) {
        if edge.is_error && edge.propagates {
            if !explored.contains(&edge.to) && !res.contains(edge) && edge != from {
                // If we haven't had this edge yet, explore the node
                res.push(edge.clone());

                let (chain, d) = get_error_chain_from_edge(graph, edge, explored, depth + 1);
                if d > max_depth {
                    max_depth = d;
                }
                res.extend(chain);
            } else {
                // Otherwise just add the edge
                res.push(edge.clone());
            }
        }
    }

    (res, max_depth)
}

pub(crate) fn to_panic_chains(call_graph: &CallGraph) -> PanicChainGraph {
    let mut nodes = BTreeMap::new();
    let mut edges = Vec::new();
    let mut panic_chains: Vec<Vec<PanicChainEdge>> = Vec::new();

    for call_node in call_graph
        .nodes
        .iter()
        .filter(|node| node.panics.explicit_invocation || node.panics.doc_section)
    {
        if !nodes.contains_key(&call_node.id()) {
            nodes.insert(
                call_node.id(),
                PanicChainNode::new(
                    nodes.len(),
                    call_node.label.clone(),
                    call_node.panics.clone(),
                ),
            );
            add_callers_to_panic_graph(
                call_graph,
                &mut nodes,
                &mut edges,
                call_node,
                call_node.panics.explicit_invocation,
                call_node.panics.doc_section,
            );
        }
        panic_chains.extend(dbg!(get_chains_from_node(
            &edges,
            nodes.get(&call_node.id()).expect("just added").id(),
        )));
    }

    let panic_invocations = nodes
        .values()
        .filter(|node| node.panics.explicit_invocation)
        .count();
    let undocumented_invocations = nodes
        .values()
        .filter(|node| node.panics.explicit_invocation && !node.panics.doc_section)
        .count();
    let documented_panics = nodes
        .values()
        .filter(|node| node.panics.doc_section)
        .count();
    let uninvoked_documented = nodes
        .values()
        .filter(|node| !node.panics.explicit_invocation && node.panics.doc_section)
        .count();

    println!();
    println!("# Panics:");
    println!("There are {panic_invocations} invocations of the panic macro, of which {undocumented_invocations} are undocumented.");
    println!("There are {documented_panics} documented panics, of which {uninvoked_documented} do not directly invoke the panic macro");
    println!(
        "There are {} panic propagation chains in this program.",
        panic_chains.len()
    );
    println!(
        "The longest chain consists of {} function calls.",
        panic_chains.iter().map(Vec::len).max().unwrap_or_default()
    );
    // println!(
    //     "The longest panic path consists of {} chained function calls.",
    //     panic_chains.iter().map(Vec::len).max().unwrap_or_default()
    // );
    // println!("The average chain consists of {} function calls.", panic_chains.iter().);
    println!();
    PanicChainGraph::new(
        nodes.into_values().collect(),
        edges,
        call_graph.crate_name.clone(),
    )
}

fn get_chains_from_node(edges: &[PanicChainEdge], id: usize) -> Vec<Vec<PanicChainEdge>> {
    edges
        .iter()
        .filter(|edge| edge.to == id)
        .flat_map(|edge| {
            if edge.edge_panic_info.catches_panic {
                vec![vec![edge.clone()]]
            } else {
                expand_chains(edges, &[edge.clone()])
            }
        })
        .collect()
}

fn expand_chains(edges: &[PanicChainEdge], chain: &[PanicChainEdge]) -> Vec<Vec<PanicChainEdge>> {
    let to = chain.last().expect("empty chain").to;
    edges
        .iter()
        .filter(|edge| edge.from == to)
        .flat_map(|edge| {
            let mut new_chain = chain.to_owned();
            new_chain.push(edge.clone());
            if chain.contains(edge) {
                vec![]
            } else {
                expand_chains(edges, &new_chain)
            }
        })
        .chain([chain.to_owned()])
        .collect()
}

fn add_callers_to_panic_graph(
    call_graph: &CallGraph,
    nodes: &mut BTreeMap<usize, PanicChainNode>,
    edges: &mut Vec<PanicChainEdge>,
    called_node: &CallNode,
    explicit_invocation: bool,
    doc_section: bool,
) {
    for call_edge in call_graph
        .edges
        .iter()
        .filter(|edge| edge.to == called_node.id())
    {
        let calling_node = call_graph
            .nodes
            .iter()
            .find(|x| x.id() == call_edge.from)
            .expect("invalid graph");
        let catches_panic = calling_node.panics.catches.contains(&called_node.label);

        if !nodes.contains_key(&calling_node.id()) {
            nodes.insert(
                calling_node.id(),
                PanicChainNode::new(
                    nodes.len(),
                    calling_node.label.clone(),
                    calling_node.panics.clone(),
                ),
            );
            add_callers_to_panic_graph(
                call_graph,
                nodes,
                edges,
                calling_node,
                calling_node.panics.explicit_invocation || explicit_invocation,
                calling_node.panics.doc_section || doc_section,
            );
        }
        edges.push(PanicChainEdge::new(
            nodes.get(&calling_node.id()).expect("just added").id(),
            nodes.get(&called_node.id()).expect("just added").id(),
            EdgePanicInfo::new(doc_section, explicit_invocation, catches_panic),
        ));
    }
}
