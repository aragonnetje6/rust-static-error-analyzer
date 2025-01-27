use crate::graphs::{
    CallEdge, CallGraph, EdgePanicInfo, ErrorChainGraph, PanicChainEdge, PanicChainGraph,
    PanicChainNode,
};
use std::collections::{hash_map::Entry, BTreeSet, HashMap};

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
    let mut nodes: Vec<_> = call_graph
        .nodes
        .iter()
        .map(|node| PanicChainNode::new(node.id(), node.label.clone(), node.panics.clone()))
        .collect();
    let mut edges: Vec<PanicChainEdge> = call_graph
        .edges
        .iter()
        .map(|edge| PanicChainEdge::new(edge.from, edge.to, EdgePanicInfo::default()))
        .collect();

    for node in &nodes {
        for edge in edges
            .iter_mut()
            .filter(|edge| edge.from == node.id())
            .filter(|edge| {
                node.panics.catches.contains(
                    &nodes
                        .iter()
                        .find(|node| node.id() == edge.to)
                        .expect("edge references nonexistent node")
                        .label,
                )
            })
        {
            edge.edge_panic_info.catches_panic = true;
        }
    }
    for node in &nodes {
        if node.panics.explicit_invocation || node.panics.doc_section {
            propagate_panic_kind(
                &mut edges,
                node.id(),
                node.panics.explicit_invocation,
                node.panics.doc_section,
            );
        }
    }

    // trim
    edges.retain(|edge| {
        edge.edge_panic_info.propagates_doc_panic
            || edge.edge_panic_info.propagates_invoked_panic
            || edge.edge_panic_info.catches_panic
    });
    nodes.retain(|node| {
        edges
            .iter()
            .any(|edge| node.id() == edge.to || node.id() == edge.from)
    });

    // renumber
    let id_map: HashMap<usize, usize> = nodes
        .iter()
        .enumerate()
        .map(|(i, node)| (node.id(), i))
        .collect();
    for node in &mut nodes {
        node.id = id_map[&node.id];
    }
    for edge in &mut edges {
        edge.from = id_map[&edge.from];
        edge.to = id_map[&edge.to];
    }

    let panic_chains: Vec<Vec<PanicChainEdge>> = nodes
        .iter()
        .filter(|node| node.panics.explicit_invocation || node.panics.doc_section)
        .flat_map(|node| get_chains_from_node(&edges, node.id()))
        .collect();

    print_panic_stats(&nodes, &panic_chains);
    PanicChainGraph::new(nodes, edges, call_graph.crate_name.clone())
}

fn print_panic_stats(nodes: &[PanicChainNode], panic_chains: &[Vec<PanicChainEdge>]) {
    let panic_invocations = nodes
        .iter()
        .filter(|node| node.panics.explicit_invocation)
        .count();
    let undocumented_invocations = nodes
        .iter()
        .filter(|node| node.panics.explicit_invocation && !node.panics.doc_section)
        .count();
    let documented_panics = nodes.iter().filter(|node| node.panics.doc_section).count();
    let uninvoked_documented = nodes
        .iter()
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
    println!(
        "The average chain consists of {} function calls.",
        if (panic_chains.iter().map(|x| x.len() as f64).sum::<f64>() / panic_chains.len() as f64)
            .is_nan()
        {
            0f64
        } else {
            panic_chains.iter().map(|x| x.len() as f64).sum::<f64>() / panic_chains.len() as f64
        }
    );
    println!();
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
    let from = chain.last().expect("empty chain").from;
    edges
        .iter()
        .filter(|edge| edge.to == from)
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

fn propagate_panic_kind(
    edges: &mut [PanicChainEdge],
    id: usize,
    explicit_invocation: bool,
    doc_section: bool,
) {
    let mut visited: BTreeSet<usize> = BTreeSet::new();
    let mut stack: Vec<usize> = edges
        .iter()
        .enumerate()
        .filter(|(_, edge)| edge.to == id)
        .map(|(i, _)| i)
        .collect();
    visited.extend(&stack);
    while let Some(current) = stack.pop() {
        edges[current].edge_panic_info.propagates_doc_panic |= doc_section;
        edges[current].edge_panic_info.propagates_invoked_panic |= explicit_invocation;
        if !edges[current].edge_panic_info.catches_panic {
            let next: Vec<usize> = edges
                .iter()
                .enumerate()
                .filter(|(_, edge)| edge.to == edges[current].from)
                .map(|(i, _)| i)
                .filter(|i| !visited.contains(i))
                .collect();
            stack.extend(&next);
            visited.extend(&next);
        }
    }
}
