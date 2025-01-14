use crate::graph::{CallEdge, CallGraph, ChainGraph};
use std::collections::{hash_map::Entry, HashMap};

pub fn to_chains(graph: &CallGraph) -> ChainGraph {
    let mut new_graph = ChainGraph::new(graph.crate_name.clone());

    let mut count: usize = 0;
    let mut max_size: usize = 0;
    let mut total_size: usize = 0;
    let mut max_depth: usize = 0;
    // Loop over all edges (e.g. function calls)
    for edge in &graph.edges {
        // Start of a chain
        if edge.is_error && !edge.propagates {
            let mut node_map: HashMap<usize, usize> = HashMap::new();

            let (mut calls, depth) = get_chain_from_edge(graph, edge, &mut vec![], 1);
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
                    Entry::Vacant(e) => {
                        *e.insert(new_graph.add_node_from_call_node(graph.nodes[call.from].clone()))
                    }
                };
                // Ditto
                let to = match node_map.entry(call.to) {
                    Entry::Occupied(e) => *e.get(),
                    Entry::Vacant(e) => {
                        *e.insert(new_graph.add_node_from_call_node(graph.nodes[call.to].clone()))
                    }
                };
                // Add the edge
                new_graph.add_edge(from, to, call.ty);
            }
        }
    }

    let panic_invocations = graph
        .nodes
        .iter()
        .filter(|node| node.panics.explicit_invocation)
        .count();

    let average_size = (total_size as f64) / (count as f64);

    println!();
    println!("There are {count} error propagation chains in this program.");
    println!("The biggest chain consists of {max_size} function calls.");
    println!("The longest error path consists of {max_depth} chained function calls.");
    println!("The average chain consists of {average_size} function calls.");
    println!("There are {panic_invocations} invocations of the panic macro.");
    println!();

    new_graph
}

fn get_chain_from_edge(
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

                let (chain, d) = get_chain_from_edge(graph, edge, explored, depth + 1);
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
