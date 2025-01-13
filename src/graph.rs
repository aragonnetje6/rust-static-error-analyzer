use dot::{Edges, Id, Kind, LabelText, Nodes, Style};
use rustc_hir::def_id::DefId;
use rustc_hir::HirId;
use std::borrow::Cow;
use std::cmp::PartialEq;

#[derive(Debug, Clone)]
pub struct CallGraph {
    pub nodes: Vec<CallNode>,
    pub edges: Vec<CallEdge>,
    pub crate_name: String,
}

#[derive(Debug, Clone)]
pub struct CallNode {
    id: usize,
    pub label: String,
    pub kind: CallNodeKind,
    pub panics: bool,
}

#[derive(Debug, Clone)]
pub enum CallNodeKind {
    LocalFn(DefId, HirId),
    NonLocalFn(DefId),
}

#[derive(Debug, Clone)]
pub struct CallEdge {
    pub from: usize,
    pub to: usize,
    pub hir_id: HirId,
    pub ty: Option<String>,
    pub propagates: bool,
    pub is_error: bool,
}

impl<'a> dot::Labeller<'a, CallNode, CallEdge> for CallGraph {
    fn graph_id(&self) -> Id<'a> {
        let mut name: String = self.crate_name.clone();
        name.retain(|e| e.is_ascii_alphanumeric() || e == '_');
        Id::new(format!("error_propagation_{name}")).expect("invalid graph ID")
    }

    fn node_id(&self, n: &CallNode) -> Id<'a> {
        Id::new(format!("n{:?}", n.id)).expect("invalid node ID")
    }

    fn node_label(&self, n: &CallNode) -> LabelText<'a> {
        LabelText::label(n.label.clone())
    }

    fn edge_label(&self, e: &CallEdge) -> LabelText<'a> {
        LabelText::label(e.ty.clone().unwrap_or(String::from("unknown")))
    }

    fn node_color(&'a self, n: &CallNode) -> Option<LabelText<'a>> {
        if n.panics {
            Some(LabelText::label("red"))
        } else {
            None
        }
    }

    fn edge_color(&'a self, e: &CallEdge) -> Option<LabelText<'a>> {
        if e.is_error && e.propagates {
            Some(LabelText::label("purple"))
        } else if e.is_error {
            Some(LabelText::label("red"))
        } else if e.propagates {
            Some(LabelText::label("blue"))
        } else {
            None
        }
    }

    fn edge_style(&'a self, e: &CallEdge) -> Style {
        if e.is_error || e.propagates {
            Style::None
        } else {
            Style::Dotted
        }
    }

    fn kind(&self) -> Kind {
        Kind::Digraph
    }
}

impl<'a> dot::GraphWalk<'a, CallNode, CallEdge> for CallGraph {
    fn nodes(&'a self) -> Nodes<'a, CallNode> {
        let mut nodes = vec![];
        for edge in &self.edges {
            if !nodes.contains(&self.nodes[edge.from]) {
                nodes.push(self.nodes[edge.from].clone());
            }
            if !nodes.contains(&self.nodes[edge.to]) {
                nodes.push(self.nodes[edge.to].clone());
            }
        }
        Cow::Owned(nodes)
    }

    fn edges(&'a self) -> Edges<'a, CallEdge> {
        Cow::Owned(self.edges.clone())
    }

    fn source(&'a self, edge: &CallEdge) -> CallNode {
        self.nodes[edge.from].clone()
    }

    fn target(&'a self, edge: &CallEdge) -> CallNode {
        self.nodes[edge.to].clone()
    }
}

#[derive(Debug, Clone)]
pub struct ChainGraph {
    pub nodes: Vec<ChainNode>,
    pub edges: Vec<ChainEdge>,
    pub crate_name: String,
}

#[derive(Debug, Clone)]
pub struct ChainNode {
    id: usize,
    label: String,
}

#[derive(Debug, Clone)]
pub struct ChainEdge {
    from: usize,
    to: usize,
    label: Option<String>,
}

impl<'a> dot::Labeller<'a, ChainNode, ChainEdge> for ChainGraph {
    fn graph_id(&'a self) -> Id<'a> {
        let mut name: String = self.crate_name.clone();
        name.retain(|e| e.is_ascii_alphanumeric() || e == '_');
        Id::new(format!("error_propagation_{name}_chains")).expect("invalid graph ID")
    }

    fn node_id(&'a self, n: &ChainNode) -> Id<'a> {
        Id::new(format!("n{:?}", n.id)).expect("invalid node ID")
    }

    fn node_label(&self, n: &ChainNode) -> LabelText<'a> {
        LabelText::label(n.label.clone())
    }

    fn edge_label(&self, e: &ChainEdge) -> LabelText<'a> {
        LabelText::label(e.label.clone().unwrap_or(String::from("unknown")))
    }
}

impl<'a> dot::GraphWalk<'a, ChainNode, ChainEdge> for ChainGraph {
    fn nodes(&'a self) -> Nodes<'a, ChainNode> {
        let mut nodes = vec![];
        for edge in &self.edges {
            if !nodes.contains(&self.nodes[edge.from]) {
                nodes.push(self.nodes[edge.from].clone());
            }
            if !nodes.contains(&self.nodes[edge.to]) {
                nodes.push(self.nodes[edge.to].clone());
            }
        }
        Cow::Owned(nodes)
    }

    fn edges(&'a self) -> Edges<'a, ChainEdge> {
        Cow::Owned(self.edges.clone())
    }

    fn source(&'a self, edge: &ChainEdge) -> ChainNode {
        self.nodes[edge.to].clone()
    }

    fn target(&'a self, edge: &ChainEdge) -> ChainNode {
        self.nodes[edge.from].clone()
    }
}

impl CallGraph {
    /// Create a new, empty graph.
    pub fn new(crate_name: String) -> Self {
        CallGraph {
            nodes: Vec::new(),
            edges: Vec::new(),
            crate_name,
        }
    }

    /// Add a node to this graph, returning its id.
    pub fn add_node(&mut self, label: String, node_kind: CallNodeKind, panics: bool) -> usize {
        let node = CallNode::new(self.nodes.len(), label, node_kind, panics);
        let id = node.id();
        self.nodes.push(node);
        id
    }

    /// Add an edge between two nodes to this graph.
    pub fn add_edge(&mut self, edge: CallEdge) {
        self.edges.push(edge);
    }

    /// Find a node of `LocalFn` kind.
    pub fn find_local_fn_node(&self, id: HirId) -> Option<&CallNode> {
        for node in &self.nodes {
            if let CallNodeKind::LocalFn(_def_id, hir_id) = node.kind {
                if hir_id == id {
                    return Some(node);
                }
            }
        }

        None
    }

    /// Find a node of `NonLocalFn` kind.
    pub fn find_non_local_fn_node(&self, id: DefId) -> Option<&CallNode> {
        for node in &self.nodes {
            if let CallNodeKind::NonLocalFn(def_id) = node.kind {
                if def_id == id {
                    return Some(node);
                }
            }
        }

        None
    }

    pub fn get_outgoing_edges(&self, node_id: usize) -> Vec<&CallEdge> {
        let mut res = vec![];

        for edge in &self.edges {
            if edge.from == node_id {
                res.push(edge);
            }
        }

        res
    }

    /// Convert this graph to dot representation.
    pub fn to_dot(&self) -> String {
        let mut buf = Vec::new();
        dot::render(self, &mut buf).expect("graph rendering failed");
        String::from_utf8(buf).expect("invalid graph representation")
    }
}

impl CallNode {
    /// Create a new node.
    fn new(id: usize, label: String, kind: CallNodeKind, panics: bool) -> Self {
        CallNode {
            id,
            label,
            kind,
            panics,
        }
    }

    /// Get the id of this node.
    pub fn id(&self) -> usize {
        self.id
    }
}

impl CallNodeKind {
    /// Get a new `LocalFn`.
    pub fn local_fn(def_id: DefId, hir_id: HirId) -> Self {
        CallNodeKind::LocalFn(def_id, hir_id)
    }

    /// Get a new `NonLocalFn`.
    pub fn non_local_fn(id: DefId) -> Self {
        CallNodeKind::NonLocalFn(id)
    }

    /// Extract the `DefId` from this node.
    pub fn def_id(&self) -> DefId {
        match self {
            CallNodeKind::LocalFn(def_id, _hir_id) => *def_id,
            CallNodeKind::NonLocalFn(def_id) => *def_id,
        }
    }
}

impl CallEdge {
    pub fn new(
        from: usize,
        to: usize,
        hir_id: HirId,
        ty: Option<String>,
        propagates: bool,
        is_error: bool,
    ) -> Self {
        Self {
            from,
            to,
            hir_id,
            ty,
            propagates,
            is_error,
        }
    }

    /// Create a new edge.
    pub fn new_untyped(from: usize, to: usize, hir_id: HirId, propagates: bool) -> Self {
        CallEdge::new(from, to, hir_id, None, propagates, false)
    }
}

impl PartialEq for CallNode {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.kind == other.kind
    }
}

impl PartialEq for CallNodeKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (CallNodeKind::LocalFn(def_id1, hir_id1), CallNodeKind::LocalFn(def_id2, hir_id2)) => {
                def_id1 == def_id2 && hir_id1 == hir_id2
            }
            (CallNodeKind::NonLocalFn(id1), CallNodeKind::NonLocalFn(id2)) => id1 == id2,
            (CallNodeKind::LocalFn(..), CallNodeKind::NonLocalFn(..))
            | (CallNodeKind::NonLocalFn(..), CallNodeKind::LocalFn(..)) => false,
        }
    }
}

impl PartialEq for CallEdge {
    fn eq(&self, other: &Self) -> bool {
        self.to == other.to && self.from == other.from
    }
}

impl ChainGraph {
    /// Create a new, empty graph.
    pub fn new(crate_name: String) -> Self {
        ChainGraph {
            nodes: Vec::new(),
            edges: Vec::new(),
            crate_name,
        }
    }

    pub fn add_node(&mut self, label: String) -> usize {
        let id = self.nodes.len();

        self.nodes.push(ChainNode::new(id, label));

        id
    }

    pub fn add_edge(&mut self, from: usize, to: usize, label: Option<String>) {
        self.edges.push(ChainEdge::new(from, to, label));
    }

    /// Convert this graph to dot representation.
    pub fn to_dot(&self) -> String {
        let mut buf = Vec::new();
        dot::render(self, &mut buf).expect("graph rendering error");
        String::from_utf8(buf).expect("graph string invalid")
    }
}

impl ChainNode {
    /// Create a new node.
    fn new(id: usize, label: String) -> Self {
        ChainNode { id, label }
    }
}

impl ChainEdge {
    /// Create a new edge.
    pub fn new(from: usize, to: usize, label: Option<String>) -> Self {
        ChainEdge { from, to, label }
    }
}

impl PartialEq for ChainNode {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl PartialEq for ChainEdge {
    fn eq(&self, other: &Self) -> bool {
        self.to == other.to && self.from == other.from
    }
}
