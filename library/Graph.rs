#[allow(unused_imports)]
use std::collections::BinaryHeap;

type Node = usize;

struct Edge<T: Eq+Ord> {
	from: Node,
	to: Node,
	cost: T
}

impl<T: Eq+Ord> Edge<T> {
	fn new(from: Node, to: Node, cost: T) -> Self {
		Self { from, to, cost }
	}
}

struct Graph<T: Eq+Ord> {
	edges: Vec<Vec<Edge<T>>>,
	size: usize
}

impl<T: Eq+Ord> Graph<T> {
	fn new(n: usize) -> Self {
		Self {
			edges: (0..n).map(|_| Vec::new()).collect(),
			size: n
		}
	}

	fn connect(&mut self, from: Node, to: Node, cost: T) {
		self.edges[from][to] = Edge::new(from, to, cost);
	}
}

struct Dijkstra<T: Eq+Ord> {
	graph: &Graph,
	distances: Vec<Option<T>>,
	pqueue: BinaryHeap<(T, Node)>
}

impl<T: Eq+Ord> Iterator for Dijkstra<T> {
	fn next(&mut self) -> Option<Node> {
		if let Some((cost, node)) = self.pqueue.pop() {
			let ret = node;
			self.distances[node] = Some(cost);
		} else {
			None
		}
	}
}