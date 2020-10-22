#[allow(unused_imports)]
use std::io::{stdin, Read};
#[allow(unused_imports)]
use std::fmt::Debug;
#[allow(unused_imports)]
use std::cmp::max;
#[allow(unused_imports)]
use std::cmp::min;

#[allow(unused_macros)]
macro_rules! read {
	($i:ident) => {
		$i.next().unwrap().parse().unwrap();
	};
	($i:ident, $t:ty) => {
		$i.next().unwrap().parse::<$t>().unwrap();
	};
}

#[allow(unused_macros)]
macro_rules! read_init {
	($i:ident) => {
		read_init!(stdin, $i)
	};
	(stdin, $i:ident) => {
		let mut _buf = String::new();
		stdin().read_to_string(&mut _buf).unwrap();
		let mut $i = _buf.split_whitespace();
	};
}

#[allow(dead_code)]
const INF: usize = 2_000_000_000;
#[allow(dead_code, non_upper_case_globals)]
const INFu64: u64 = 1_000_000_000_000_000_000;
#[allow(dead_code)]
const MOD: u64 = 1_000_000_007;

fn main() {
	read_init!(buf);
	let (n, m, t): (usize, usize, u64) = (read!(buf), read!(buf), read!(buf));
	let v: Vec<u64> = (0..n).map(|_| read!(buf)).collect(); // A
	let (a, b, c): (Vec<usize>, Vec<usize>, Vec<u64>) = (0..m)
		.map(|_| (read!(buf, usize), read!(buf, usize), read!(buf, u64)))
		.fold((Vec::with_capacity(m), Vec::with_capacity(m), Vec::with_capacity(m)), |(mut va, mut vb, mut vc), (ua, ub, uc)| {
			va.push(ua - 1);
			vb.push(ub - 1);
			vc.push(uc);
			(va, vb, vc)
		});

	println!("{}", solve(n, m, t, v, a, b, c));
}

fn solve(n: usize, m: usize, t: u64, v: Vec<u64>, a: Vec<usize>, b: Vec<usize>, c: Vec<u64>) -> u64 {
	let mut graph1 = Graph {
		edge: vec![vec![]; n],
		size: n
	};
	let mut graph2 = Graph {
		edge: vec![vec![]; n],
		size: n
	};
	for i in 0..m {
		graph1.edge[a[i]].push((b[i], c[i]));
		graph2.edge[b[i]].push((a[i], c[i]));
	}

	let distances_from_zero = dijkstra(&graph1, 0);
	let distances_to_zero = dijkstra(&graph2, 0);
	let mut result: u64 = 0;
	for (d, &a) in distances_from_zero.iter().zip(distances_to_zero.iter()).zip(v.iter()) {
		match d {
			(Some(x), Some(y)) => result = max(result, t.saturating_sub(x + y) * a),
			_ => ()
		}
	}
	result
}

fn dijkstra(graph: &Graph, start: usize) -> Vec<Option<u64>> {
	use std::cmp::Reverse;
	use std::collections::BinaryHeap;

	let n = graph.size;
	let mut distances: Vec<Option<u64>> = vec![None; n];
	let mut pq = BinaryHeap::new();

	distances[start] = Some(0);
	pq.push(Reverse((0, start)));
	while let Some(Reverse((distance, node))) = pq.pop() {
		if distance > distances[node].unwrap_or(INFu64) {
			continue;
		}
		for &(next_node, cost) in graph.edge[node].iter() {
			let next_distance = distance + cost;
			if next_distance < distances[next_node].unwrap_or(INFu64) {
				distances[next_node] = Some(next_distance);
				pq.push(Reverse((next_distance, next_node)));
			}
		}
	}
	distances
}

#[derive(Eq, PartialEq, Clone, Default, Debug)]
struct Graph {
	edge: Vec<Vec<(usize, u64)>>,
	size: usize
}

mod graph {
	#[allow(unused_imports)]
	use std::collections::VecDeque;
	#[allow(unused_imports)]
	use std::collections::BinaryHeap;
	#[allow(unused_imports)]
	use std::collections::HashMap;
	#[allow(unused_imports)]
	use std::hash::Hash;

	#[allow(dead_code)]
	pub trait Graph<T: Copy> {
		fn neighborhoods(&self, node: T) -> Vec<T>;
	}

	#[allow(dead_code)]
	pub trait OrderedCollection<T> {
		fn push(&mut self, value: T);
		fn pop(&mut self) -> Option<T>;
	}

	#[allow(dead_code)]
	impl<T> OrderedCollection<T> for Vec<T> {
		fn push(&mut self, value: T) {
			Vec::push(self, value);
		}

		fn pop(&mut self) -> Option<T> {
			Vec::pop(self)
		}
	}

	#[allow(dead_code)]
	impl<T> OrderedCollection<T> for VecDeque<T> {
		fn push(&mut self, value: T) {
			self.push_back(value);
		}

		fn pop(&mut self) -> Option<T> {
			self.pop_front()
		}
	}

	#[allow(dead_code)]
	#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash, Default, Debug)]
	pub struct Cost(pub usize);

	#[allow(dead_code)]
	pub trait Search<T>
	where
		Self: Sized+Graph<T>,
		T: Eq+PartialEq+Hash+Copy
	{
		fn into_search<S: OrderedCollection<T>>(&self, node: T, ord_collection: S) -> GraphSearch::<T, Self, S> {
			let mut iter = GraphSearch {
				collection: ord_collection,
				costs: HashMap::new(),
				underlying: self
			};
			iter.collection.push(node);
			iter.costs.insert(node, Cost(0));
			iter
		}
		fn bfs(&self, node: T) -> Bfs::<T, Self> {
			self.into_search(node, VecDeque::new())
		}
		fn dfs(&self, node: T) -> Dfs::<T, Self> {
			self.into_search(node, Vec::new())
		}
	}

	#[allow(dead_code)]
	pub struct GraphSearch<'a, T, U, S>
	where
		T: Eq+PartialEq+Hash+Copy,
		U: Graph<T>,
		S: OrderedCollection<T>
	{
		collection: S,
		costs: HashMap<T, Cost>,
		underlying: &'a U
	}

	#[allow(dead_code)]
	type Dfs<'a, T, U> = GraphSearch<'a, T, U, Vec<T>>;
	#[allow(dead_code)]
	type Bfs<'a, T, U> = GraphSearch<'a, T, U, VecDeque<T>>;

	#[allow(dead_code)]
	impl<'a, T, U, S> Iterator for GraphSearch<'a, T, U, S>
	where
		T: Eq+PartialEq+Hash+Copy,
		U: Graph<T>,
		S: OrderedCollection<T>
	{
		type Item = (T, Cost);

		fn next(&mut self) -> Option<(T, Cost)> {
			if let Some(node) = self.collection.pop() {
				let cost: Cost = *self.costs.get(&node).unwrap();
				for next_node in self.underlying.neighborhoods(node) {
					if !self.costs.contains_key(&next_node) {
						self.costs.insert(next_node, Cost(cost.0 + 1));
						self.collection.push(next_node);
					}
				}
				Some((node, cost))
			} else {
				None
			}
		}
	}
}

mod iter_utils {
	#[allow(dead_code)]
	struct CumulativeSum<I: Iterator> {
		next: Option<u64>,
		underlying: I
	}

	#[allow(dead_code)]
	impl<I> Iterator for CumulativeSum<I>
	where
		I: Iterator,
		I::Item: Into<u64>
	{
		type Item = u64;

		fn next(&mut self) -> Option<u64> {
			match (self.underlying.next(), self.next) {
				(Some(x), Some(y)) => {
					self.next = Some(x.into() + y);
					Some(y)
				},
				(None, Some(y)) => {
					self.next = None;
					Some(y)
				},
				_ => None
			}
		}
	}

	#[allow(dead_code)]
	trait CumulativeSumExt: Iterator {
		fn cumulative_sum(self) -> CumulativeSum<Self>
		where
			Self: Sized,
			Self::Item: Into<u64>
		{
			CumulativeSum {
				next: Some(0u64),
				underlying: self
			}
		}
	}

	#[allow(dead_code)]
	impl<I: Iterator> CumulativeSumExt for I {}
}
