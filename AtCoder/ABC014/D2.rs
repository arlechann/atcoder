fn main() {
	let mut solver = solve::Solver::new();
	solver.input();
	println!("{}", solver.solve());
}

#[allow(dead_code)]
mod solve {
	use super::*;
	#[allow(unused_imports)]
	use std::cmp::max;
	#[allow(unused_imports)]
	use std::cmp::min;
	#[allow(unused_imports)]
	use std::fmt::Debug;

	const MOD: u64 = 1_000_000_007;

	macro_rules! def {
		(
			struct = $struct:ident;
			method = $method:ident(&mut $self:ident);
			global = {
				$($gi:ident: $gtt:ty),*
			};
		 	$($i:ident: $tt:tt),*
		) => {
			#[derive(Default)]
			pub struct $struct {
				$(
					$i: member_type!($tt),
				)*
				$(
					$gi: $gtt,
				)*
				input: input::Input,
			}

			impl $struct {
				pub fn $method(&mut $self) {
					$(
						$self.$i = read_type!($self; $tt);
					)*
				}
			}
		};
	}

	macro_rules! member_type {
		([$tt:tt; $i:ident]) => {
			Vec<member_type!($tt)>
		};
		([$tt:tt; $e:expr]) => {
			Vec<member_type!($tt)>
		};
		(($($ty:ty),*)) => {
			($(member_type!($ty),)*)
		};
		($ty:ty) => {
			$ty
		};
	}

	macro_rules! read_type {
		($self:ident; [$tt:tt; $i:ident]) => {
			(0..$self.$i).map(|_| read_type!($self; $tt)).collect()
		};
		($self:ident; [$tt:tt; $e:expr]) => {
			(0..$e).map(|_| read_type!($self; $tt)).collect()
		};
		($self:ident; ($($ty:ty),*)) => {
			($(read_type!($self; $ty),)*)
		};
		($self:ident; $ty:ty) => {
			$self.input.read::<$ty>()
		};
	}

	def! {
		struct = Solver;
		method = input(&mut self);
		global = {};
		n: usize,
		xy: [(usize, usize); (self.n - 1)],
		q: usize,
		ab: [(usize, usize); q]
	}

	use doubling::Doubling;
	use graph::*;

	#[derive(Default, Debug)]
	struct Tree {
		root: Node,
		parents: Vec<Node>,
		depths: Vec<Depth>,
		size: usize,
	}

	impl Tree {
		fn new(edge_list: &[(usize, usize)], size: usize, root: usize) -> Self {
			let mut edges = vec![vec![]; size];
			for &(x, y) in edge_list.iter() {
				edges[x - 1].push(y - 1);
				edges[y - 1].push(x - 1);
			}

			let mut tree = Self {
				root: Node(root),
				parents: vec![Node(0); size],
				depths: vec![Depth(0); size],
				size: size,
			};
			tree.dfs(&edges, Node(0), Node(0), Depth(0));
			tree
		}

		fn dfs(&mut self, edges: &[Vec<usize>], node: Node, prev: Node, depth: Depth) {
			self.parents[node.0] = prev;
			self.depths[node.0] = depth;
			for &next in edges[node.0].iter() {
				if next != prev.0 {
					self.dfs(edges, Node(next), node, Depth(depth.0 + 1));
				}
			}
		}
	}

	impl RootedTree for Tree {
		fn root(&self) -> Node {
			self.root
		}
		fn parents(&self) -> &[Node] {
			self.parents.as_slice()
		}
		fn depths(&self) -> &[Depth] {
			self.depths.as_slice()
		}
	}

	impl Solver {
		pub fn new() -> Self {
			Default::default()
		}

		pub fn solve(&mut self) -> String {
			let mut result = Vec::with_capacity(self.q);
			let tree = Tree::new(&self.xy, self.n, 0);
			let doubling = Doubling::new(tree.parents());
			for &(a, b) in self.ab.iter() {
				let (_lca, distance) =
					tree.lowest_common_ancestor(&doubling, Node(a - 1), Node(b - 1));
				result.push((distance.0 + 1).to_string());
			}
			result.join("\n")
		}
	}
}

#[allow(dead_code)]
mod input {
	use std::fmt::Debug;
	use std::io::{stdin, Read};
	use std::str::FromStr;
	static mut _INPUT_BUF: String = String::new();

	pub struct Input {
		buf: &'static str,
		input_iter: Box<dyn Iterator<Item = &'static str>>,
	}

	impl Input {
		pub fn read<T>(&mut self) -> T
		where
			T: FromStr,
			T::Err: Debug,
		{
			self.input_iter.next().unwrap().parse().unwrap()
		}
	}

	impl Default for Input {
		fn default() -> Self {
			unsafe {
				stdin().read_to_string(&mut _INPUT_BUF).unwrap();
				let input_iter: Box<dyn Iterator<Item = &str>> =
					Box::new(_INPUT_BUF.split_whitespace());
				Self {
					buf: &_INPUT_BUF,
					input_iter,
				}
			}
		}
	}
}

#[allow(dead_code)]
mod search {
	use std::ops::*;
	pub fn bin_search<T, F>(mut ok: T, mut ng: T, pred: F) -> T
	where
		T: Copy
			+ Eq
			+ PartialEq
			+ PartialOrd
			+ Add<Output = T>
			+ Sub<Output = T>
			+ Div<Output = T>
			+ From<i8>,
		F: Fn(T) -> bool,
	{
		while max(ok, ng) - min(ok, ng) > T::from(1) {
			let middle = (ok + ng) / T::from(2);
			if pred(middle) {
				ok = middle;
			} else {
				ng = middle;
			}
		}
		ok
	}

	fn min<T: Copy + PartialOrd>(a: T, b: T) -> T {
		if a < b {
			a
		} else {
			b
		}
	}

	fn max<T: Copy + PartialOrd>(a: T, b: T) -> T {
		if a > b {
			a
		} else {
			b
		}
	}
}

#[allow(dead_code)]
mod imos {
	pub struct Imos {
		array: Vec<i64>,
		len: usize,
	}

	impl Imos {
		pub fn with_capacity(len: usize) -> Self {
			Self {
				array: vec![0; len],
				len: len,
			}
		}

		pub fn query(&mut self, left: usize, right: usize, value: i64) {
			assert!(left < right);
			self.array[left] += value;
			self.array[right] -= value;
		}

		pub fn apply(&self) -> Vec<i64> {
			self.array
				.iter()
				.scan(0i64, |s, &e| {
					*s = *s + e;
					Some(*s)
				})
				.collect()
		}
	}
}

#[allow(dead_code)]
mod doubling {
	use std::fmt::Debug;

	const USIZE_BITS: usize = 32;

	#[derive(Clone, Debug)]
	pub struct Doubling<T: Copy + Clone + Debug + From<usize> + Into<usize>> {
		doubling: Vec<Vec<T>>,
		len: usize,
	}

	impl<T: Copy + Clone + Debug + From<usize> + Into<usize>> Doubling<T> {
		pub fn new(a: &[T]) -> Self {
			let len = a.len();
			let mut doubling = vec![vec![T::from(0); len]; USIZE_BITS];
			doubling[0] = a.to_vec();
			for k in 0..USIZE_BITS - 1 {
				for i in 0..len {
					doubling[k + 1][i] = doubling[k][doubling[k][i].into()];
				}
			}
			Self {
				doubling: doubling,
				len: len,
			}
		}

		pub fn query(&self, mut now: T, mov: usize) -> T {
			for k in 0..USIZE_BITS {
				if mov & (1 << k) != 0 {
					now = self.doubling[k][now.into()];
				}
			}
			now
		}

		pub fn query_power_of_two(&self, now: T, mov: usize) -> T {
			self.doubling[mov][now.into()]
		}
	}
}

#[allow(dead_code)]
mod iter_utils {
	pub mod collect_vec {
		pub trait CollectVecExt: Iterator {
			fn collect_vec(self) -> Vec<Self::Item>
			where
				Self: Sized,
			{
				self.collect::<Vec<Self::Item>>()
			}
		}

		impl<I: Iterator> CollectVecExt for I {}
	}

	pub mod cumulative_sum {
		pub struct CumulativeSum<I: Iterator> {
			next: Option<u64>,
			underlying: I,
		}

		impl<I> Iterator for CumulativeSum<I>
		where
			I: Iterator,
			I::Item: Into<u64>,
		{
			type Item = u64;

			fn next(&mut self) -> Option<u64> {
				match (self.underlying.next(), self.next) {
					(Some(x), Some(y)) => {
						self.next = Some(x.into() + y);
						Some(y)
					}
					(None, Some(y)) => {
						self.next = None;
						Some(y)
					}
					_ => None,
				}
			}
		}

		pub trait CumulativeSumExt: Iterator {
			fn cumulative_sum(self) -> CumulativeSum<Self>
			where
				Self: Sized,
				Self::Item: Into<u64>,
			{
				CumulativeSum {
					next: Some(0u64),
					underlying: self,
				}
			}
		}

		impl<I: Iterator> CumulativeSumExt for I {}
	}

	pub mod run_length {
		pub struct RunLength<I: Iterator> {
			before: Option<I::Item>,
			underlying: I,
		}

		impl<I> Iterator for RunLength<I>
		where
			I: Iterator,
			I::Item: Eq + PartialEq + Copy,
		{
			type Item = I::Item;

			fn next(&mut self) -> Option<I::Item> {
				while let Some(x) = self.underlying.next() {
					if self.before.as_ref().filter(|&b| *b == x).is_none() {
						self.before = Some(x);
						return Some(x);
					}
				}
				None
			}
		}

		pub trait RunLengthExt: Iterator {
			fn run_length(self) -> RunLength<Self>
			where
				Self: Sized,
				Self::Item: Eq + PartialEq + Copy,
			{
				RunLength {
					before: None,
					underlying: self,
				}
			}
		}

		impl<I: Iterator> RunLengthExt for I {}
	}

	pub mod counter {
		use std::collections::HashMap;
		use std::hash::Hash;

		pub trait CounterExt: Iterator {
			fn counter(mut self) -> HashMap<Self::Item, usize>
			where
				Self: Sized,
				Self::Item: Eq + PartialEq + Hash,
			{
				let mut map = HashMap::new();
				while let Some(e) = self.next() {
					*map.entry(e).or_insert(0) += 1;
				}
				map
			}
		}

		impl<I: Iterator> CounterExt for I {}
	}
}

#[allow(dead_code)]
mod slice_utils {
	pub mod bin_search {
		pub trait LowerBoundExt<T: Eq + Ord> {
			fn lower_bound(&self, x: T) -> usize;
		}

		impl<T: Eq + Ord> LowerBoundExt<T> for [T] {
			fn lower_bound(&self, x: T) -> usize {
				let n = self.len() as isize;
				if n == 0 {
					return 0;
				}
				let mut ng: isize = -1;
				let mut ok: isize = n - 1;
				while (ok - ng).abs() > 1 {
					let middle = (ok + ng) / 2;
					if self[middle as usize] >= x {
						ok = middle;
					} else {
						ng = middle;
					}
				}
				ok as usize
			}
		}

		pub trait UpperBoundExt<T: Eq + Ord> {
			fn upper_bound(&self, x: T) -> usize;
		}

		impl<T: Eq + Ord> UpperBoundExt<T> for [T] {
			fn upper_bound(&self, x: T) -> usize {
				let n = self.len() as isize;
				if n == 0 {
					return 0;
				}
				let mut ng: isize = -1;
				let mut ok: isize = n - 1;
				while (ok - ng).abs() > 1 {
					let middle = (ok + ng) / 2;
					if self[middle as usize] > x {
						ok = middle;
					} else {
						ng = middle;
					}
				}
				ok as usize
			}
		}
	}
}

#[allow(dead_code)]
mod prime {
	use std::cmp::min;

	pub struct SieveOfEratosthenes {
		sieve: Vec<bool>,
		index: usize,
		len: usize,
	}

	impl Iterator for SieveOfEratosthenes {
		type Item = bool;

		fn next(&mut self) -> Option<bool> {
			if let Some(&is_prime) = self.sieve.get(self.index) {
				if is_prime {
					for i in (self.index..self.len).step_by(self.index) {
						self.sieve[i] = false;
					}
				}
				self.index += 1;
				Some(is_prime)
			} else {
				None
			}
		}
	}

	pub fn sieve_of_eratosthenes(n: usize) -> SieveOfEratosthenes {
		let mut ret = SieveOfEratosthenes {
			sieve: vec![true; n],
			index: 0,
			len: n,
		};
		for i in 0..(min(n, 2)) {
			ret.sieve[i] = false;
		}
		ret
	}

	pub fn primes(n: usize) -> Vec<usize> {
		sieve_of_eratosthenes(n)
			.enumerate()
			.filter(|&(_, p)| p)
			.map(|(i, _)| i)
			.collect::<Vec<_>>()
	}

	pub fn divisors(n: usize) -> Vec<usize> {
		let mut v = vec![];
		(1..=n)
			.take_while(|i| i * i <= n)
			.filter(|&i| n % i == 0)
			.for_each(|i| {
				v.push(i);
				if i * i != n {
					v.push(n / i);
				}
			});
		v
	}
}

#[allow(dead_code)]
mod union_find {
	#[derive(Eq, PartialEq, Clone, Default, Debug)]
	pub struct UnionFind {
		len: usize,
		parents: Vec<usize>,
		rank: Vec<usize>,
		size: Vec<usize>,
	}

	impl UnionFind {
		pub fn new(n: usize) -> Self {
			Self {
				len: n,
				parents: (0..n).collect(),
				rank: vec![0; n],
				size: vec![1; n],
			}
		}

		pub fn merge(&mut self, a: usize, b: usize) {
			let mut a_root: usize = self.root(a);
			let mut b_root: usize = self.root(b);
			if a_root == b_root {
				return;
			}
			if self.rank[a_root] < self.rank[b_root] {
				std::mem::swap(&mut a_root, &mut b_root);
			}
			if self.rank[a_root] == self.rank[b_root] {
				self.rank[a_root] += 1;
			}
			self.size[a_root] += self.size[b_root];
			self.parents[b_root] = a_root;
		}

		pub fn is_same(&mut self, a: usize, b: usize) -> bool {
			self.root(a) == self.root(b)
		}

		pub fn size(&mut self, n: usize) -> usize {
			let root: usize = self.root(n);
			self.size[root]
		}

		pub fn len(&self) -> usize {
			self.len
		}

		pub fn groups(&mut self) -> Vec<Vec<usize>> {
			for i in 0..self.len {
				self.root(i);
			}

			let mut ret: Vec<Vec<usize>> = (0..self.len)
				.map(|_| Vec::with_capacity(self.len))
				.collect();
			for i in 0..self.len {
				ret[self.parents[i]].push(i);
			}
			ret.into_iter().filter(|v| !v.is_empty()).collect()
		}

		fn root(&mut self, node: usize) -> usize {
			if self.parents[node] == node {
				node
			} else {
				self.parents[node] = self.root(self.parents[node]);
				self.parents[node]
			}
		}
	}
}

#[allow(dead_code)]
mod graph {
	use super::doubling::Doubling;
	use std::cmp::Ordering;
	use std::ops::Add;
	use std::ops::Sub;

	const USIZE_BITS: usize = 32;

	#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash, Debug, Default)]
	pub struct Node(pub usize);
	#[derive(Eq, PartialEq, PartialOrd, Clone, Copy, Hash, Debug, Default)]
	pub struct Edge {
		from: Node,
		to: Node,
		weight: Weight,
	}
	#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash, Debug, Default)]
	pub struct Weight(pub i64);
	#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash, Debug, Default)]
	pub struct Depth(pub usize);

	impl From<usize> for Node {
		fn from(n: usize) -> Self {
			Self(n)
		}
	}

	impl Into<usize> for Node {
		fn into(self) -> usize {
			self.0
		}
	}

	impl Ord for Edge {
		fn cmp(&self, other: &Self) -> Ordering {
			self.weight.cmp(&other.weight)
		}
	}

	impl Add for Weight {
		type Output = Self;

		fn add(self, other: Self) -> Self {
			Self(self.0 + other.0)
		}
	}

	impl Sub for Depth {
		type Output = Self;

		fn sub(self, other: Self) -> Self {
			assert!(self >= other);
			Self(self.0 - other.0)
		}
	}

	pub trait RootedTree {
		fn root(&self) -> Node;
		fn parents(&self) -> &[Node];
		fn depths(&self) -> &[Depth];

		fn parent(&self, node: Node) -> Node {
			self.parents()[node.0]
		}
		fn depth(&self, node: Node) -> Depth {
			self.depths()[node.0]
		}

		fn lowest_common_ancestor(
			&self,
			doubling: &Doubling<Node>,
			a: Node,
			b: Node,
		) -> (Node, Weight) {
			let a_depth = self.depth(a);
			let b_depth = self.depth(b);
			if a_depth > b_depth {
				return self.lowest_common_ancestor(doubling, b, a);
			}

			let depth_diff = b_depth.0 - a_depth.0;
			let mut a = a;
			let mut b = doubling.query(b, depth_diff);
			if a == b {
				return (a, Weight(depth_diff as i64));
			}

			let mut lca_depth = 0;
			for i in (0..=a_depth.0.next_power_of_two().trailing_zeros() as usize).rev() {
				let a_tmp = doubling.query_power_of_two(a, i);
				let b_tmp = doubling.query_power_of_two(b, i);
				if a_tmp != b_tmp {
					a = a_tmp;
					b = b_tmp;
					lca_depth += 1 << i;
				}
			}
			let lca = self.parent(a);
			lca_depth += 1;
			(lca, Weight((lca_depth * 2 + depth_diff) as i64))
		}
	}
}
