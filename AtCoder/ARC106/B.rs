fn main() {
	let mut solver = solve::Solver::new();
	solver.input();
	println!("{}", if solver.solve() { "Yes" } else { "No" });
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

	#[allow(unused_imports)]
	use union_find::UnionFind;

	const MOD: u64 = 1_000_000_007;

	#[derive(Default)]
	pub struct Solver {
		n: usize,
		m: usize,
		a: Vec<i64>,
		b: Vec<i64>,
		cd: Vec<(usize, usize)>,
		input: input::Input,
	}

	impl Solver {
		pub fn new() -> Self {
			Default::default()
		}

		pub fn input(&mut self) {
			self.n = self.input.read();
			self.m = self.input.read();
			self.a = (0..self.n).map(|_| self.input.read::<i64>()).collect();
			self.b = (0..self.n).map(|_| self.input.read::<i64>()).collect();
			self.cd = (0..self.m)
				.map(|_| {
					(
						self.input.read::<usize>() - 1,
						self.input.read::<usize>() - 1,
					)
				})
				.collect();
		}

		pub fn solve(&self) -> bool {
			let mut uf: UnionFind = UnionFind::new(self.n);
			for &(c, d) in self.cd.iter() {
				uf.merge(c, d);
			}

			let mut connect_a = vec![0; self.n];
			let mut connect_b = vec![0; self.n];
			for i in 0..self.n {
				let root = uf.root(i);
				connect_a[root] += self.a[i];
				connect_b[root] += self.b[i];
			}

			connect_a.iter().zip(connect_b.iter()).all(|(a, b)| a == b)
		}
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
				size: vec![0; n],
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

		pub fn root(&mut self, node: usize) -> usize {
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
mod imos {
	struct Imos {
		array: Vec<i64>,
		len: usize,
	}

	impl Imos {
		fn with_capacity(len: usize) -> Self {
			Self {
				array: vec![0; len],
				len: len,
			}
		}

		fn query(&mut self, left: usize, right: usize, value: i64) {
			assert!(left < right);
			self.array[left] += value;
			self.array[right] -= value;
		}

		fn apply(&self) -> Vec<i64> {
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
mod iter_utils {
	pub mod cumulative_sum {
		struct CumulativeSum<I: Iterator> {
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

		trait CumulativeSumExt: Iterator {
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
