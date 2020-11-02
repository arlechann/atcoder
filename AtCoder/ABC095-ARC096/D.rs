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
		 	$($i:ident: $tt:tt),*
		) => {
			#[derive(Default)]
			pub struct $struct {
				$(
					$i: member_type!($tt),
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
		n: usize,
		c: i64,
		xv: [(i64, i64); n]
	}

	use iter_utils::cumulative_sum::*;

	impl Solver {
		pub fn new() -> Self {
			Default::default()
		}

		pub fn solve(&self) -> i64 {
			let (x, v): (Vec<i64>, Vec<i64>) = self.xv.iter().copied().unzip();
			let x_rev: Vec<i64> = x.iter().copied().rev().collect();
			let v_rev: Vec<i64> = v.iter().copied().rev().collect();
			let v_cumsum: Vec<i64> = v.iter().copied().cumulative_sum().skip(1).collect();
			let v_rev_cumsum: Vec<i64> = v_rev.iter().copied().cumulative_sum().skip(1).collect();

			let value_one_way_max: Vec<i64> = x
				.iter()
				.zip(v_cumsum.iter())
				.map(|(&x, &v)| v - x)
				.scan(0, |state, v| {
					*state = max(*state, v);
					Some(*state)
				})
				.collect();
			let value_both_way_max: Vec<i64> = x
				.iter()
				.zip(v_cumsum.iter())
				.map(|(&x, &v)| v - x * 2)
				.scan(0, |state, v| {
					*state = max(*state, v);
					Some(*state)
				})
				.collect();

			let value_rev_one_way_max: Vec<i64> = x_rev
				.iter()
				.zip(v_rev_cumsum.iter())
				.map(|(&x, &v)| v - self.c + x)
				.scan(-1000000000000000, |state, v| {
					*state = max(*state, v);
					Some(*state)
				})
				.collect();
			let value_rev_both_way_max: Vec<i64> = x_rev
				.iter()
				.zip(v_rev_cumsum.iter())
				.map(|(&x, &v)| v - (self.c - x) * 2)
				.scan(-1000000000000000, |state, v| {
					*state = max(*state, v);
					Some(*state)
				})
				.collect();

			let left_first: Vec<i64> = value_both_way_max
				.iter()
				.rev()
				.copied()
				.chain(vec![0])
				.chain(value_rev_one_way_max.iter().copied())
				.collect();
			let right_first: Vec<i64> = value_one_way_max
				.iter()
				.rev()
				.copied()
				.chain(vec![0])
				.chain(value_rev_both_way_max.iter().copied())
				.collect();

			let left_first_max: i64 = left_first
				.windows(self.n + 1)
				.map(|w| w[0] + w[self.n as usize])
				.max()
				.unwrap();
			let right_first_max: i64 = right_first
				.windows(self.n + 1)
				.map(|w| w[0] + w[self.n as usize])
				.max()
				.unwrap();

			max(left_first_max, right_first_max)
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
		pub struct CumulativeSum<I: Iterator> {
			next: Option<i64>,
			underlying: I,
		}

		impl<I> Iterator for CumulativeSum<I>
		where
			I: Iterator,
			I::Item: Into<i64>,
		{
			type Item = i64;

			fn next(&mut self) -> Option<i64> {
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
				Self::Item: Into<i64>,
			{
				CumulativeSum {
					next: Some(0i64),
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
