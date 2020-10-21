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
const MOD: u64 = 1_000_000_007;

fn main() {
	read_init!(buf);
	let (h, w): (usize, usize) = (read!(buf), read!(buf)); // (R, C)
	let (sy, sx): (usize, usize) = (read!(buf), read!(buf));
	let (gy, gx): (usize, usize) = (read!(buf), read!(buf));
	let c: Vec<Vec<char>> = (0..h).map(|_| read!(buf, String).chars().collect()).collect();

	println!("{}", solve(h, w, sy, sx, gy, gx, c));
}

fn solve(h: usize, w: usize, sy: usize, sx: usize, gy: usize, gx: usize, c: Vec<Vec<char>>) -> usize {
	let maze = Maze {
		maze: c,
		height: h,
		width: w
	};
	maze.bfs((sy - 1, sx - 1)).find(|&((y, x), _)| y == gy - 1 && x == gx - 1).map(|(_, cost)| cost.0).unwrap()
}

#[derive(Eq, PartialEq, Clone, Default, Debug)]
struct Maze {
	maze: Vec<Vec<char>>,
	height: usize,
	width: usize
}

use graph::Graph;
impl Graph<(usize, usize)> for Maze {
	fn neighborhoods(&self, node: (usize, usize)) -> Vec<(usize, usize)> {
		const DY_DX: [(isize, isize); 4] = [(0, 1), (1, 0), (0, -1), (-1, 0)];

		let (y, x) = node;
		DY_DX.iter()
			.copied()
			.map(|(dy, dx)| ((y as isize + dy) as usize, (x as isize + dx) as usize))
			.filter(|&(y, x)| self.maze[y][x] == '.')
			.collect()
	}
}

use graph::IntoBfs;
impl IntoBfs<(usize, usize)> for Maze {}

mod graph {
	#[allow(unused_imports)]
	use std::collections::VecDeque;
	#[allow(unused_imports)]
	use std::collections::HashMap;
	#[allow(unused_imports)]
	use std::hash::Hash;

	#[allow(dead_code)]
	pub trait Graph<T: Copy> {
		fn neighborhoods(&self, node: T) -> Vec<T>;
	}

	#[allow(dead_code)]
	#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash, Default, Debug)]
	pub struct Cost(pub usize);

	#[allow(dead_code)]
	pub trait IntoBfs<T>
	where
		Self: Sized+Graph<T>,
		T: Eq+PartialEq+Hash+Copy
	{
		fn bfs(&self, node: T) -> Bfs::<T, Self> {
			let mut iter = Bfs {
				queue: VecDeque::new(),
				costs: HashMap::new(),
				underlying: self
			};
			iter.queue.push_back(node);
			iter.costs.insert(node, Cost(0));
			iter
		}
	}

	#[allow(dead_code)]
	pub struct Bfs<'a, T, U>
	where
		T: Eq+PartialEq+Hash+Copy,
		U: Graph<T>
	{
		queue: VecDeque<T>,
		costs: HashMap<T, Cost>,
		underlying: &'a U
	}

	#[allow(dead_code)]
	impl<'a, T, U> Iterator for Bfs<'a, T, U>
	where
		T: Eq+PartialEq+Hash+Copy,
		U: Graph<T>
	{
		type Item = (T, Cost);

		fn next(&mut self) -> Option<(T, Cost)> {
			if let Some(node) = self.queue.pop_front() {
				let cost: Cost = *self.costs.get(&node).unwrap();
				for next_node in self.underlying.neighborhoods(node) {
					if !self.costs.contains_key(&next_node) {
						self.costs.insert(next_node, Cost(cost.0 + 1));
						self.queue.push_back(next_node);
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
