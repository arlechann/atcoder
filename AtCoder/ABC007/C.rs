#[allow(unused_imports)]
use std::io::{stdin, Read};
#[allow(unused_imports)]
use std::cmp::max;
#[allow(unused_imports)]
use std::cmp::min;
#[allow(unused_imports)]
use std::collections::VecDeque;

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
	let distances = bfs(h, w, sy - 1, sx - 1, &c);
	distances[gy - 1][gx - 1].unwrap()
}

fn bfs(h: usize, w: usize, sy: usize, sx: usize, c: &[Vec<char>]) -> Vec<Vec<Option<usize>>> {
	const dyx: [(isize, isize); 4] = [(0, 1), (1, 0), (0, -1), (-1, 0)];

	let mut distances: Vec<Vec<Option<usize>>> = (0..h).map(|_| (0..w).map(|_| None).collect()).collect();
	let mut que: VecDeque<(isize, isize, usize)> = VecDeque::new();

	que.push_back((sy as isize, sx as isize, 0));
	distances[sy][sx] = Some(0);
	while let Some((y, x, d)) = que.pop_front() {
		for (dy, dx) in dyx.iter() {
			let ny = (y + dy) as usize;
			let nx = (x + dx) as usize;
			if c[ny][nx] == '.' && distances[ny][nx].is_none() {
				que.push_back((ny as isize, nx as isize, d + 1));
				distances[ny][nx] = Some(d + 1);
			}
		}
	}
	distances
}

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
