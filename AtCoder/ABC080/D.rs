#[allow(unused_imports)]
use std::cmp::max;
#[allow(unused_imports)]
use std::cmp::min;
#[allow(unused_imports)]
use std::fmt::Debug;
#[allow(unused_imports)]
use std::io::{stdin, Read};

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
	let (n, c): (usize, usize) = (read!(buf), read!(buf));
	let stc: Vec<(usize, usize, usize)> = (0..n)
		.map(|_| (read!(buf), read!(buf), read!(buf)))
		.collect();

	println!("{}", solve(n, c, stc));
}

fn solve(n: usize, c: usize, stc: Vec<(usize, usize, usize)>) -> usize {
	let t_max = stc.iter().map(|(s, t, c)| t).cloned().max().unwrap_or(0);
	let mut imos = Imos::new(t_max + 3);
	for &(s, t, c) in stc.iter() {
		imos.query(s, t + 1, 1 << (c - 1));
	}
	imos.apply()
		.into_iter()
		.map(|x| x.count_ones())
		.max()
		.unwrap_or(0) as usize
}

struct Imos {
	array: Vec<u32>,
	len: usize,
}

impl Imos {
	fn new(len: usize) -> Self {
		Self {
			array: vec![0; len],
			len: len,
		}
	}

	fn query(&mut self, left: usize, right: usize, value: u32) {
		assert!(left < right);
		self.array[left] ^= value;
		self.array[right] ^= value;
	}

	fn apply(&self) -> Vec<u32> {
		self.array
			.iter()
			.scan(0u32, |s, &e| {
				*s = *s ^ e;
				Some(*s)
			})
			.collect()
	}
}

#[allow(dead_code)]
mod iter_utils {
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
