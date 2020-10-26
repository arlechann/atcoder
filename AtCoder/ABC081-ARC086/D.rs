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
const MOD: u64 = 1_000_000_007;

fn main() {
	read_init!(buf);
	let n: usize = read!(buf);
	let a: Vec<isize> = (0..n).map(|_| read!(buf)).collect();

	let result: Vec<(usize, usize)> = solve(n, a);
	println!("{}", result.len());
	result
		.iter()
		.for_each(|(from, to)| println!("{} {}", from + 1, to + 1));
}

fn solve(n: usize, mut a: Vec<isize>) -> Vec<(usize, usize)> {
	let (a_max, a_max_index) = a
		.iter()
		.copied()
		.enumerate()
		.map(|(i, e)| (e, i))
		.max()
		.unwrap();
	let (a_min, a_min_index) = a
		.iter()
		.copied()
		.enumerate()
		.map(|(i, e)| (e, i))
		.min()
		.unwrap();

	let mut result: Vec<(usize, usize)> = Vec::new();

	let (op_v, op_i) = if a_max.abs() > a_min.abs() {
		(a_max, a_max_index)
	} else {
		(a_min, a_min_index)
	};
	for i in 0..n {
		if i != op_i {
			result.push((op_i, i));
			a[i] += a[op_i];
		}
	}

	if a_max.abs() > a_min.abs() {
		for i in 1..n {
			result.push((i - 1, i));
		}
	} else {
		for i in (1..n).rev() {
			result.push((i, i - 1));
		}
	}
	result
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
