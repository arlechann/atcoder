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

struct Solver {
	s: String,
	x: isize,
	y: isize,
}

impl Solver {
	fn with_input() -> Self {
		read_init!(buf);
		Self {
			s: read!(buf),
			x: read!(buf),
			y: read!(buf),
		}
	}

	fn solve(&mut self) -> bool {
		let x: Vec<isize> = self
			.s
			.split("T")
			.enumerate()
			.filter(|&(i, _)| i % 2 == 0)
			.map(|(_, s)| s.len() as isize)
			.collect();
		let y: Vec<isize> = self
			.s
			.split("T")
			.enumerate()
			.filter(|&(i, _)| i % 2 == 1)
			.map(|(_, s)| s.len() as isize)
			.collect();

		self.x -= x.iter().next().unwrap();
		let x: Vec<isize> = x.iter().skip(1).copied().collect();

		let x_sum = x.iter().sum::<isize>();
		let y_sum = y.iter().sum::<isize>();
		if (x_sum + self.x) % 2 == 1
			|| (y_sum + self.y) % 2 == 1
			|| x_sum < self.x.abs()
			|| y_sum < self.y.abs()
		{
			return false;
		}
		let x_turn = ((x_sum + self.x) / 2).abs() as usize;
		let y_turn = ((y_sum + self.y) / 2).abs() as usize;

		let mut x_dp: Vec<bool> = vec![false; x_turn + 1];
		x_dp[0] = true;
		for &x in x.iter() {
			for i in 0..=x_turn {
				if x_dp[i] && (i + x as usize) <= x_turn {
					x_dp[i + x as usize] = true;
				}
			}
		}

		let mut y_dp: Vec<bool> = vec![false; y_turn + 1];
		y_dp[0] = true;
		for &y in y.iter() {
			for i in 0..=y_turn {
				if y_dp[i] && (i + y as usize) <= y_turn {
					y_dp[i + y as usize] = true;
				}
			}
		}

		x_dp[x_turn] && y_dp[y_turn]
	}
}

fn main() {
	let mut solver = Solver::with_input();
	println!("{}", if solver.solve() { "Yes" } else { "No" });
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
