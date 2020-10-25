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

	println!("{}", solve());
}

fn solve() -> usize {
	todo!()
}

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
				.scan(0u32, |s, &e| {
					*s = *s + e;
					Some(*s)
				})
				.collect()
		}
	}
}

mod iter_utils {
	#[allow(dead_code)]
	struct CumulativeSum<I: Iterator> {
		next: Option<u64>,
		underlying: I,
	}

	#[allow(dead_code)]
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

	#[allow(dead_code)]
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

	#[allow(dead_code)]
	impl<I: Iterator> CumulativeSumExt for I {}
}
