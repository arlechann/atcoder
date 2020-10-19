#[allow(unused_imports)]
use std::io::{stdin, Read};
#[allow(unused_imports)]
use std::cmp::max;
#[allow(unused_imports)]
use std::cmp::min;

#[allow(unused_macros)]
macro_rules! read {
	($i:ident) => {
		$i.next().unwrap().parse().unwrap();
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
	todo!();
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
