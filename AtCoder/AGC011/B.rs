#[allow(unused_imports)]
use std::io::Read;
#[allow(unused_imports)]
use std::cmp::max;
#[allow(unused_imports)]
use std::cmp::min;

macro_rules! read {
	($i:ident) => {
		$i.next().unwrap().parse().unwrap();
	};
}

macro_rules! read_init {
	($i:ident) => {
		read_init!(stdin, $i)
	};
	(stdin, $i:ident) => {
		let mut _buf = String::new();
		std::io::stdin().read_to_string(&mut _buf).unwrap();
		let mut $i = _buf.split_whitespace();
	};
}

#[allow(dead_code)]
const MOD: usize = 1_000_000_007;

fn main() {
	read_init!(buf);
	let n: usize = read!(buf);
	let a: Vec<u64> = (0..n).map(|_| read!(buf)).collect();

	println!("{}", solve(n, a));
}

fn solve(n: usize, mut a: Vec<u64>) -> u64 {
	a.sort();
	let a_cumsum = a.iter().copied().cumulative_sum().collect::<Vec<_>>();
	a_cumsum.iter().zip(a).fold(0, |acc, (x, a)| { if x * 2 < a { 0 } else { acc + 1 } }) + 1
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
