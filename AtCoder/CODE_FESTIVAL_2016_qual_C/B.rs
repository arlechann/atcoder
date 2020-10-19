#[allow(unused_imports)]
use std::io::Read;
#[allow(unused_imports)]
use std::cmp::max;
#[allow(unused_imports)]
use std::cmp::min;

#[allow(unused_macros)]
macro_rules! read_line {
	($(let $i:ident : $t:ty);*;) => {
		let mut buf = String::new();
		std::io::stdin().read_line(&mut buf).unwrap();
		let mut iter = buf.split_whitespace();
		$(
			let $i: $t = iter.next().unwrap().parse().unwrap();
		)*
	};
}

macro_rules! read {
	($i:ident) => {
		$i.next().unwrap().parse().unwrap();
	};
}

macro_rules! read_init {
	(stdin, $i:ident) => {
		let mut _buf = String::new();
		std::io::stdin().read_to_string(&mut _buf).unwrap();
		let mut $i = _buf.split_whitespace();
	};
}

#[allow(dead_code)]
struct CumulativeSum<I: Iterator> {
	next: Option<usize>,
	underlying: I
}

#[allow(dead_code)]
impl<I> Iterator for CumulativeSum<I>
where
	I: Iterator,
	I::Item: Into<usize>
{
	type Item = usize;

	fn next(&mut self) -> Option<usize> {
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
		Self::Item: Into<usize>
	{
		CumulativeSum {
			next: Some(0usize),
			underlying: self
		}
	}
}

#[allow(dead_code)]
impl<I: Iterator> CumulativeSumExt for I {}

#[allow(dead_code)]
const MOD: usize = 1_000_000_007;

fn main() {
	read_init!(stdin, buf);
	let k: usize = read!(buf);
	let t: usize = read!(buf);
	let a: Vec<usize> = (0..t).map(|_| read!(buf)).collect();

	println!("{}", solve(k, t, &a));
}

fn solve(k: usize, t: usize, a: &[usize]) -> usize {
	let a_max: usize = a.iter().copied().max().unwrap_or(0);
	if a_max > (k + 1) / 2 {
		(a_max - (k + 1) / 2) * 2 - (1 - k % 2)
	} else {
		0
	}
}
