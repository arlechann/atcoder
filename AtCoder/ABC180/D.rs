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
	read_line!{
		let x: u128;
		let y: u128;
		let a: u128;
		let b: u128;
	}

	println!("{}", solve(x, y, a, b));
}

fn solve(x: u128, y: u128, a: u128, b: u128) -> u128 {
	let mut x: u128 = x;
	let mut y: u128 = y;
	
	y -= 1;
	let mut count_a: u128 = 0;
	while x * (a - 1) < b && x * a <= y {
		x *= a;
		count_a += 1;
	}
	let count_b: u128 = (y - x) / b;
	count_a + count_b
}
