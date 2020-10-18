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
		let n: usize;
		let p: usize;
	}
	let mut buf = String::new();
	std::io::stdin().read_line(&mut buf).unwrap();
	let iter = buf.split_whitespace();
	let a: Vec<usize> = iter.map(|x| x.parse().unwrap()).collect();

	println!("{}", solve(n, p, &a));
}

fn solve(n: usize, p: usize, a: &[usize]) -> u64 {
	let odd_count: usize = count_odd(a);
	let even_count: usize = n - odd_count;

	std::cmp::max(1 - p as u64, 2u64.pow(odd_count as u32) / 2) * 2u64.pow(even_count as u32)
}

fn count_odd(a: &[usize]) -> usize {
	a.iter().filter(|&x| x % 2 == 1).count()
}
