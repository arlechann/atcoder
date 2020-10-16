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

struct CumulativeSum<I: Iterator> {
	next: Option<usize>,
	underlying: I
}

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

impl<I: Iterator> CumulativeSumExt for I {}

const MOD: usize = 1_000_000_007;

fn main() {
	read_line!{
		let n: usize;
	}
	let mut buf = String::new();
	std::io::stdin().read_line(&mut buf).unwrap();
	let a: Vec<usize> = buf.split_whitespace().flat_map(|x| x.parse()).collect();

	println!("{}", solve(n, &a));
}

fn solve(n: usize, a: &[usize]) -> usize {
	let mut colors = [0usize; 3];
	let mut ret = 1usize;
	for &x in a.iter() {
		let mut count = 0;
		let mut is_done = false;
		for c in colors.iter_mut() {
			if x == *c {
				if !is_done {
					is_done = true;
					*c += 1;
				}
				count += 1;
			}
		}
		ret *= count;
		ret %= MOD;
	}
	ret
}
