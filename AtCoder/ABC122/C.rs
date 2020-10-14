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

fn main() {
	read_line!{
		let n: usize;
		let q: usize;
	}
	read_line!{
		let s: String;
	}

	let s = s.chars().collect::<Vec<_>>();
	let count = s.windows(2).map(|a| if a[0] == 'A' && a[1] == 'C' { 1 } else { 0 }).collect::<Vec<usize>>();
	let csum = count.into_iter().cumulative_sum().collect::<Vec<usize>>();

	for _ in 0..q {
		read_line!{
			let l: usize;
			let r: usize;
		}
		println!("{}", solve(&csum, l, r));
	}
}

fn solve(csum: &[usize], l: usize, r: usize) -> usize {
	csum[r - 1] - csum[l - 1]
}
