use std::cmp::max;

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
	}

	let mut s = vec![0u8; n];

	loop {
		println!("{}", String::from_utf8(s.iter().map(|&c| c + 'a' as u8).collect::<Vec<_>>()).unwrap());
		if is_end(n, &s) {
			break;
		}
		count_up(n, &mut s);
	}
}

fn count_up(n: usize, s: &mut [u8]) {
	let m = s.iter().scan(0, |mx, &e| {
		let ret = *mx;
		*mx = max(*mx, e);
		Some(ret)
	}).collect::<Vec<_>>();
	let mut is_done = false;
	for (e, &m) in s.iter_mut().zip(m.iter()).rev() {
		if is_done {
			break;
		} else if *e > m {
			*e = 0;
		} else {
			*e += 1;
			is_done = true;
		}
	}
}

fn is_end(n:usize, s: &[u8]) -> bool {
	s.windows(2).all(|w| w[0] + 1 == w[1])
}
