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
		let t: usize;
	}

	for _ in 0..t {
		test_case();
	}
}

fn test_case() {
	read_line!{
		let s: String;
	}

	println!("{}", solve(s.clone()));
}

fn solve(s: String) -> i32 {
	let atcoder = "atcoder";
	if s > atcoder.to_string() {
		return 0;
	}

	if s.chars().all(|c| c == 'a') {
		return -1;
	}

	let s = s.chars().collect::<Vec<_>>();
	let atcoder = atcoder.chars().collect::<Vec<_>>();
	atcoder.iter().enumerate().scan(true , |eq, (i, &ac)| {
		if *eq {
			let &c = s.iter().nth(i).unwrap();
			*eq = c == ac;
			s.iter().skip(i).position(|&sc| ac < sc)
		} else {
			None
		}
	}).min().unwrap() as i32
}
