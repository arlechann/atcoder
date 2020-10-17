use std::cmp::min;

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
	}

	let mut s: Vec<Vec<char>> = Vec::new();
	for _ in 0..n {
		read_line! {
			let line: String;
		}
		let line: Vec<char> = line.chars().collect();
		s.push(line);
	}

	println!("{}", solve(n, &s));
}

fn solve(n: usize, s: &[Vec<char>]) -> usize {
	let ab: usize = s.iter().map(|line| count_ab(line)).sum();
	let start_with_b = s.iter().filter(|line| is_start_with(line, 'B')).count();
	let end_with_a = s.iter().filter(|line| is_end_with(line, 'A')).count();
	let start_with_b_and_end_with_a = s.iter().filter(|line| is_start_with(line, 'B') && is_end_with(line, 'A')).count();
	ab + min(start_with_b, end_with_a) - if (start_with_b & end_with_a) != 0 && start_with_b_and_end_with_a == start_with_b && start_with_b_and_end_with_a == end_with_a { 1 } else { 0 }
}

fn count_ab(line: &[char]) -> usize {
	line.windows(2).filter(|p| p[0] == 'A' && p[1] == 'B').count()
}

fn is_start_with(line: &[char], c: char) -> bool {
	line.iter().next().filter(|&x| *x == c).is_some()
}

fn is_end_with(line: &[char], c: char) -> bool {
	line.iter().last().filter(|&x| *x == c).is_some()
}
