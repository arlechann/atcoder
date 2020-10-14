use std::io::Read;

fn main() {
	let mut buf = String::new();
	std::io::stdin().read_to_string(&mut buf).unwrap();

	let mut iter = buf.split_whitespace();
	let n: u64 = iter.next().unwrap().parse().unwrap();
	let a: u64 = iter.next().unwrap().parse().unwrap();
	let b: u64 = iter.next().unwrap().parse().unwrap();
	let c: u64 = iter.next().unwrap().parse().unwrap();
	let d: u64 = iter.next().unwrap().parse().unwrap();
	let e: u64 = iter.next().unwrap().parse().unwrap();

	println!("{}", solve(n, a, b, c, d, e));
}

fn solve(n: u64, a: u64, b: u64, c: u64, d: u64, e: u64) -> u64 {
	let min = vec![a, b, c, d, e].into_iter().min().unwrap();
	(n + min - 1) / min + 4
}
