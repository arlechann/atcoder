use std::io::Read;

fn main() {
	let mut buf = String::new();
	std::io::stdin().read_to_string(&mut buf).unwrap();

	let mut iter = buf.split_whitespace();
	let a: u32 = iter.next().unwrap().parse().unwrap();
	let b: u32 = iter.next().unwrap().parse().unwrap();
	let t: u32 = iter.next().unwrap().parse().unwrap();

	println!("{}", solve(a, b, t));
}

fn solve(a: u32, b: u32, t: u32) -> u32 {
	t / a * b
}