fn main() {
	let mut buf = String::new();
	std::io::stdin().read_line(&mut buf).unwrap();

	let mut iter = buf.split_whitespace();
	let n: u32 = iter.next().unwrap().parse().unwrap();

	let mut buf = String::new();
	std::io::stdin().read_line(&mut buf).unwrap();

	let v: Vec<u32> = buf.split_whitespace().map(|x| x.parse().unwrap()).collect();

	let mut buf = String::new();
	std::io::stdin().read_line(&mut buf).unwrap();

	let c: Vec<u32> = buf.split_whitespace().map(|x| x.parse().unwrap()).collect();

	println!("{}", solve(n, &v, &c));
}

fn solve(n: u32, v: &[u32], c: &[u32]) -> u32 {
	rec(n, &v, &c, 0, 0) as u32
}

fn rec(n: u32, v: &[u32], c: &[u32], x: i32, y: i32) -> i32 {
	if n == 0 {
		return x - y;
	}
	std::cmp::max(rec(n - 1, &v[1..], &c[1..], x + v[0] as i32, y + c[0] as i32), rec(n - 1, &v[1..], &c[1..], x, y))
}