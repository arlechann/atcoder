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

fn main() {
	read_line!{
		let b: char;
	}

	println!("{}", solve(b));
}

fn solve(b: char) -> char {
	match b {
		'A' => 'T',
		'C' => 'G',
		'G' => 'C',
		'T' => 'A',
		_ => '_'
 	}
}
