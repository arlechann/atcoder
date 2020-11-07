use input::*;

fn main() {
	let mut input: Input = Default::default();
	let cards: Vec<(usize, usize)> = (0..=100)
		.map(|i| {
			if i == 0 {
				(0, 0)
			} else {
				(input.read(), input.read())
			}
		})
		.collect();
	let state: State = init(cards);
	println!("{}", state.operations);
}

fn sa() -> State {
	todo!()
}

fn init(input: Vec<(usize, usize)>) -> State {
	let mut operations = String::new();
	for v in input.windows(2) {
		if v[1].0 < v[0].0 {
			let n = v[0].0 - v[1].0;
			operations += &"U".repeat(n);
		} else if v[1].0 > v[0].0 {
			let n = v[1].0 - v[0].0;
			operations += &"D".repeat(n);
		}
		if v[1].1 < v[0].1 {
			let n = v[0].1 - v[1].1;
			operations += &"L".repeat(n);
		} else if v[1].1 > v[0].1 {
			let n = v[1].1 - v[0].1;
			operations += &"R".repeat(n);
		}
		operations += &"I";
	}

	State { operations }
}

fn eval(s: &State) -> usize {
	todo!()
}

struct State {
	operations: String,
}

#[allow(dead_code)]
mod input {
	use std::fmt::Debug;
	use std::io::{stdin, Read};
	use std::str::FromStr;
	static mut _INPUT_BUF: String = String::new();

	pub struct Input {
		buf: &'static str,
		input_iter: Box<dyn Iterator<Item = &'static str>>,
	}

	impl Input {
		pub fn read<T>(&mut self) -> T
		where
			T: FromStr,
			T::Err: Debug,
		{
			self.input_iter.next().unwrap().parse().unwrap()
		}
	}

	impl Default for Input {
		fn default() -> Self {
			unsafe {
				stdin().read_to_string(&mut _INPUT_BUF).unwrap();
				let input_iter: Box<dyn Iterator<Item = &str>> =
					Box::new(_INPUT_BUF.split_whitespace());
				Self {
					buf: &_INPUT_BUF,
					input_iter,
				}
			}
		}
	}
}
