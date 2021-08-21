mod competitive {
	#[allow(dead_code)]
	mod solver {
		pub struct Solver {}

		impl Solver {
			pub fn new() -> Self {
				todo!()
			}

			pub fn solve() {
				todo!()
			}
		}
	}

	#[allow(dead_code)]
	mod input {
		use std::fmt::Debug;
		use std::io::{stdin, Read};
		use std::str::FromStr;

		pub struct Input {}

		impl Input {
			pub fn new() -> Self {
				todo!()
			}

			pub fn read<T>(&mut self) -> T
			where
				T: FromStr,
				T::Err: Debug,
			{
				todo!()
			}
		}

		impl Default for Input {
			fn default() -> Self {
				todo!()
			}
		}
	}

	mod output {
		use std::fmt::Debug;
		use std::io::stdout;

		pub struct Output {}

		impl Output {
			pub fn new() -> Self {
				todo!()
			}
		}

		impl Default for Output {
			fn default() -> Self {
				todo!()
			}
		}
	}
}
