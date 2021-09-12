use competitive::solver::{Input, Output, Solver};

fn main() {
    let input = Input::<std::io::Stdin>::default();
    let output = Output::<std::io::Stdout>::default();
    let mut solver = Solver::with_io(input, output);
    solver.run()
}

mod competitive {
    #[allow(dead_code)]
    pub mod solver {
        use std::fmt::{Debug, Display, Formatter};
        use std::io::{stdin, stdout, Read, Stdin, Stdout, Write};
        use std::str::FromStr;

        pub struct Solver<T: Read, U: Write> {
            input: Input<T>,
            output: Output<U>,
        }

        impl<T: Read, U: Write> Solver<T, U> {
            pub fn with_io(input: Input<T>, output: Output<U>) -> Self {
                Self { input, output }
            }

            pub fn run(&mut self) {
                let result = self.solve().into();
                self.output.write(result);
            }

            fn solve(&mut self) -> OutputType {
                let a = self.input.read::<usize>();
                let b = self.input.read::<usize>();

                if a + b >= 10 {
                    "error".to_string().into()
                } else {
                    (a + b).into()
                }
            }
        }

        static mut _INPUT_BUF: String = String::new();

        fn write_buf<T: Read>(source: &mut T) {
            unsafe {
                source.read_to_string(&mut _INPUT_BUF).unwrap();
            }
        }

        fn read_buf() -> &'static str {
            unsafe { &_INPUT_BUF }
        }

        pub struct Input<T: Read> {
            source: T,
            iter: Box<dyn Iterator<Item = &'static str>>,
        }

        impl<T: Read> Input<T> {
            pub fn new(mut source: T) -> Self {
                write_buf(&mut source);
                let iter = Box::new(read_buf().split_ascii_whitespace());
                Self { source, iter }
            }

            pub fn read<U>(&mut self) -> U
            where
                U: FromStr,
                U::Err: Debug,
            {
                self.iter.next().unwrap().parse().unwrap()
            }
        }

        impl Default for Input<Stdin> {
            fn default() -> Self {
                Self::new(stdin())
            }
        }

        #[allow(non_camel_case_types)]
        pub enum OutputType {
            iSize(isize),
            Int32(i32),
            Int64(i64),
            uSize(usize),
            uInt32(u32),
            uInt64(u64),
            Bool(bool),
            String(String),
            Vec(Vec<OutputType>),
        }

        impl From<isize> for OutputType {
            fn from(v: isize) -> Self {
                Self::iSize(v)
            }
        }

        impl From<i32> for OutputType {
            fn from(v: i32) -> Self {
                Self::Int32(v)
            }
        }

        impl From<i64> for OutputType {
            fn from(v: i64) -> Self {
                Self::Int64(v)
            }
        }

        impl From<usize> for OutputType {
            fn from(v: usize) -> Self {
                Self::uSize(v)
            }
        }

        impl From<u32> for OutputType {
            fn from(v: u32) -> Self {
                Self::uInt32(v)
            }
        }

        impl From<u64> for OutputType {
            fn from(v: u64) -> Self {
                Self::uInt64(v)
            }
        }

        impl From<bool> for OutputType {
            fn from(v: bool) -> Self {
                Self::Bool(v)
            }
        }

        impl From<String> for OutputType {
            fn from(v: String) -> Self {
                Self::String(v)
            }
        }

        impl<T: Into<OutputType>> From<Vec<T>> for OutputType {
            fn from(v: Vec<T>) -> Self {
                Self::Vec(v.into_iter().map(|e| e.into()).collect())
            }
        }

        impl Display for OutputType {
            fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
                match self {
                    Self::iSize(value) => write!(f, "{}", *value),
                    Self::Int32(value) => write!(f, "{}", *value),
                    Self::Int64(value) => write!(f, "{}", *value),
                    Self::uSize(value) => write!(f, "{}", *value),
                    Self::uInt32(value) => write!(f, "{}", *value),
                    Self::uInt64(value) => write!(f, "{}", *value),
                    Self::String(value) => write!(f, "{}", *value),
                    Self::Bool(value) => {
                        if *value {
                            write!(f, "Yes")
                        } else {
                            write!(f, "No")
                        }
                    }
                    Self::Vec(v) => {
                        write!(
                            f,
                            "{}",
                            v.into_iter()
                                .map(|e| format!("{}", e))
                                .collect::<Vec<_>>()
                                .join("\n")
                        )
                    }
                }
            }
        }

        pub struct Output<T: Write>(T);

        impl<T: Write> Output<T> {
            pub fn new(destination: T) -> Self {
                Self(destination)
            }

            pub fn write(&mut self, result: OutputType) {
                self.0.write_fmt(format_args!("{}\n", result)).unwrap();
            }
        }

        impl Default for Output<Stdout> {
            fn default() -> Self {
                Self::new(stdout())
            }
        }
    }
}
