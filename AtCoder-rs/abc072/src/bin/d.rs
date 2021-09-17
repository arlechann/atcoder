use competitive::atcoder::{Atcoder, Solver};
use itertools::*;

fn main() {
    let mut atcoder = Atcoder::default();
    atcoder.run(Global::default())
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
struct Global {}

impl Solver for Global {
    type Result = usize;

    fn solve<R: std::io::Read>(
        &mut self,
        input: &mut competitive::input::Input<R>,
    ) -> <Self as Solver>::Result {
        let n: usize = input.read();
        let p: Vec<usize> = (0..n).map(|_| input.read()).collect_vec();

        let mut p = p.clone();
        let mut result = 0;
        for i in 0..n - 1 {
            if p[i] == i + 1 && p[i + 1] == i + 2 {
                p.swap(i, i + 1);
                result += 1;
            }
        }

        for i in 0..n - 1 {
            if p[i] == i + 1 {
                p.swap(i, i + 1);
                result += 1;
            }
        }

        for i in 0..n - 1 {
            if p[i + 1] == i + 2 {
                p.swap(i, i + 1);
                result += 1;
            }
        }

        result
    }
}
