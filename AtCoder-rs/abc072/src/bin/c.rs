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
        const A_MAX: usize = 100000;

        let n: usize = input.read();
        let a: Vec<usize> = (0..n).map(|_| input.read()).collect_vec();

        let mut count = vec![0; A_MAX + 1];
        for &ai in a.iter() {
            if ai != 0 {
                count[ai - 1] += 1;
            }
            count[ai] += 1;
            if ai != A_MAX {
                count[ai + 1] += 1;
            }
        }

        count.into_iter().max().unwrap()
    }
}
