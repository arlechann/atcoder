use std::collections::HashSet;

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
        let n = input.read::<usize>();
        let la = (0..n)
            .map(|_| {
                let l = input.read();
                let a = (0..l).map(|_| input.read::<u64>()).collect_vec();
                (l, a)
            })
            .collect_vec();

        let mut s = HashSet::new();

        for la in la {
            s.insert(la);
        }

        s.len()
    }
}
