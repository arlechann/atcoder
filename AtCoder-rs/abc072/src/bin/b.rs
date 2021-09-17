use competitive::atcoder::{Atcoder, Solver};

fn main() {
    let mut atcoder = Atcoder::default();
    atcoder.run(Global::default())
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
struct Global {}

impl Solver for Global {
    type Result = String;

    fn solve<R: std::io::Read>(
        &mut self,
        input: &mut competitive::input::Input<R>,
    ) -> <Self as Solver>::Result {
        input
            .read::<String>()
            .chars()
            .enumerate()
            .filter(|&(i, _)| i % 2 == 0)
            .map(|(_, c)| c)
            .collect::<String>()
    }
}
