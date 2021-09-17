use competitive::atcoder::{Atcoder, Solver};

fn main() {
    let mut atcoder = Atcoder::default();
    atcoder.run(Global::default())
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
struct Global {
    x: i64,
    t: i64,
}

impl Solver for Global {
    type Result = i64;

    fn solve<R: std::io::Read>(
        &mut self,
        input: &mut competitive::input::Input<R>,
    ) -> <Self as Solver>::Result {
        self.x = input.read();
        self.t = input.read();
        std::cmp::max::<i64>(self.x - self.t, 0)
    }
}
