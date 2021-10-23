use competitive::atcoder::{Atcoder, Solver};
use itertools::*;

fn main() {
    let mut atcoder = Atcoder::default();
    atcoder.run(Global::default())
}

const PIECE_NUM: usize = 8;
const NODE_NUM: usize = PIECE_NUM + 1;

use std::collections::{HashSet, VecDeque};

#[derive(Clone, PartialEq, Eq, Debug, Default)]
struct Global {
    edges: Vec<Vec<usize>>,
}

impl Global {
    pub fn check(&self, state: &[isize]) -> bool {
        for i in 0..PIECE_NUM {
            if state[i] != i as isize {
                return false;
            }
        }
        true
    }

    pub fn bfs(&mut self, state: Vec<isize>, empty: usize) -> Option<usize> {
        let mut que = VecDeque::new();
        let mut used = HashSet::new();

        que.push_back((state, empty, 0));
        while !que.is_empty() {
            let (state, empty, depth) = que.pop_front().unwrap();
            if self.check(&state) {
                return Some(depth);
            }

            used.insert(state.clone());

            for &next in self.edges[empty].clone().iter() {
                let mut next_state = state.clone();
                next_state.swap(empty, next);
                if used.contains(&next_state) {
                    continue;
                }
                que.push_back((next_state, next, depth + 1));
            }
        }
        None
    }
}

impl Solver for Global {
    type Result = isize;

    fn solve<R: std::io::Read>(
        &mut self,
        input: &mut competitive::input::Input<R>,
    ) -> <Self as Solver>::Result {
        let m = input.read();
        let uv: Vec<(usize, usize)> = (0..m)
            .map(|_| (input.read::<usize>() - 1, input.read::<usize>() - 1))
            .collect_vec();
        let p: Vec<usize> = (0..PIECE_NUM)
            .map(|_| input.read::<usize>() - 1)
            .collect_vec();

        self.edges = vec![vec![]; NODE_NUM];
        for i in 0..m {
            self.edges[uv[i].0].push(uv[i].1);
            self.edges[uv[i].1].push(uv[i].0);
        }

        let mut piece_on_node = vec![-1; NODE_NUM];
        for i in 0..PIECE_NUM {
            piece_on_node[p[i]] = i as isize;
        }
        let empty = {
            let mut e = 0;
            for i in 0..PIECE_NUM {
                if piece_on_node[i] == -1 {
                    e = i;
                }
            }
            e
        };

        let result = self.bfs(piece_on_node, empty);

        if let Some(r) = result {
            r as isize
        } else {
            -1
        }
    }
}
