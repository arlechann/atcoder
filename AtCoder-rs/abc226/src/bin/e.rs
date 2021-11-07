use std::collections::HashSet;

use competitive::atcoder::{Atcoder, Solver};
use itertools::*;

fn main() {
    let mut atcoder = Atcoder::default();
    atcoder.run(Global::default())
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct UnionFind {
    parents: Vec<usize>,
    rank: Vec<usize>,
    size: Vec<usize>,
    edge_count: Vec<usize>,
}

impl UnionFind {
    pub fn new(n: usize) -> Self {
        Self {
            parents: (0..n).collect(),
            rank: vec![0; n],
            size: vec![1; n],
            edge_count: vec![0; n],
        }
    }

    pub fn merge(&mut self, a: usize, b: usize) {
        let mut a_root: usize = self.root(a);
        let mut b_root: usize = self.root(b);
        if a_root == b_root {
            self.edge_count[a_root] += 1;
            return;
        }
        if self.rank[a_root] < self.rank[b_root] {
            std::mem::swap(&mut a_root, &mut b_root);
        }
        if self.rank[a_root] == self.rank[b_root] {
            self.rank[a_root] += 1;
        }
        self.size[a_root] += self.size[b_root];
        self.edge_count[a_root] += self.edge_count[b_root] + 1;
        self.parents[b_root] = a_root;
    }

    pub fn is_same(&mut self, a: usize, b: usize) -> bool {
        self.root(a) == self.root(b)
    }

    pub fn size(&mut self, n: usize) -> usize {
        let root: usize = self.root(n);
        self.size[root]
    }

    pub fn len(&self) -> usize {
        self.parents.len()
    }

    pub fn edge_count(&mut self, n: usize) -> usize {
        let r = self.root(n);
        self.edge_count[r]
    }

    pub fn parents(&mut self) -> Vec<usize> {
        let len = self.parents.len();
        for i in 0..len {
            self.root(i);
        }

        self.parents.clone()
    }

    pub fn groups(&mut self) -> Vec<Vec<usize>> {
        let len = self.parents.len();
        for i in 0..len {
            self.root(i);
        }

        let mut ret: Vec<Vec<usize>> = (0..len).map(|_| Vec::with_capacity(len)).collect();
        for i in 0..len {
            ret[self.parents[i]].push(i);
        }
        ret.into_iter().filter(|v| !v.is_empty()).collect()
    }

    fn root(&mut self, node: usize) -> usize {
        if self.parents[node] != node {
            self.parents[node] = self.root(self.parents[node]);
        }
        self.parents[node]
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
struct Global {}

const MOD: u64 = 998244353;

fn mod_pow(a: u64, n: u64) -> u64 {
    let mut a = a;
    let mut n = n;
    let mut ret = 1u64;
    while n != 0 {
        if n % 2 != 0 {
            ret *= a;
            ret %= MOD;
        }
        a *= a;
        a %= MOD;
        n /= 2;
    }
    ret
}

impl Solver for Global {
    type Result = usize;

    fn solve<R: std::io::Read>(
        &mut self,
        input: &mut competitive::input::Input<R>,
    ) -> <Self as Solver>::Result {
        let n = input.read();
        let m = input.read();
        let uv = (0..m)
            .map(|_| (input.read::<usize>() - 1, input.read::<usize>() - 1))
            .collect_vec();

        if n != m {
            return 0;
        }

        let mut uf = UnionFind::new(n);
        for &(u, v) in uv.iter() {
            uf.merge(u, v);
        }

        for i in 0..n {
            if uf.edge_count(i) != uf.size(i) {
                return 0;
            }
        }

        mod_pow(
            2,
            uf.parents().into_iter().collect::<HashSet<_>>().len() as u64,
        ) as usize
    }
}
