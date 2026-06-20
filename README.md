# atcoder

競技プログラミング用のコード置き場です。主に AtCoder の提出コードを蓄積しつつ、再利用しやすいライブラリを言語別に管理しています。

## Profiles

- AtCoder: <https://atcoder.jp/users/binununn>
- AOJ: <http://judge.u-aizu.ac.jp/onlinejudge/user.jsp?id=binununn>
- Codeforces: <https://codeforces.com/profile/binununn>

## Repository Layout

- `AtCoder/`: AtCoder の提出コード
- `AtCoder-rs/`: Rust での AtCoder 提出コード
- `AOJ/`: Aizu Online Judge の提出コード
- `Codeforces/`: Codeforces の提出コード
- `GCJ/`: Google Code Jam の提出コード
- `library/common-lisp/`: Common Lisp 向けの競プロライブラリ
- `library/cpp/`: C++ のスニペット集
- `library/rust/`: Rust のテンプレート類

## Common Lisp Library

`library/common-lisp/` には、AtCoder などで使うことを想定した Common Lisp ライブラリをまとめています。

含まれる主なモジュール:

- 基本ユーティリティ: `utility.syntax`, `utility.number`, `utility.base`, `utility.window`
- 数学: `math`, `mint`
- データ構造: `deque`, `vector-bintree`, `binary-heap`, `ordered-map`, `union-find`, `segment-tree`, `trie`
- 線形代数・幾何: `linalg.vector`, `linalg.matrix`, `linalg.geometry`
- グラフ・アルゴリズム: `graph`, `algorithm`
- 入出力・補助: `input`, `match`, `amb`, `atcoder.test`

### Test

テストは `rove` ベースです。

~~~bash
make -C library/common-lisp test
~~~

### Template Generation

提出用に 1 ファイルへ連結したい場合は `template.lisp` を生成します。`template.lisp` は生成物なので、編集対象は `src/*.lisp` です。

~~~bash
make -C library/common-lisp template
~~~

## Notes

- ルート README は全体像の案内に留めています。
- Common Lisp ライブラリの運用メモは [`AGENTS.md`](./AGENTS.md) にあります。
