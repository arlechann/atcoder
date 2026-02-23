# AGENTS.md

このリポジトリで Common Lisp ライブラリ（`library/common-lisp`）を扱うときの運用メモ。

## 1. 生成物の扱い
- `library/common-lisp/template.lisp` は手編集しない。
- `library/common-lisp/src/*.lisp` を編集し、必要なら以下で再生成する。
  - `make -C library/common-lisp template`

## 2. テスト実行
- テストは `atcoder-library-tests`（rove）で実行する。
- `Makefile` に `test` ターゲットを追加済み。
- 実行時に `XDG_CACHE_HOME` を明示指定すること（環境依存の権限エラー回避）。
  - 例: `XDG_CACHE_HOME=/tmp make -C library/common-lisp test`
- `test` ターゲット内で `sb-ext:unlock-package :sb-di` を呼び、SBCL + rove/dissect の package lock エラーを回避済み。

## 3. テスト方針
- 外部アクセスを行う機能（`src/210-test.lisp`）は単体テストで必ずモック化する。
  - `tests/atcoder-test-test.lisp` では `symbol-function` 差し替えでネットワーク呼び出しをモック。

## 4. ここまでで追加済みの主な単体テスト
- `tests/deque-test.lisp`
- `tests/vector-bintree-test.lisp`
- `tests/binary-heap-test.lisp`
- `tests/linalg-vector-test.lisp`
- `tests/linalg-matrix-test.lisp`
- `tests/linalg-geometry-test.lisp`
- `tests/union-find-test.lisp`
- `tests/segment-tree-test.lisp`
- `tests/trie-test.lisp`
- `tests/graph-test.lisp`
- `tests/algorithm-test.lisp`
- `tests/amb-test.lisp`
- `tests/input-test.lisp`
- `tests/atcoder-test-test.lisp`

## 5. 開発時の注意
- `atcoder-library-tests.asd` に新規テストファイルを追加し忘れないこと。
- 境界チェックは実装上 `#-atcoder` 条件が多い。テスト条件と期待値を揃えること。
- 重要: `AtCoder/` 以下および `AtCoder-rs/` 以下は、競技中に LLM が参照・編集するとルール違反となり罰則を受ける可能性があるため、競技中は絶対に触れないこと。
