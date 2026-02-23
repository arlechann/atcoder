(asdf:defsystem "atcoder-library-tests"
  :depends-on ("rove" "atcoder-library-core")
  :serial t
  :pathname "./"
  :components ((:file "tests/library-test")))
