(defmodule unit-lodox-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from ltest
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "ltest/include/ltest-macros.lfe")


(deftest two-plus-two
  (is-equal 4 (+ 2 2)))

(deftest meta-docs
  (is-equal
   '(#([docs 1]
       "Given a path to an LFE file, return a proplist with keys of the form,
`(fname arity)`, and their docstrings as values."))
   (ld-parse:docs "src/ld-parse.lfe")))
