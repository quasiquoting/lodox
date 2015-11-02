(defmodule unit-lodox-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from ltest
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest meta-docs
  (is-equal
   '(#(ld-parse
       (#((docs 1)
          "Given a path to an LFE file or a directory containing LFE files,
return a proplist with module names as keys and for values,
proplists with keys of the form, `(fname arity)`, and their docstrings as values.")
        #((docs 2)
          "Given a filename, `file`, and a directory, `dir`, call #'docs/1 on `(filename:join dir file)`."))))
   (ld-parse:docs "src/ld-parse.lfe")))
