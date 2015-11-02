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
   '(#(docs/0 #M(arglists (()) arity 0 doc "TODO: write docstring" name docs))
     #(docs/1
       #M(name     docs
          arity    1
          arglists ((file-or-dir))
          doc      "Given a path to an LFE file or a directory containing LFE files,
return a map from module name to orddict from fun/arity to a property map."))
     #(docs/2
       #M(name     docs
          arity    2
          arglists ((file dir))
          doc      "Given a filename, `file`, and a directory, `dir`, call #'docs/1 on `(filename:join dir file)`."))
     #(to-org/1
       #M(name     to-org
          arity    1
          arglists ((dict))
          doc      "TODO: write docstring

Project level."))
     #(to-org/2
       #M(name     to-org
          arity    2
          arglists ((dict filename))
          doc      "TODO: write docstring

Module level.")))
   (ld-parse:docs "src/ld-parse.lfe")))
