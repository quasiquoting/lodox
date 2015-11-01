(defmodule unit-lodox-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from ltest
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "ltest/include/ltest-macros.lfe")


(deftest code-change
  ;; FIXME: This unit test fails by default.
  (is-equal #(ok "data")
            (ld--server:code_change "old version"
                                    "state"
                                    "extra")))
