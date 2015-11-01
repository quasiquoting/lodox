(defmodule ld-sup
  (behaviour supervisor)
  ;; API
  (export (start_link 0))
  ;; Supervisor callbacks
  (export (init 1)))

(defun SERVER () (MODULE))


;;;===================================================================
;;; API
;;;===================================================================

(defun start_link () (supervisor:start_link `#(local ,(SERVER)) (MODULE) '()))


;;;===================================================================
;;; Supervisor callbacks
;;;===================================================================

(defun init
  (['()]
   '#(ok #(#m(strategy  one_for_one
              intensity 3
              period    1)
             [#m(id       ld-server
                 start    #(ld-server start_link [])
                 restart  permanent
                 shutdown 2000
                 type     worker
                 modules  [ld-server])]))))
