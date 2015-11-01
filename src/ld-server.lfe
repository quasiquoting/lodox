(defmodule ld-server
  (behaviour gen_server)
  ;; API
  (export (start_link 0))
  ;; gen_server callbacks
  (export (init 1)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (terminate 2)
          (code_change 3)))

(defrecord state)

(defun SERVER () (MODULE))


;;;===================================================================
;;; API
;;;===================================================================


(defun start_link ()
  (gen_server:start_link `#(local ,(SERVER)) (MODULE) '[] '[]))


;;;===================================================================
;;; gen_server callbacks
;;;===================================================================

(defun init (args) `#(ok ,(make-state)))

(defun handle_call (_request _from state) `#(reply ok ,state))

(defun handle_cast (message state) `#(noreply ,state))

(defun handle_info (info state) `#(noreply ,state))

(defun terminate (_reason _state) 'ok)

(defun code_change (old-version state extra) `#(ok ,state))
