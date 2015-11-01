(defmodule ld-util
  (export (get-version 0) (get-versions 0)))

(defun get-version () (lutil:get-app-version 'lodox))

(defun get-versions () (++ (lutil:get-versions) `(#(lodox ,(get-version)))))
