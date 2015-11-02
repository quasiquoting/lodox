(defmodule ld-util
  (export (get-version 0) (get-versions 0)))

(defun get-version ()
  "Return the current version of Lodox."
  (lutil:get-app-version 'lodox))

(defun get-versions ()
  "Return a proplist with keys, `erlang`, `emulator`, `driver-version`, `lfe`,
`lutil` and `lodox`, mapped to the respective versions.

See also: #'get-version/0"
  (++ (lutil:get-versions) `(#(lodox ,(get-version)))))
