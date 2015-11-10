(defmodule lodox-util
  (doc "Utility functions to inspect the current version of lodox and its dependencies.")
  (export (get-version 0) (get-versions 0) (when* 2)))

(defun get-version ()
  "Return the current version of Lodox."
  (lutil:get-app-version 'lodox))

(defun get-versions ()
  "Return a proplist with keys, `erlang`, `emulator`, `driver-version`, `lfe`,
`lutil` and `lodox`, mapped to the respective versions.

See also: #'get-version/0"
  (++ (lutil:get-versions) `(#(lodox ,(get-version)))))

(defun when* (test then)
  "Given a test that returns a boolean, if test is true, return then,
otherwise false."
  (if test then))
