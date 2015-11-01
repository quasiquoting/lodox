(defmodule ld-app
  (behaviour application)
  (export (start 2) (stop 1)))

(defun start (_type _args)
  (case (ld-sup:start_link)
    (`#(ok ,pid) `#(ok ,pid))
    (other       `#(error ,other))))

(defun stop (state) 'ok)
