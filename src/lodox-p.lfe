(defmodule lodox-p
  (export (arglist? 1) (arg? 1) (string? 1)))

(defun arglist?
  "Given a term, return true if it seems like a valid arglist, otherwise false."
  (['()]                      'true)
  ([lst] (when (is_list lst)) (lists:all #'arg?/1 lst))
  ([_]                        'false))

(defun arg?
  "Given a term, return true if it seems like a valid member of an arglist,
otherwise false.

See also: [[arglist?/1]]"
  ([x] (when (is_atom x)) 'true)
  ([x] (when (is_list x))
   (lists:member (car x) '(= () backquote quote binary list tuple)))
  ([x] (when (is_map x)) 'true)
  ([x] (when (is_tuple x)) 'true)
  ([_] 'false))

(defun string? (data)
  "Return true if data is an ISO-latin-1 string, otherwise false."
  (io_lib:printable_list data))
