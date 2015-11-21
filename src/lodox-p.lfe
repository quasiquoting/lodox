(defmodule lodox-p
  (export (clauses? 1) (clause? 1)
          (arglist? 1) (arg? 1)
          (string? 1)))

(defun clauses? (forms)
  "TODO: write docstring"
  (lists:all #'clause?/1 forms))

(defun clause?
  "TODO: write docstring"
  ([`(,h . ,_)] (lodox-p:arglist? h))
  ([_]          'false))

(defun arglist?
  "Given a term, return `true` if it is either the empty list or a list
containing only items that satisfy [`arg?/1`](#func-arg.3F), otherwise `false`."
  (['()]                      'true)
  ([lst] (when (is_list lst)) (lists:all #'arg?/1 lst))
  ([_]                        'false))

(defun arg?
  "Return `true` if `x` seems like a valid element of an arglist,
otherwise `false`."
  ([(= x `(,h . ,_t))]
   (orelse (string? x)
           (lists:member h '(= () backquote quote binary list tuple))
           (andalso (is_atom h) (lists:prefix "match-" (atom_to_list h)))))
  ([x]
   (orelse (is_atom x) (is_map x) (is_tuple x) (string? x))))

(defun string? (data)
  "Return `true` if `data` is a flat list of printable (possibly Unicode)
characters, otherwise `false`."
  (io_lib:printable_list data))
