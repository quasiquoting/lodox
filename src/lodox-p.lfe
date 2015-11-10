(defmodule lodox-p
  (export (arglist? 1) (arg? 1) (string? 1)))

(defun arglist?
  "Given a term, return `true` if it is either the empty list or a list s.t.
`∀ x ∈ lst (arg? x)`, otherwise `false`.

See also: [`arglist?/1`](#func-arg.3F.2F1)"
  (['()]                      'true)
  ([lst] (when (is_list lst)) (lists:all #'arg?/1 lst))
  ([_]                        'false))

(defun arg?
  "Return `true` if `x` seems like a valid element of an arglist,
otherwise `false`."
  ([`(,x . ,_t)] (lists:member x '(= () backquote quote binary list tuple)))
  ([x]           (orelse (is_atom x) (is_map x) (is_tuple x))))

(defun string? (data)
  "Return `true` if `data` is a flat list of printable (possibly Unicode)
characters, otherwise `false`."
  (andalso (is_list data)
           (orelse (io_lib:printable_list data)
                   (io_lib:printable_unicode_list data)
                   (andalso (lists:all #'is_integer/1 data)
                            (io_lib:printable_unicode_list
                             (unicode:characters_to_list (binary:list_to_bin data)))))))
