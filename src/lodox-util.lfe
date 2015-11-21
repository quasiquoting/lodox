(defmodule lodox-util
  (doc "Utility functions to inspect the current version of lodox and its dependencies.")
  (export (get-version 0) (get-versions 0)
          (search-funcs 2) (search-funcs 3)
          (when* 2)))

(defun get-version ()
  "Return the current version of Lodox."
  (lutil:get-app-version 'lodox))

(defun get-versions ()
  "Return a proplist with keys, `erlang`, `emulator`, `driver-version`, `lfe`,
  `lutil` and `lodox`, mapped to the respective versions.

  See also: [`get-version/0`](#func-get-version.3F.2F0)"
  (++ (lutil:get-versions) `(#(lodox ,(get-version)))))

(defun when* (test then)
  "Given a test that returns a boolean, if test is true, return then,
  otherwise false."
  (if test then))

(defun search-funcs (modules partial-func)
  (search-funcs modules partial-func 'undefined))

(defun search-funcs
  "TODO: write docstring"
  ([modules partial-func starting-mod]
   (let* ((suffix  (if (lists:member #\/ partial-func)
                     partial-func
                     (++ "/" partial-func)))
          (matches (lists:filter
                     (lambda (func-name) (lists:suffix suffix func-name))
                     (exported-funcs modules))))
     (case (lists:dropwhile
            (lambda (func-name)
              (=/= (atom_to_list starting-mod) (module func-name)))
            matches)
       (`(,func . ,_) func)
       ('()           (case matches
                        (`(,func . ,_) func)
                        ('()           'undefined)))))))

(defun exported-funcs (modules)
  "TODO: write docstring"
  (lc ((<- mod modules)
       (<- func (mref mod 'exports)))
    (func-name mod func)))

(defun func-name (mod func)
  "TODO: write docstring"
  (++ (atom_to_list (mref mod 'name))
      ":" (atom_to_list (mref func 'name))
      "/" (integer_to_list (mref func 'arity))))

(defun module (func-name)
  (lists:takewhile (lambda (c) (=/= c #\:)) func-name))
