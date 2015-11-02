(defmodule ld-parse
  (export (docs 1)))

;;;===================================================================
;;; API
;;;===================================================================

(defun docs (file)
  "Given a path to an LFE file, return a proplist with keys of the form,
`(fname arity)`, and their docstrings as values."
  (let* ((`#(ok ,forms)          (lfe_io:read_file file))
         (`#(ok ,mod-form)       (find-first forms #'defmodule?/1))
         (`#(ok (,_ . ,exports)) (find-first mod-form #'export?/1))
         (all?                   (=:= '(all) exports)))
    (lists:foldr (lambda (form docs)
                   (case (doc form)
                     (`#(ok ,(= fa `[,f ,a]) ,doc)
                      (case (orelse all? (lists:member fa exports))
                        ('true `(#(,fa ,doc) . ,docs))
                        (_ docs)))
                     (_ docs)))
                 '() forms)))


;;;===================================================================
;;; Internal functions
;;;===================================================================

(defun doc
  ([`(defun ,name ,arglist-or-doc ,doc-or-form ,body-or-forms)]
   (when (is_atom name)
         (is_list arglist-or-doc)
         (is_list doc-or-form)
         (is_list body-or-forms))
   (cond
    ((andalso (io_lib:printable_list doc-or-form) (arglist? arglist-or-doc))
     `#(ok (,name ,(length arglist-or-doc)) ,doc-or-form))
    ((io_lib:printable_list arglist-or-doc)
     `#(ok (,name ,(length (car doc-or-form))) ,arglist-or-doc))
    ('true 'not-found)))
  ([`(defun ,name ,doc-or-arglist . ,forms)] (when (is_atom name)
                                                   (is_list doc-or-arglist)
                                                   (is_list forms))
   (cond
    ((andalso (io_lib:printable_list doc-or-arglist)
              (lists:all (match-lambda
                           ([`(,maybe-arglist . ,_t)] (arglist? maybe-arglist))
                           ([_] 'false))
                         forms))
     `#(ok (,name ,(length (caar forms))) ,doc-or-arglist))
    ((andalso (arglist? doc-or-arglist)
              (io_lib:printable_list (car forms)))
     `#(ok (,name ,(length doc-or-arglist)) ,(car forms)))
    ('true 'not-found)))
  ([_] 'not-found))

(defun mod-name (file) (list_to_atom (filename:basename file ".lfe")))

(defun arglist?
  (['()] 'true)
  ([lst] (when (is_list lst)) (lists:all #'is_atom/1 lst))
  ([_]   'false))

(defun defmodule?
  ([`(defmodule . ,_)] 'true)
  ([_]                 'false))

(defun export?
  ([`(export . ,_)] 'true)
  ([_]              'false))

(defun find-first
  "Ported from
http://joearms.github.io/2015/01/08/Some_Performance-Measurements-On-Maps.html"
  ([`(,h . ,t) pred]
   (case (funcall pred h)
     ('true  `#(ok ,h))
     ('false (find-first t pred))))
  (['() _] 'not-found))
