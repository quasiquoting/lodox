(defmodule ld-parse
  (export (docs 1) (docs 2)))

;;;===================================================================
;;; API
;;;===================================================================

(defun docs
  "Given a path to an LFE file or a directory containing LFE files,
return a proplist with module names as keys and for values,
proplists with keys of the form, `(fname arity)`, and their docstrings as values."
  ([file-or-dir]
   (case (filelib:is_dir file-or-dir)
     ('true
      (let ((dir (filename:absname file-or-dir)))
        (lists:flatmap (lambda (file) (docs file dir))
                       (filelib:wildcard "*.lfe" dir))))
     ('false
      (case (filelib:is_file file-or-dir)
        ('true
         (let* ((`#(ok ,forms)          (lfe_io:read_file file-or-dir))
                (`#(ok ,mod-form)       (find-first forms #'defmodule?/1))
                (`#(ok (,_ . ,exports)) (find-first mod-form #'export?/1))
                (all? (=:= '(all) exports))
                (f    (lambda (form docs)
                        (case (doc form)
                          (`#(ok ,(= fa `[,f ,a]) ,doc)
                           (case (orelse all? (lists:member fa exports))
                             ('true `(#(,fa ,doc) . ,docs))
                             (_ docs)))
                          (_ docs)))))
           (case (lists:foldr f '() forms)
             ('()     '())
             (results `(#(,(mod-name file-or-dir) ,results))))))
        ('false
         '#(error no-file-or-directory)))))))

(defun docs (file dir)
  "Given a filename, `file`, and a directory, `dir`, call #'docs/1 on `(filename:join dir file)`."
  (docs (filename:join dir file)))


;;;===================================================================
;;; Internal functions
;;;===================================================================

(defun doc
  "TODO: write docstring"
  ([`(defun ,name ,arglist-or-doc ,doc-or-form ,body-or-clause)]
   (when (is_atom name)
         (is_list arglist-or-doc)
         (is_list doc-or-form)
         (is_list body-or-clause))
   (cond
    ((andalso (io_lib:printable_list doc-or-form) (arglist? arglist-or-doc))
     `#(ok (,name ,(length arglist-or-doc)) ,doc-or-form))
    ((io_lib:printable_list arglist-or-doc)
     `#(ok (,name ,(length (car doc-or-form))) ,arglist-or-doc))
    ('true 'not-found)))
  ([`(defun ,name ,doc-or-arglist . ,forms)]
   (when (is_atom name)
         (is_list doc-or-arglist)
         (is_list forms))
   (cond
    ((andalso (io_lib:printable_list doc-or-arglist)
              (lists:all (match-lambda
                           ([`(,maybe-arglist . ,_t)] (arglist? maybe-arglist))
                           ([_]                       'false))
                         forms))
     `#(ok (,name ,(length (caar forms))) ,doc-or-arglist))
    ((andalso (arglist? doc-or-arglist)
              (io_lib:printable_list (car forms)))
     `#(ok (,name ,(length doc-or-arglist)) ,(car forms)))
    ('true 'not-found)))
  ([_] 'not-found))

(defun mod-name (file) (list_to_atom (filename:basename file ".lfe")))

(defun arglist?
  "Given a term, return true if it seems like a valid arglist, otherwise false."
  (['()]                      'true)
  ([lst] (when (is_list lst)) (lists:all #'arg?/1 lst))
  ([_]                        'false))

(defun arg?
  "Given a term, return true if it seems like a valid member of an arglist,
otherwise false."
  ([x] (when (is_atom x)) 'true)
  ([x] (when (is_list x))
   (lists:member (car x) '(= () backquote quote binary list tuple)))
  ([x] (when (is_map x)) 'true)
  ([x] (when (is_tuple x)) 'true)
  ([_] 'false))

(defun defmodule?
  ([`(defmodule . ,_)] 'true)
  ([_]                 'false))

(defun export?
  ([`(export . ,_)] 'true)
  ([_]              'false))

(defun find-first
  "Ported from http://joearms.github.io/2015/01/08/Some_Performance-Measurements-On-Maps.html"
  ([`(,h . ,t) pred]
   (case (funcall pred h)
     ('true  `#(ok ,h))
     ('false (find-first t pred))))
  (['() _] 'not-found))
