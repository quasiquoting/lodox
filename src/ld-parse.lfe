(defmodule ld-parse
  (export all)
  ;; (export (docs 0) (docs 1) (docs 2)
  ;;         (to-org 1) (to-org 2))
  )

;;;===================================================================
;;; API
;;;===================================================================

(defun docs ()
  "TODO: write docstring"
  (docs "src"))

;; TODO: write a better docstring
(defun docs
  "Given a path to an LFE file or a directory containing LFE files,
return a map... TODO: rewrite docstring."
  ([file-or-dir]
   (case (filelib:is_dir file-or-dir)
     ('true
      (let* ((dir          (filename:absname file-or-dir))
             (project-name (filename:basename (filename:dirname dir))))
        (map 'name        project-name
             'version     "VERSION"
             'description "DESCRIPTION"
             'documents   '()
             'modules
             (lists:foldl (lambda (file acc)
                            (case (docs file dir)
                              ('()     acc)
                              (exports (cons (map 'name    (mod-name file)
                                                  'doc     "MODULE DOC"
                                                  'exports exports)
                                             acc))))
                          '() (filelib:wildcard "*.lfe" dir)))))
     ('false
      (case (filelib:is_file file-or-dir)
        ('true
         (let* ((`#(ok ,forms)          (lfe_io:read_file file-or-dir))
                (`#(ok ,mod-form)       (ld-seq:find-first forms #'defmodule?/1))
                (`#(ok (,_ . ,exports)) (ld-seq:find-first mod-form #'export?/1))
                (all? (=:= '(all) exports))
                (f    (lambda (form acc)
                        (case (doc form)
                          (`#(ok ,(= doc `#m(name ,f arity ,a)))
                           (case (orelse all? (lists:member `(,(list_to_atom f) ,a) exports))
                             ('true  (cons doc acc))
                             ('false acc)))
                          (_ acc)))))
           (lists:foldl f '() forms)))
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
     `#(ok #m(name     ,(atom_to_list name)
              arity    ,(length arglist-or-doc)
              arglists (,arglist-or-doc)
              doc      ,doc-or-form)))
    ((andalso (=/= arglist-or-doc '()) (io_lib:printable_list arglist-or-doc))
     `#(ok #m(name     ,(atom_to_list name)
              arity    ,(length (car doc-or-form))
              arglists ,(lists:map #'pattern/1 `(,doc-or-form ,body-or-clause))
              doc      ,arglist-or-doc)))
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
     `#(ok #m(name     ,(atom_to_list name)
              arity    ,(length (caar forms))
              arglists ,(lists:map #'pattern/1 forms)
              doc      ,doc-or-arglist)))
    ((andalso (arglist? doc-or-arglist)
              (io_lib:printable_list (car forms)))
     `#(ok #m(name     ,(atom_to_list name)
              arity    ,(length doc-or-arglist)
              arglists (,doc-or-arglist)
              doc      ,(car forms))))
    ('true 'not-found)))
  ([_] 'not-found))

(defun mod-name (file) (filename:basename file ".lfe"))

(defun pattern
  ([`(,patt ,(= guard `(when . ,_)) . ,_)] `(,patt ,guard))
  ([`(,arglist . ,_)] arglist))

(defun to-org (dict)
  "TODO: write docstring

Project level."
  (lists:foreach
   (match-lambda
     ([`#(,mod-name ,mod-dict)]
      (to-org mod-dict (filename:join "doc" (++ (atom_to_list mod-name) ".org")))))
   (orddict:to_list dict)))

(defun to-org (dict filename)
  "TODO: write docstring

Module level."
  (let ((f (match-lambda
             ([sig
               `#m(name     ,name
                   arity    ,arity
                   arglists ,arglists
                   doc      ,doc)
               output]
              (let ((parts `(,(++ "** " (atom_to_list sig))
                             "#+BEGIN_SRC lfe"
                             ,(string:join
                               (lists:map (lambda (arglist)
                                            (re:replace
                                             (lfe_io_pretty:term arglist)
                                             "comma " ". ,"
                                             '(global #(return list))))
                                          arglists)
                               "\n")
                             "#+END_SRC"
                             "#+BEGIN_EXAMPLE"
                             ,doc
                             "#+END_EXAMPLE\n")))
                (case output
                  ("" (string:join parts "\n"))
                  (_  (string:join `(,output ,(string:join parts "\n")) "\n")))))
             ([_ _ _] 'bad-dict))))
    (file:write_file filename (++ "* " (filename:basename filename ".org") "\n"
                                  (orddict:fold f "" dict)))))

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
