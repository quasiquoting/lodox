(defmodule lodox-parse
  (doc "Parsing LFE source files for metadata.")
  (export (docs 1)))

(include-lib "lodox/include/lodox-macros.lfe")

;;;===================================================================
;;; API
;;;===================================================================

;; TODO: write a better docstring
(defun docs (app-name)
  "Given an app-name (binary), return a map like:


    '#m(name        #\"lodox\"
        version     \"0.4.0\"
        description \"The LFE rebar3 Lodox plugin\"
        documents   ()
        modules     {{list of maps of module metadata}})"
  (let* ((app         (doto (binary_to_atom app-name 'latin1)
                            (application:load)))
         (app-info    (let ((`#(ok ,info) (application:get_all_key app)))
                        (maps:from_list info)))
         (modules     (mod-docs (mref app-info 'modules)))
         (version     (maps:get 'vsn         app-info ""))
         (description (maps:get 'description app-info "")))
    `#m(name        ,app-name
        version     ,version
        description ,description
        documents   '()
        modules     ,modules)))


;;;===================================================================
;;; Internal functions
;;;===================================================================

(defun form-doc
  "TODO: write docstring"
  ([`(defun ,name ,arglist-or-doc ,doc-or-form ,body-or-clause)]
   (when (is_atom name)
         (is_list arglist-or-doc)
         (is_list doc-or-form)
         (is_list body-or-clause))
   (cond
    ((and (lodox-p:string? doc-or-form) (lodox-p:arglist? arglist-or-doc))
     `#(ok #m(name     ,(atom_to_list name)
              arity    ,(length arglist-or-doc)
              arglists (,arglist-or-doc)
              doc      ,doc-or-form)))
    ((and (=/= arglist-or-doc '()) (lodox-p:string? arglist-or-doc))
     `#(ok #m(name     ,(atom_to_list name)
              arity    ,(length (car doc-or-form))
              arglists ,(lists:map #'pattern/1 `(,doc-or-form ,body-or-clause))
              doc      ,arglist-or-doc)))
    ('true 'not-found)))
  ([`(defun ,name () ,constant)]
   'not-found)
  ([`(defun ,name ,doc-or-arglist . ,forms)]
   (when (is_atom name)
         (is_list doc-or-arglist)
         (is_list forms))
   (cond
    ((andalso (lodox-p:string? doc-or-arglist)
              (lists:all (match-lambda
                           ([`(,maybe-arglist . ,_t)]
                            (lodox-p:arglist? maybe-arglist))
                           ([_] 'false))
                         forms))
     `#(ok #m(name     ,(atom_to_list name)
              arity    ,(length (caar forms))
              arglists ,(lists:map #'pattern/1 forms)
              doc      ,doc-or-arglist)))
    ((andalso (lodox-p:arglist? doc-or-arglist)
              (lodox-p:string? (car forms)))
     `#(ok #m(name     ,(atom_to_list name)
              arity    ,(length doc-or-arglist)
              arglists (,doc-or-arglist)
              doc      ,(car forms))))
    ('true 'not-found)))
  ([_] 'not-found))

(defun form-doc (form line exports)
  (case (form-doc form)
    (`#(ok ,(= doc `#m(name ,f arity ,a)))
     (lodox-util:when* (lists:member `#(,(list_to_atom f) ,a) exports)
       `#(true ,(mset doc 'line line))))
    ('not-found 'false)))

(defun mod-behaviour (module)
  (let ((attributes (call module 'module_info 'attributes)))
    (proplists:get_value 'behaviour attributes '())))

(defun mod-docs
  ([mods] (when (is_list mods))
   (lists:filtermap #'mod-docs/1 mods))
  ([mod]  (when (is_atom mod))
   (let ((file (proplists:get_value 'source (call mod 'module_info 'compile))))
     (case (filename:extension file)
       (".lfe" (case (mod-docs file (call mod 'module_info 'exports))
                 ('()     'false)
                 (exports `#(true #m(name      ,(mod-name mod)
                                     behaviour ,(mod-behaviour mod)
                                     doc       ,(mod-doc mod)
                                     exports   ,exports
                                     ;; dirty hack
                                     filepath  ,file)))))
       (_      'false)))))

(defun mod-docs (file exports)
  (if (filelib:is_file file)
    (let ((`#(ok ,forms) (lfe_io:parse_file file)))
      (lists:filtermap
        (match-lambda ([`#(,form ,line)] (form-doc form line exports)))
        forms))
    '()))

(defun mod-doc
  ([module] (when (is_atom module))
   (let ((attributes (call module 'module_info 'attributes)))
     (proplists:get_value 'doc attributes ""))))

(defun mod-name (mod) (call mod 'module_info 'module))

(defun pattern
  ([`(,patt ,(= guard `(when . ,_)) . ,_)] `(,patt ,guard))
  ([`(,arglist . ,_)] arglist))
