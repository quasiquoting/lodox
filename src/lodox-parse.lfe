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

```lfe
'#m(name        #\"lodox\"
    version     \"0.4.2\"
    description \"The LFE rebar3 Lodox plugin\"
    documents   ()
    modules     {{list of maps of module metadata}})
```"
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
    ;; (defun name arglist maybe-doc body)
    ((andalso (lodox-p:arglist? arglist-or-doc)
              (or (=:= '() arglist-or-doc)
                  (not (lodox-p:string? arglist-or-doc))))
     `#(ok #m(name     ,name
              arity    ,(length arglist-or-doc)
              arglists (,arglist-or-doc)
              doc      ,(if (lodox-p:string? doc-or-form) doc-or-form ""))))
    ;; (defun name doc clause clause)
    ((and (=/= arglist-or-doc '()) (lodox-p:string? arglist-or-doc))
     `#(ok #m(name     ,name
              arity    ,(length (car doc-or-form))
              arglists ,(patterns `(,doc-or-form ,body-or-clause))
              doc      ,arglist-or-doc)))
    ;; (defun name clause clause clause)
    ((lodox-p:clauses? `(,arglist-or-doc ,doc-or-form ,body-or-clause))
     `#(ok #m(name     ,name
              arity    ,(length (car arglist-or-doc))
              arglists ,(patterns
                         `(,arglist-or-doc ,doc-or-form ,body-or-clause))
              doc      "")))))
  ([`(defun ,name () ,constant)]
   `#(ok #m(name     ,name
            arity    0
            arglists [()]
            doc      "")))
  ([`(defun ,name ,doc-or-arglist . ,(= forms `(,form . ,_)))]
   (when (is_atom name)
         (is_list doc-or-arglist)
         (is_list forms))
   (cond
    ;; (defun name arglist maybe-doc body)
    ((lodox-p:arglist? doc-or-arglist)
     `#(ok #m(name     ,name
              arity    ,(length doc-or-arglist)
              arglists (,doc-or-arglist)
              doc      ,(if (lodox-p:string? form) form ""))))
    ;; (defun name doc clauses)
    ((andalso (lodox-p:string? doc-or-arglist)
              (lodox-p:clauses? forms))
     `#(ok #m(name     ,name
              arity    ,(length (car form))
              arglists ,(patterns forms)
              doc      ,doc-or-arglist)))
    ;; (defun name clauses)
    ((lodox-p:clauses? `(,doc-or-arglist . ,forms))
     `#(ok #m(name     ,name
              arity    ,(length (car doc-or-arglist))
              arglists ,(patterns `(,doc-or-arglist . ,forms))
              doc      "")))))
  ([`(defun ,name ,clause)]
   (when (is_atom name)
         (is_list clause))
   (if (lodox-p:clause? clause)
     `#(ok #m(name     ,name
              arity    ,(length (car clause))
              arglists ,(pattern clause)
              doc      ""))
     'undefined))
  ;; This pattern matches non-defun forms.
  ([_] 'undefined))

(defun form-doc (form line exports)
  (case (form-doc form)
    (`#(ok ,(= doc `#m(name ,f arity ,a)))
     (lodox-util:when* (lists:member `#(,f ,a) exports)
       `#(true ,(mset doc 'line line))))
    ('undefined 'false)))

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

(defun patterns (forms) (lists:map #'pattern/1 forms))

(defun pattern
  ([`(,patt ,(= guard `(when . ,_)) . ,_)] `(,@patt ,guard))
  ([`(,arglist . ,_)] arglist))
