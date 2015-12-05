(defmodule lodox
  (doc "The Lodox Rebar3 provider.

[http://www.rebar3.org/docs/plugins](http://www.rebar3.org/docs/plugins)")
  (behaviour provider)
  (export all))

(defun namespace ()     'default)

(defun provider-name () 'lodox)

(defun short-desc ()    "Generate documentation from LFE source files.")

(defun deps () '(#(default app_discovery)))

(defun desc () (short-desc))


;;;===================================================================
;;; API
;;;===================================================================

(defun init (state)
  "Initiate the Lodox provider."
  (rebar_api:debug "Initializing {default, lodox}" '())
  (let* ((opts `(#(name       ,(provider-name))   ; The 'user friendly' name
                 #(module     ,(MODULE))          ; The module implementation
                 #(namespace  ,(namespace))       ; Plugin namespace
                 #(opts       ())                 ; List of plugin options
                 #(deps       ,(deps))            ; The list of dependencies
                 #(example    "rebar3 lodox doc") ; How to use the plugin
                 #(short_desc ,(short-desc))      ; A one-line description
                 #(desc       ,(desc))            ; A longer description
                 #(bare       true)))             ; Task can be run by user
         (provider (providers:create opts)))
    (let ((state* (rebar_state:add_provider state provider)))
      (rebar_api:debug "Initialized {default, lodox}" '())
      `#(ok ,state*))))

(defun do (state)
  "Generate documentation for each application in the proejct."
  (rebar_api:debug "Starting do/1 for {default, lodox}" '())
  (let ((apps (case (rebar_state:current_app state)
                ('undefined (rebar_state:project_apps state))
                (apps-info   `(,apps-info)))))
    (lists:foreach #'write-docs/1 apps))
  `#(ok ,state))

(defun format_error (reason)
  "When an exception is raised or a value returned as
`#(error #((MODULE) reason)` will see the `(format_error reason)`
function called for them, so a string can be formatted explaining
the issue."
  (io_lib:format "~p" `(,reason)))


;;;===================================================================
;;; Internal functions
;;;===================================================================

(defun write-docs (app-info)
  (let* ((`(,opts ,app-dir ,name ,vsn ,out-dir)
          (lists:map (lambda (f) (call 'rebar_app_info f app-info))
                     '(opts dir name original_vsn out_dir)))
         (ebin-dir (filename:join out-dir "ebin"))
         (doc-dir  (filename:join app-dir "doc")))
    (rebar_api:debug "Adding ~p to the code path" `(,ebin-dir))
    (code:add_path ebin-dir)
    (let ((project (lodox-parse:docs name))
          (opts    `#m(output-path ,doc-dir app-dir ,app-dir)))
      (rebar_api:debug "Generating docs for ~p" `(,(mref project 'name)))
      (lodox-html-writer:write-docs project opts))
    (generated name vsn doc-dir)))

(defun generated
  ([name `#(cmd ,cmd) doc-dir]
   (generated name (os:cmd (++ cmd " | tr -d \"\\n\"")) doc-dir))
  ([name vsn doc-dir]
   (rebar_api:console "Generated ~s v~s docs in ~s" `(,name ,vsn ,doc-dir))))
