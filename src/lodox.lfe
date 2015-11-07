(defmodule lodox
  (behaviour provider)
  (export all))

(defun namespace () 'lfe) ; All LFE plugins need to have this

(defun provider-name () 'doc)

(defun short-desc () "The LFE rebar3 doc (Lodox) plugin")

(defun deps () '(#(default app_discovery)))


;;;===================================================================
;;; API
;;;===================================================================

(defun init (state)
  (let* ((opts `(#(name       ,(provider-name))      ; The 'user friendly' name
                 #(module     ,(MODULE))             ; The module implementation
                 #(namespace  ,(namespace))          ; Plugin namespace
                 #(opts       ())                    ; List of plugin options
                 #(deps       ,(deps))               ; The list of dependencies
                 #(example    "rebar3 lfe doc")      ; How to use the plugin
                 #(short_desc ,(short-desc))         ; A one-line description
                 #(desc       ,(info (short-desc)))  ; A longer description
                 #(bare       true)))                ; Task can be run by user
         (provider (providers:create opts)))
    `#(ok ,(rebar_state:add_provider state provider))))

(defun do (state)
  (ld-html-writer:write-docs)
  `#(ok ,state))

(defun format_error (reason)
  (io_lib:format "~p" `(,reason)))


;;;===================================================================
;;; API
;;;===================================================================

(defun info (desc)
  (io_lib:format
    (++ "~n~s~n~n"
        "Generate documentation from LFE source files."
        "~n")
    `(,desc)))
