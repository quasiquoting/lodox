(defmodule ld-org-writer
  (doc "Documentation writer that outputs Org.")
  (export (to-org 1) (to-org 2)))

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
