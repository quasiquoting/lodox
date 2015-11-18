(defmodule unit-lodox-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from ltest
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest projects-shapes
  (lists:zipwith #'validate-project/2 (src-dirs) (all-docs)))

(defun validate-project (dir project)
  (is (is_map project))
  (is (non-empty-list? (mref* project 'description)))
  (is (is_list (mref* project 'documents)))
  (is (is_list (mref* project 'modules)))
  (is-equal (project-name dir) (mref* project 'name))
  (is (is_list (mref* project 'version))))

(deftest modules-shapes
  (lists:foreach #'validate-module/1 (project-wide 'modules)))

(defun validate-module (module)
  (is (is_map module))
  (is-equal '(behaviour doc exports filepath name) (maps:keys module))
  (is (is_list (mref* module 'doc)))
  (is (is_list (mref* module 'exports)))
  (is (is_atom (mref* module 'name))))

(deftest exports-shapes
  (lists:foreach #'validate-exports/1 (project-wide 'exports 'modules)))

(defun validate-exports (export)
  (is (is_map export))
  (is-equal '(arglists arity doc line name) (maps:keys export))
  (let ((arglists (mref* export 'arglists)))
    (is (andalso (is_list arglists) (lists:all #'is_list/1 arglists))))
  (is (is_integer (mref* export 'arity)))
  (is (non-empty-list? (mref* export 'doc)))
  (is (is_atom (mref* export 'name))))

(defun all-docs () (lists:map #'lodox-parse:docs/1 '(#"lodox")))

(defun mref* (m k) (maps:get k m 'error))

(defun non-empty-list?
  (['()]                      'false)
  ([lst] (when (is_list lst)) 'true)
  ([_]                        'false))

(defun project-name
  (["src"] #"lodox")
  ([dir]   (filename:basename (filename:dirname dir))))

(defun project-wide
  ([f]   (when (is_function f)) (lists:flatmap f (all-docs)))
  ([key]                        (project-wide (lambda (proj) (mref* proj key)))))

(defun project-wide (key2 key1)
  (project-wide
   (lambda (proj) (lists:flatmap (lambda (m) (mref* m key2)) (mref* proj key1)))))

(defun src-dirs () '("src"))
