(defmodule unit-lodox-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from ltest
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest project-shape
  (let ((project (ld-parse:docs)))
    (is (is_map project))
    (is (non-empty-list? (mref* project 'description)))
    (is (is_list (mref* project 'documents)))
    (is (is_list (mref* project 'modules)))
    (is-equal "lodox" (mref* project 'name))
    (is (is_list (mref* project 'version)))))

(deftest modules-shapes
  (let ((modules (mref* (ld-parse:docs) 'modules)))
    (is (is_list modules))
    (lists:foreach #'module?/1 modules)))

(defun module? (module)
  (is (is_map module))
  (is-equal '(doc exports name) (maps:keys module))
  (is (non-empty-list? (mref* module 'doc)))
  (is (is_list (mref* module 'exports)))
  (is (non-empty-list? (mref* module 'name))))

(deftest exports-shapes
  (let ((exports (lists:map (lambda (module) (mref* module 'exports))
                            (mref* (ld-parse:docs) 'modules))))
    (is (is_list exports))))

(defun export? (export)
  (is (is_map export))
  (is-equal '(arglists arity doc name) (maps:keys export))
  (let ((arglists (mref* export 'arglists)))
    (is (andalso (is_list arglists) (lists:all #'is_list/1 arglists))))
  (is (is_integer (mref* export 'arity)))
  (is (non-empty-list? (mref* export 'doc)))
  (is (non-empty-list? (mref* export 'name))))

(defun mref* (m k) (maps:get k m 'error))

(defun non-empty-list?
  (['()]                      'false)
  ([lst] (when (is_list lst)) 'true)
  ([_]                        'false))
