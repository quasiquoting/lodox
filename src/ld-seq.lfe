(defmodule ld-seq
  (doc "Miscellaneous list utility functions, inspired by/stolen from Clojure.")
  (export (distinct 1)
          (find-first 2)
          (reductions 2) (reductions 3)
          (rest 1)))

(defun distinct
  "Return a list of the elements of lst with duplicates removed."
  (['()] '())
  ([lst] (distinct lst (sets:new) '())))

(defun distinct
  (['() _seen acc] (lists:reverse acc))
  ([`(,h . ,t) seen acc]
   (case (sets:is_element h seen)
     ('true  (distinct t seen acc))
     ('false (distinct t (sets:add_element h seen) (cons h acc))))))

(defun find-first
  "Ported from http://joearms.github.io/2015/01/08/Some_Performance-Measurements-On-Maps.html"
  ([`(,h . ,t) pred]
   (case (funcall pred h)
     ('true  `#(ok ,h))
     ('false (find-first t pred))))
  (['() _] 'not-found))

(defun reductions
  "Call [[reductions/3]] with h as init and t as the list."
  ([f `(,h . ,t)] (reductions f h t)))

(defun reductions
  "Return a list of the intermediate values of the reduction (as in
lists:foldl/3) of the given list by f, starting with init."
  ([f init '()]        `(,init))
  ([f init `(,h . ,t)] `(,init . ,(reductions f (funcall f init h) t))))

(defun rest
  "Return a possibly empty list of the elements after the first."
  (['()]        '())
  ([`(,h . ,t)] t))
