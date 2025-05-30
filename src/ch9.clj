(ns ch9
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :as l :refer :all]))

;; first goal of conda line is question
;; rest is answer
;;
;; law of conda
;; first line whose question succeeds
;; only that line contributes

(run 1 [q]
  (conda
   [u# s#]
   [s# u#]))

(run 1 [q]
  (conda
   [u# s#]
   [s# s#]))

(run 1 [q]
  (conda
   [s# u#]
   [s# s#]))

(run* [x]
  (conda
   [(== 'olive x) s#]
   [s# (== 'oil x)]))
;; only (olive)

(run* [x]
  (conda
   [(== 'virgin x) u#]
   [(== 'olive x) s#]
   [s# (== 'oil x)]))

(run* [q]
  (fresh [x y]
    (== 'split x)
    (== 'pea y)
    (conda
     [(== 'split x) (== x y)] ;; true
     [s# s#])))

(run* [q]
  (fresh [x y]
    (== 'split x)
    (== 'pea y)
    (conda
     [(== x y) (== 'split x)] ; fails
     (s# s#)))) ; true

(defn not-pastao [x]
  (conda
   [(== 'pasta x) u#]
   [(== 'spaghetti x) s#]))
