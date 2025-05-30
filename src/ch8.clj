(ns ch8
  (:require [clojure.core.logic :as l]
            [ch7 :refer [pluso poso >1o]]))

(declare staro)

(defn bound-staro [q p n m]
  l/s#)

(defn odd-staro [x n m p]
  (l/fresh [q]
    (bound-staro q p n m)
    (staro x m q)
    (pluso (l/llist 0 q) m p)))

(defn staro [n m p]
  (l/conde
    [(l/== '() n) (l/== '() p)]
    [(poso n) (l/== '() m) (l/== '() p)]
    [(l/== (l/llist 1 nil) n) (poso m) (l/== m p)]
    [(>1o n) (l/== (l/llist 1 nil) m) (l/== n p)]
    [(l/fresh [x z]
       (l/== (l/llist 0 x) n) (poso x)
       (l/== (l/llist 0 z) p) (poso z)
       (>1o m)
       (staro x m z))]
    [(l/fresh [x y]
       (l/== (l/llist 1 x) n) (poso x)
       (l/== (l/llist 0 y) m) (poso y)
       (staro m n p))]
    [(l/fresh [x y]
       (l/== (l/llist 1 x) n) (poso x)
       (l/== (l/llist 1 y) m) (poso y)
       (odd-staro x n m p))]))

(l/run 10 [x y r]
  (staro x y r))

(l/run* [p]
  (staro '(0 1) '(0 0 1) p))
;; p -> ( 0 0 0 1 )
;; y got multplied by x
;; 4 got multiplied by 2


(l/run 1 [x y r]
  (l/== (list x y r) '((1 1) (1 1) (1 0 0 1)))
  (staro x y r))
