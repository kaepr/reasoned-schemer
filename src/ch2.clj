(ns ch2
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

;; Chapter 2
;; Teaching old toys new tricks

;; scheme's caro -> firsto
;; https://github.com/clojure/core.logic/wiki/Differences-from-The-Reasoned-Schemer

(run* [q]
  (firsto '(a c o r n) q))

(run* [q]
  (fresh [x y]
    (firsto `(~q ~y) x)
    (== 'pear x)))
;; (pear)

;; (defn caro [p a]
;;   (fresh [d]
;;     (== (lcons a d) p)))

; Unlike car, it expects two arguments

(cons
 (first '(grape raisin pear))
 (first '((a) (b) (c))))
; -> (grape a)

(run* [r]
  (fresh [x y]
    (firsto '(grape raisin pear) x)
    (firsto '((a) (b) (c)) y)
    (== (lcons x y) r)))
;; -> r = ((grape a))
;;

(run* [r]
  (fresh [x y]
    (resto '(grape raisin pear) x)
    (firsto '((a) (b) (c)) y)
    (== (lcons x y) r)))
;; r -> ((raisin pear) a)

(run* [q]
  (resto '(a c o r n) '(c o r n)))

(run* [x]
  (resto '(c o r n) `(~x r n)))

;; above fails due to difference in ` and '
;; ` evaluates things to namespace symbols
;;
;; either keep both as ` to avoid confusion
;;
;; or use something like list to avoid that problem

(run* [x]
  (resto '(c o r n) (list x 'r 'n))) ;; works

(comment

  ;; notice the difference of output

  (let [x 123]
      (list x 'r 'n)) ;; not namespaced symbols

  (let [x 123]
    `(~x r n)) ;; namespace symbols

  ())

(run* [x]
  (resto `(c o r n) `(~x r n)))

(run* [x]
  (resto '(c o r n) (list x 'r 'n)))

(run* [l]
  (fresh [x]
    (resto l '(c o r n)) ;; rest of `l` is (c o r n)
    (firsto l x) ;; first of `l` is `x`
    (== 'a x))) ;; `x` is `a`
;; -> l is associated with value (a c o r n)

(comment

  (cons '(a b c) '(2))

  ())

(run* [l]
  (conso '(a b c) '(d e) l))
;; l = ((a b c) d e)

(run* [x]
  (conso x '(a b c) '(d a b c)))
;; x = d

(run* [r]
  (fresh [x y z]
    (== (list 'e 'a 'd x) r); (e a d x) -> r
    (conso y (list 'a z 'c) r))) ; (y a z c) -> r
;; r -> (e a d c)

(run* [x]
  (conso x `(a ~x c) `(d a ~x c)))
;; x == d

(run* [l]
  (fresh [x]
    (== `(d a ~x c) l)
    (conso x `(a ~x c) l)))
;; l -> (d a d c)

(run* [l]
  (fresh [x]
    (conso x `(a ~x c) l)
    (== `(d a ~x c) l)))
;; (d a d c)

(comment

  (defn consoo [a d p]
    (firsto p a)
    (resto p d))


  (defn conso
      "A relation where l is a collection, such that a is the first of l
    and d is the rest of l. If ground d must be bound to a proper tail."
      [a d l]
      (== (lcons a d) l))
    

  (run* [l]
     (fresh [x]
         (consoo x `(a ~x c) l)
         (== `(d a ~x c) l)))

 ())

(run* [l]
  (fresh [d t x y w]
    (conso w '(n u s) t)
    (resto l t)
    (firsto l x)
    (== 'b x)
    (resto l d)
    (firsto d y)
    (== 'o y)))
;; bonus


;; null? -> empty?

(empty? '(grape raisin pear))

(empty? '())

;; nullo -> emptyo
;; nilo unifies with nil

(run* [q]
  (emptyo '(a b c)))

(run* [q]
  (emptyo '()))

(run* [x]
  (emptyo x))

(comment

  (defn nulloo [x]
    (== '() x))

  ())

(defn pair? [x]
  (or (lcons? x) (and (coll? x) (seq x))))

(pair? (llist 'split 'pea))

(pair? (llist '(split) 'pea))

(lcons '() nil)

(pair? (lcons '() nil))

(pair? ())

(pair? 'pair)

(pair? 'pear)

(pair? '(pear))

(lcons '(split) 'pea)

(llist '(split) 'pea)

(run* [r]
  (fresh [x y]
    (== (lcons x (lcons y 'salad)) r)))

(run* [r]
  (fresh [x y]
    (== (llist x y 'salad) r)))

(lcons 'x (lcons 'y 'salad))

;; (_0 . (_1 . 'salad))

(defn pairo [p]
  (fresh [a d]
    (conso a d p)))

(run* [q]
  (pairo (llist q q)))
;; it just creates a pair
;; does not add any constraint
;; on what q can be


(pairo '())

(run* [q]
  (pairo '())) ;; no values can satisy this pair creation

(run* [q]
  (pairo 'pair)) ;; same

(run* [x]
  (pairo x)) ;; x can be anything of the form (_0 . _1)

(run* [r]
  (pairo (lcons r '())))

(defn singleton? [l]
  (cond
    (pair? l) (empty? (rest l))
    :else false))

(lcons 'tofu nil)
(lcons '(tofu) '())

(singleton? (lcons 'tofu nil)) ;; yes, because it's (tofu)

(singleton? (lcons '(tofu) nil)) ;; yes because its ((tofu))

(singleton? 'tofu) ;; no , not a list

(llist 'e 'tofu nil)

(singleton? (llist 'tofu 'cheese nil))

(singleton? '())

;; singleton? determines if the argument is
;; proper list of length one
;;
;;
;; list is proper if it is the empty list
;; or it is a pair, whose cdr is proper

(comment

  (defn singletono [l]
    (conde
     [(pairo l) (fresh [d]
                  (resto l d)
                  (emptyo d))]
     [s# u#]))


  (defn singletono [l]
    (pairo l)
    (fresh [d]
      (resto l d)
      (emptyo d)))

  ())

(defn singletono [l]
  (pairo l)
  (fresh [d]
    (resto l d)
    (emptyo d)))

(defn singletono [l]
  (fresh [d]
    (resto l d)
    (emptyo d)))

(run* [q]
  (singletono q))

;; Any conde that has a u# as a top level goal
;; cannot contribute values
;;



(defn firstoo [l a]
  (fresh [d]
    (conso a d l)))

(defn restoo [l d]
  (fresh [a]
    (conso a d l)))

;; the same thing
;; they are the same relation

(run* [q]
  (fresh [d]
    (restoo q d)
    (emptyo d))
  (fresh [a]
    (firstoo q a)
    (== a 2)))
