(ns core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

;; goal is something which succeds, fails or has no value

(run* [q]
  u#)

(== 'pea 'pod)

(run* [q]
  (== 'pea 'pod))

(run* [q]
  (== q 'pea))

(run* [q]
  (== 'pea q))

;;
;; Law 1 of ==
;;
;; ( == v w ) same as ( == w v )
;;
;;
;; Every variable is initially fresh.
;;
;; A variable is no longer fresh if it becomes associated
;; with a non variable value
;; or if it becomes associated with a variable
;; that itself is no longer fresh.
;;

(run* [q]
  (== 'pea q)) ;; q not fresh, as it becomes associated with 'pea


(run* [q]
  s#) ;; remains fresh, q not associated with any value
;; => (_0)

;; reify -> make something more concrete

;; so above, _0 is reified as fresh variable q

(run* [q]
  (== 'pea 'pea))
;; (_0) q is still fresh

(run* [q]
  (== q q))
;; (_0) q is still fresh, could be anything as no value is associated


(run* [q]
  (fresh [x]
    (== 'pea q)))

;; => (pea)

;; adding an unused variable does not change the
;; value associated with any other variable
;;
;; both variables start out fresh,
;; but only x remains fresh


(run* [q]
  (fresh [x]
    (== (lcons x '()) q)))

(run* [q]
  (fresh [x]
    (== `(~x) q)))

;; `(~x) same as (cons x)

;; two different variable can be made same
;; by fusing them
(run* [q]
  (fresh [x]
    (== x q)))

(run* [q]
  (== '(((pea)) pod) '(((pea)) pod)))

(run* [q]
  (== `(((pea)) pod) `(((pea)) ~q)))

(run* [q]
  (== `(((~q)) pod) `(((pea)) pod)))

(run* [q]
  (fresh [x]
    (== `(((~q)) pod) `(((~x)) pod))))

(run* [q]
  (fresh [x]
    (== `(~x ~x) q)))

;; every instance of same fresh variable
;; is replaced by the same reified variable

(run* [q]
  (fresh [x]
    (fresh [y]
      (== `(~q ~y) `((~x ~y) ~x)))))

;; two variables are different when they are not fused
;; every variable initially introduced is different from
;; every other variable

(run* [q]
  (fresh [x]
    (== 'pea q)))

;; x -> _0, q -> pea


(run* [q]
  (fresh [x]
    (fresh [y]
      (== `(~x ~y) q))))

;; each fresh variable is treated differently
;; thus 0 and 1

(run* [s]
  (fresh [t]
    (fresh [u]
      (== `(~t ~u) s))))

;; difference with the above is only in its variable
;; names

(run* [q]
  (fresh [x]
    (fresh [y]
      (== `(~x ~y ~x) q))))

(run* [q]
  (== `(pea) `pea))

(run* [q]
  (== `(~q) q)) ;; not same


;; (x) can never be same x
;; no matter what value is associated with x,
;; x cannot equal to a list in which x occurs
;;
;; A variable x occurs in variable y
;; when x ( or any variable fused with x )
;; appears in the value associated with y
;;
;;
;; A variable x occurs in a list l when x
;; ( or any variable fused with x ) is an
;; element of l, or when x occurs in an element of l
;;

;; Second Law of ==
;;
;; If x is fresh, then (== v x) succeeds
;; and associates v with x, unless x occurs in v.
;;

(run* [q]
  (conjo s# s#))

(run* [q]
  s#
  (== 'corn q)) ;; NOTE: no special conj operator was neede
;; not sure if that's good or not

(run* [q]
  u#
  (== 'corn q)) ;; no solution possible due to u#


(run* [q]
  (== 'corn q)
  (== 'meal q))
;; fails, as its not possible to be two values
;; at once
;;
;; after first success, it was no longer fresh
;; it cannot be associated again

(run* [q]
  (== 'corn q)
  (== 'corn q)) ;; corn

(run* [q]
  (conde [u#] [u#])) ;; representing disjunction using conde

(run* [q]
  (conde
   [(== 'olive q)]
   [u#])) ;; olive

(run* [q]
  (conde
   [(== 'olive q)]
   [(== 'oil q)])) ;; (olive oil) as it can be two values

(run* [q]
  (fresh [x]
    (fresh [y]
      (conde
       [(== `(~x ~y) q)]
       [(== `(~y ~x) q)]))))
;;
;; two possible values
;; from first block -> (0 1)
;; from second block -> (0 1)
;;
;; the variables x and y are not fused in the previous expression
;;
;; each value produced by a run* expression is reified
;; independently of any other values
;;
;; that's why numbering of reified variables begin from 0
;; within each reified value
;;

(run* [x]
  (conde
   [(== 'olive x)]
   [(== 'oil x)]))

(run* [x]
  (conde
   [(== 'oil x)]
   [(== 'olive x)]))

;; both above expressions are same
;; order of values does not matter


(run* [x]
  (conde
   [(== 'olive x) u#]
   [(== 'oil x)])) ;; oil

(run* [x]
  (conde
   [(== 'virgin x) u#]
   [(conde
     [(== 'olive x)]
     [(conde
       [s#]
       [(== 'oil x)])])]))
;; (oil _0 olive)
;; the middle _0 comes because of s#
;; which makes the goal success for any value of x

(run* [r]
  (fresh [x]
    (fresh [y]
      (== 'split x)
      (== 'pea y)
      (== `(~x ~y) r)))) ;; ((split pea))

(run* [r]
  (fresh [x y]
    (== 'split x)
    (== 'pea y)
    (== `(~x ~y) r)))
; r -> (split pea)

(run* [r x y]
  (== 'split x)
  (== 'pea y)
  (== `(~x ~y) r))

; the answer is always a list of possible answers
; ( [(split pea) split pea] )

(run* [x y]
  (== 'split x)
  (== 'pea y))

(run* [x y z]
  (== 'soup z)
  (conde
   [(== 'split x) (== 'pea y)]
   [(== 'red x) (== 'bean y)]))

(defn teacupo [t]
  (conde
   [(== t 'tea)]
   [(== t 'cup)]))

;; https://github.com/philoskim/reasoned-schemer-for-clojure/blob/master/src/rs/ch1.clj
(defn teacupo-1 [x]
  (conde
   [(== 'tea x) s#]
   [(== 'cup x) s#]
   [s# u#]))

;; defining a custom teacup relation

;; relation -> is a kind of function, that when
;; given arguments, produces a goal

(run* [x]
  (teacupo x))

(run* [x]
  (teacupo-1 x))

(run* [x y]
  (conde
   [(teacupo x) (== true y)]
   [(== false x) (== true y)]))
;; ([tea _0] [cup _0] [u# _0])

(run* [x y]
  (conde
   [(teacupo-1 x) (== true y)]
   [(== false x) (== true y)]))

(run* [x y]
  (teacupo x)
  (teacupo y))

(run* [x y]
  (conde
   [(== (teacupo x) (teacupo x))]
   [(== false x) (teacupo y)]))
;; ( [false tea] [false cup] [tea _0] [cup _0] )

;; Law of conde
;;
;; Every successfull conde line
;; contributes one or more values.
