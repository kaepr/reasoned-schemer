(ns ch3
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

(defn pair? [x]
  (or (lcons? x) (and (coll? x) (seq x))))

(defn pairo [p]
  (fresh [a d]
    (conso a d p)))

; instead of list?, use seq?

(seq? '((a) (a b) c))

(seq? '())

(seq? 'a)

(llist 'd 'a 't 'e 's)

(seq? (llist 'd 'a 't 'e 's))

(defn listo [l]
  (conde
   [(emptyo l) s#]
   [(pairo l) (fresh [d]
                (resto l d)
                (listo d))]
   [s# u#]))


(defn listo [l]
  (conde
   [(emptyo l)] ; if empty, great
   [(fresh [d]
      (resto l d) ; otherwise get the rest
      (listo d))])) ; and make sure its a list

;; Any top level s# can be removed from conde line

(run* [x]
  (listo (list 'a 'b x 'd)))
;; x can be anything, as it will still remain a list


(comment

  (run* [x]
    (listo (llist 'a 'b 'c x))) ;; infinite values possible
  ;; kind of hangs
  ;;
  ;; x could be empty
  ;; x could (_0)
  ;; or (_0 _1) and so on
  ;;
  ;; everything will satisfy it being a list

  ())

(run 1 [x]
  (listo (llist 'a 'b 'c x))) ;; x -> ()

(llist 'a 'b 'c ())

(llist 'a 'b 'c nil)

(run 5 [x]
  (listo (llist 'a 'b 'c x))) ;; only return 10 possible results

;; the increasing _0 variables represent the
;; fresh variable which gets added as part of listo definition
;; in the block of conde

; list of list
(defn lol? [l]
  (cond
    (empty? l) true
    (seq? (first l)) (lol? (rest l))))

(defn lolo [l]
  (conde
   [(emptyo l)] ;; if it empty, good
   [(fresh [a]
     (firsto l a) ;; define a to be first of l
     (listo a)) ;; make sure a is list
    (fresh [d]
      (resto l d) ;; define d to be rest of l
      (lolo d))])) ;; recurse

(comment

  (run* [q]
    (fresh [x y]
      (lolo `((a b) (~x c) (d ~y)))))

  ())

(run 10 [q]
  (fresh [x y]
    (lolo (list '(a b) (list x 'c) (list 'd y)))))
;; q will always be lolo,
;; as relation will be satisfied for any value of x or y

(run 1 [l]
  (lolo l))

(run 10 [l]
  (lolo l))

(run 1 [q]
  (fresh [x]
    (lolo `((a b) ~x))))

(run 1 [q]
  (fresh [x]
    (lolo (llist (list 'a 'b) x))))

(run 1 [x]
  (lolo (llist '(a b) '(c d) x)))

(run 5 [x]
  (lolo (llist '(a b) '(c d) x)))

(run 5 [x]
  (lolo x))

(llist 123 nil)

(defn singletono [l]
  (fresh [a]
    (== (llist a nil) l)))

;; list of singletons
(defn loso [l]
  (conde
   [(emptyo l)]
   [(fresh [a]
      (firsto l a)
      (singletono a))
    (fresh [d]
      (resto l d)
      (loso d))]))

(run 1 [z]
  (loso (llist '(g) z)))

(run 10 [z]
  (loso (llist '(g) z)))

(= (lcons 'e 'w) (llist 'e 'w))

(run 10 [r]
  (fresh [w x y z]
    (loso (llist '(g) (llist 'e w) (llist x y) z))
    (== (llist w (llist x y) z) r)))
;; does not work for me !

(run 10 [out]
  (fresh [w x y z]
    (== (llist '(g) (llist 'e w) (llist x y) z) out)
    (loso out)))

(run 10 [out w x y z]
  (fresh []
    (== (llist '(g) (llist 'e w) (llist x y) z) out)
    (loso out)))

(comment)


(defn member? [x l]
  (cond
    (empty? l) false
    (= (first l) x) true
    :else (member? x (rest l))))

(defn memberoo [x l]
  (conde
   ;; [(emptyo l) u#]
   [(firsto l x)]
   ;; [(fresh [a]
   ;;    (firsto l a)
   ;;    (== a x))]
   [(fresh [d]
           (resto l d)
           (memberoo x d))]))

;; there's already a membero in core.logic
;; so adding an extra o

(run* [q]
  (memberoo 'olive '(virgin olive oil)))

(run 10 [y]
  (memberoo y (list 'hummus 'with 'pita)))
;; y has to be a member


(run* [y]
  (memberoo y '()))

;; y has to be empty list
;; bcoz no members

(run* [y]
  (memberoo y '(hummus with pita)))

(comment

  (run* [y]
    (memberoo y l))
  ;; y will always be l
  ;; given l is proper list

  ())

(llist 'pear 'grape 'peaches)

(run* [y]
  (memberoo y (llist 'pear 'grape 'peaches)))

(run* [x]
  (memberoo 'e (llist 'pasta x 'fagioli)))
;; what are the values of x, for which e becomes a member
;; so only e

(run 1 [x]
  (memberoo 'e (list 'pasta 'e x 'fagioli)))
;; _0, it already a member before even reaching x
;; other sol is e itself
(run 10 [x]
  (memberoo 'e (list 'pasta x 'e 'fagioli)))
;; (e _0)
;; first e, then anything

(run* [x y]
  (memberoo 'e (list 'pasta x 'fagioli y)))
;; ([e _0] [_0 e])


(run* [q]
  (fresh [x y]
    (== (list 'pasta x 'fagioli y) q)
    (memberoo 'e q)))
; ((pasta e fagioli _0) (pasta _0 fagioli e))


(run 1 [l]
  (memberoo 'tofu l))

(comment

 (run* [l]
   (memberoo 'tofu l))
 ;; infinite

 ())

(run 5 [l]
  (memberoo 'tofu l))

(defn proper-membero [x l]
  (conde
   [(firsto l x) (fresh [d]
                   (resto l d)
                   (listo d))] ;; cdr of first must be list
   ;; thus proper list
   [(fresh [d]
      (resto l d)
      (proper-membero x d))]))

(run 12 [l]
  (proper-membero 'tofu l))
