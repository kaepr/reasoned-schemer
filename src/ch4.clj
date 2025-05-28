(ns ch4
  (:require [clojure.core.logic :as l]))


;; takes in two lists
;; joins them
(defn- append [l t]
  (cond
    (empty? l) t
    :else (cons (first l)
                (append (rest l) t))))

(append '(a b c) '(d e))

(comment

   (append 'a '(d e)) ;; fails
   (append '(d e) 'a) ;; fails
   (concat '(d e) 'a) ;; fails

  ())

;; out must be value of `(@l @t)
(defn appendoo [l t out]
  (l/conde
   [(l/emptyo l) (l/== t out)]
   [(l/fresh [res]
      (l/fresh [d]
        (l/resto l d)
        (appendoo d t res))
      (l/fresh [a]
        (l/firsto l a)
        (l/conso a res out)))]))

(defn appendoo [l t out]
  (l/conde
   [(l/emptyo l) (l/== t out)]
   [(l/fresh [a d res]
      (l/conso a d l) ;; takes out the hd, tl from the list
      (appendoo d t res)
      (l/conso a res out))])) ;; builds a hd, tl into list

(l/run 6 [x]
  (l/fresh [y z]
    (appendoo x y z))) ;; has to be of form list

(l/run 6 [y]
  (l/fresh [x z]
    (appendoo x y z))) ;; can be anything
;; as x and z can still do the appending

(l/run 6 [z]
  (l/fresh [x y]
    (appendoo x y z)))

(l/run 6 [x y z]
  (appendoo x y z))


(append '(a) nil) ;; although clojure does not allow
;; non seqs to be append
;; its valid in scheme
;; thus the difference in appendo and append


(l/run* [x]
  (l/appendo '(cake)
             '(tastes yummy)
             x))

(l/llist 'cake '& 'ice 'y)

(l/llist 'tastes 'yummy)

(l/run* [x]
  (l/fresh [y]
    (l/appendo `(cake & ice ~y)
               `(tastes yummy)
               x)))

(l/run* [x]
  (l/fresh [y]
    (l/appendo
     `(cake & ice cream)
     y
     x)))

(l/run 1 [x]
  (l/fresh [y]
    (l/appendo
     (l/llist 'cake&ice y)
     '(d t)
     x)))

(l/run 5 [x]
  (l/fresh [y]
    (l/appendo
     (l/llist 'cake&ice y)
     '(d t)
     x)))

(l/run 10 [y]
  (l/fresh [x]
    (l/appendo
     (l/llist 'cake&ice y)
     '(d t)
     x)))

(l/run 5 [x]
  (l/fresh [y]
    (l/appendo
     (l/llist 'cake '& 'ice y)
     (l/llist 'd 't y)
     x)))

(l/run* [x]
  (l/fresh [z]
    (l/appendo
     (list 'cake '& 'ice 'cream)
     (l/llist 'd 't z)
     x)))
;; there's only value
;; because `l` is constant
;; this is value which keeps changing
;; in appendo conde
;; so nothing really changes

(l/run 10 [x]
  (l/fresh [y]
    (l/appendo x y '(cake & ice d t))))

(l/run 10 [y]
  (l/fresh [x]
    (l/appendo x y '(cake & ice d t))))

; all prefixes, then all suffixes

(l/run 6 [x y]
  (appendoo x y '(cake & ice d t)))

(comment

  (l/run 7 [x y]
    (appendoo x y '(cake & ice d t)))
  ;; no value, as it's trying to find the last 7th solution
  ;; which does not exist

  ())

;; this is fixed by swapping the
;; last two goals in appendoo

(defn appendoo [l t out]
  (l/conde
   [(l/emptyo l) (l/== t out)]
   [(l/fresh [a d res]
      (l/conso a d l) ;; takes out the hd, tl from the list
      (l/conso a res out) ;; builds a hd, tl into list
      (appendoo d t res))]))

(l/run* [x y]
  (appendoo x y '(cake & ice d t)))
;; now fixed, only shows the possible values

;; First Commandment
;; Within each sequence of goals
;; move non recursive goals
;; before recursive goals
;;
;; Earlier example was trying to search
;; for remaining lists which satisfy appendo
;;

(defn swappendo [l t out]
  (l/conde
   [(l/fresh [a d res]
      (l/conso a d l) ;; takes out the hd, tl from the list
      (l/conso a res out) ;; builds a hd, tl into list
      (swappendo d t res))]
   [(l/emptyo l) (l/== t out)]))

(l/run* [x y]
  (swappendo x y '(cake & ice d t)))
;; same as appendo

;; Law of swapping conde lines
;; Swapping two conde lines does not
;; affect the values contributed by conde

(defn unwrapo [x out]
  (l/conde
   [(l/fresh [a]
      (l/firsto x a)
      (unwrapo a out))]
   [(l/== x out)]))

(l/run* [x]
  (unwrapo '((((pizza)))) x))

;; returns all partially wrapped versions
;; of the input

(l/run 1 [x]
  (unwrapo x 'pizza))

(l/run 10 [x]
  (unwrapo x 'pizza))
