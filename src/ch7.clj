(ns ch7
  (:require [clojure.core.logic :as l]))

(defn bit-xoro [x y r]
  (l/conde
   [(l/== 0 x) (l/== 0 y) (l/== 0 r)]
   [(l/== 0 x) (l/== 1 y) (l/== 1 r)]
   [(l/== 1 x) (l/== 0 y) (l/== 1 r)]
   [(l/== 1 x) (l/== 1 y) (l/== 0 r)]))

(l/run* [x y]
  (bit-xoro x y 0))

(l/run* [x y]
  (bit-xoro x y 1))

(l/run* [x y r]
  (bit-xoro x y r))

(defn bit-ando [x y r]
  (l/conde
   [(l/== 0 x) (l/== 0 y) (l/== 0 r)]
   [(l/== 0 x) (l/== 1 y) (l/== 0 r)]
   [(l/== 1 x) (l/== 0 y) (l/== 0 r)]
   [(l/== 1 x) (l/== 1 y) (l/== 1 r)]))

;; wrong
;; cannot be done using just defn
;; (defn half-addero [x y r c]
;;   (bit-xoro x y r)
;;   (bit-ando x y c))

;; switched to using dummy fresh
;; instead of using defne
(defn half-addero [x y r c]
  (l/fresh []
    (bit-xoro x y r)
    (bit-ando x y c)))

(l/run* [r]
  (half-addero 1 1 r 1))

(l/run* [x y r c]
  (half-addero x y r c))

(defn full-addero [b x y r c]
  (l/fresh [w xy wz]
    (half-addero x y w xy)
    (half-addero w b r wz)
    (bit-xoro xy wz c)))

(l/run* [r c]
  (full-addero 0 1 1 r c))

(l/run* [r c]
  (full-addero 1 1 1 r c))

(l/run* [b x y r c]
  (full-addero b x y r c))

;; 0 -> ()
;; 1 -> (1)
;; 5 -> (1 0 1)
;; 7 -> (1 1 1)
;; 6 -> (0 1 1) 0*2^0 + 1*2^1 + 1*2^2
;; 19 -> (1 1 0 0 1)

;; every non empty lists
;; except which represents 0
;; ends with 1

(defn build-num [n]
  (cond
    (zero? n) '()
    (even? n) (cons 0 (build-num (/ n 2)))
    (odd? n) (cons 1 (build-num (/ (dec n) 2)))))

(build-num 6)

(defn build-name [n]
  (cond
    (odd? n) (cons 1 (build-num (/ (dec n) 2)))
    (even? n) (cons 0 (build-num (/ n 2)))
    (zero? n) '()))

;; for any number n,
;; one and only one cond question is true
;;
;; we can order the cond in any order
;; called non overlapping property

(defn poso [n]
  (l/fresh [a d]
    (l/== (l/llist a d) n)))

;; any positive number
;; q is always fresh, if poso suceeds
;; so always truthy value, given n is positive

(l/run* [q]
  (poso '(0 1 1)))

(l/run* [q]
  (poso '(1)))

(l/run* [q]
  (poso '(0)))

(l/run* [q]
  (poso '()))

(l/run* [r]
  (poso r))

;; poso r always succeeds
;; when r is fresh

(defn >1o [n]
  (l/fresh [a ad dd]
    (l/== (l/llist a ad dd) n)))

(l/run* [q]
  (>1o '(0 1 1)))

(l/run* [q]
  (>1o '(0 1)))

(l/run* [q]
  (>1o '(1))) ;; no value

(l/run* [q]
  (>1o '())) ;; no value

(l/run* [r]
  (>1o r))

;; (l/run 3 [x y r]
;;   (addero))

(declare gen-addero)

;;
;; was having trouble implementing, took reference below
;; https://www.youtube.com/watch?v=h2unN2XNHBU&t=9621s&ab_channel=faster-than-light-memes

(= '(1) (l/llist 1 nil))

;; did not work with '(1) for some reason
(defn addero [b n m r]
  (l/conde
   [(l/== 0 b) (l/== '() m) (l/== n r)]
   [(l/== 0 b) (l/== '() n) (l/== m r) (poso m)]
   [(l/== 1 b) (l/== '() m) (addero 0 n (l/llist 1 nil) r)]
   [(l/== 1 b) (l/== '() n) (poso m) (addero 0 (l/llist 1 nil) m r)]
   [(l/== (l/llist 1 nil) n) (l/== (l/llist 1 nil) m)
    (l/fresh [a c]
      (l/== (l/llist a c nil) r)
      (full-addero b 1 1 a c))]
   [(l/== (l/llist 1 nil) n) (gen-addero b n m r)]
   [(l/== (l/llist 1 nil) m) (>1o n) (>1o r) (addero b (l/llist 1 nil) n r)]
   [(>1o n) (gen-addero b n m r)]))

(defn gen-addero [b n m r]
  (l/fresh [a c d e x y z]
    (l/== (l/llist a x) n)
    (l/== (l/llist d y) m)
    (poso y)
    (l/== (l/llist c z) r)
    (poso z)
    (full-addero b a d c e)
    (addero e x y z)))

(l/run* [s]
  (gen-addero 1 '(0 1 1) '(1 1) s))

(l/run* [x y]
  (addero 0 x y '(1 0 1)))

(defn pluso [n m k]
  (addero 0 n m k))

(l/run* [x y]
  (pluso x y '(1 0 1)))

(defn minuso [n m k]
  (pluso m k n))

(l/run* [q]
  (minuso '(0 0 0 1) '(1 0 1) q))

(defn lengtho [l n]
  (l/conde
   [(l/emptyo l) (l/== '() n)]
   [(l/fresh [d res]
      (l/resto l d)
      (pluso (l/llist 1 nil) res n)
      (lengtho d res))]))

(l/run 1 [n]
  (lengtho '(a b c d) n))

(l/run 1 [n]
  (lengtho '(a b c d e) n))

(comment

 (l/run* [q]
   (lengtho '(1 0 1) q))
 ;; find a number, which whose length is same as
 ;; its value
 ;; in this case, find some number,
 ;; where array length is length is 3

 (l/run* [q]
   (lengtho '(0 1) q))

 ())

(l/run* [q]
  (lengtho '(1 0 1) 3))

(l/run* [ls]
  (lengtho ls '(1 0 1)))

(l/run 3 [q]
  (lengtho q q))
;; (() (1) (0 1))
;; these numbers are same as their length

(comment

 (l/run 4 [q]
   (lengtho q q))
 ;; no value, keeps looking for fourth value

 ())
