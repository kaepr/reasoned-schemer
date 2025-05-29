(ns ch5
  (:require [clojure.core.logic :as l]))

;; remove elements from l
;; until x is encountered
(defn mem [x l]
  (cond
    (empty? l) false
    (= (first l) x) l
    :else (mem x (rest l))))

(mem 'fig '(roll okra fig beet roll pea))

(mem 'fig
     '(roll okra beet beet roll pea))

(mem 'roll
     (mem 'fig
          '(roll okra fig beet roll pea)))

(defn memo [x l out]
  (l/conde
   [(l/emptyo l) l/u#]
   [(l/fresh [a]
      (l/firsto l a)
      (l/== a x))
    (l/== l out)]
   [l/s# (l/fresh [d]
           (l/resto l d)
           (memo x d out))]))

(defn memo [x l out]
  (l/conde
   [(l/firsto l x) (l/== l out)]
   [(l/fresh [d]
           (l/resto l d)
           (memo x d out))]))

(l/run* [q]
  (memo 'fig '(pea) '(pea))) ;; not satisfied

(l/run* [out]
  (memo 'fig '(fig) out)) ;; only satisfied
;; when out is (fig)

(l/run* [out]
  (memo 'fig '(fig pea) out))

(l/run* [out]
  (memo 'fig '(abc fig pea) out))

(l/run* [r]
  (memo r
        '(roll okra fig beet fig pea)
        '(fig beet fig pea)))

(l/run* [x]
  (memo 'fig '(fig pea) (list 'pea x)))

(l/run* [out]
  (memo
   'fig
   '(beet fig pea)
   out))

(l/run 1 [out]
  (memo 'fig '(fig fig pea) out))

(l/run* [out]
  (memo 'fig '(fig fig pea) out))

;; This happends because in conde
;; every successfull branch contributes
;; one or more values.
;;
;; Cond statements in normal programming
;; takes only branch. Whereas it goes through every path.
;;

(l/run* [out]
  (l/fresh [x]
    (memo 'fig (list 'a x 'c 'fig 'e) out)))

(l/run 5 [x y]
  (memo 'fig (l/llist 'fig 'd 'fig 'e y) x))

;; removes first occurence of x
;; and returns list back
(defn rember [x l]
  (cond
    (empty? l) '()
    (= (first l) x) (rest l)
    :else (cons (first l) (rember x (rest l)))))

(rember 'pea '(a b pea d pea e))

(defn rembero [x l out]
  (l/conde
   [(l/emptyo l) (l/== '() out)]
   [(l/fresh [a]
      (l/firsto l a)
      (l/== a x))
    (l/resto l out)]
   [l/s# (l/fresh [res]
           (l/fresh [d]
             (l/resto l d)
             (rembero x d res))
           (l/fresh [a]
             (l/firsto l a)
             (l/conso a res out)))]))

(defn rembero [x l out]
  (l/conde
   [(l/emptyo l) (l/== '() out)]
   [(l/conso x out l)]
   [(l/fresh [a d res]
      (l/conso a d l)
      (l/conso a res out)
      (rembero x d res))]))

(l/run* [out]
  (rembero 'pea '(pea) out))

(l/run* [out]
  (rembero 'pea '(pea pea) out))

(l/run* [out]
  (l/fresh [y z]
    (rembero y (list 'a 'b y 'd z 'e)
             out)))

(l/run* [y z]
  (rembero y
           (list y 'd z 'e)
           (list y 'd 'e)))

(l/run 4 [y z w out]
  (rembero y (l/llist z w) out))

(l/run 5 [y z w out]
  (rembero y (l/llist z w) out))
