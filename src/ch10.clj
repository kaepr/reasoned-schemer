(ns ch10
  (:refer-clojure :exclude [==]))

;; References from Logic Programming - Episode 1 Timothy Baldridge
;; https://www.youtube.com/watch?v=7CWEPRKOwgI&t=214s&ab_channel=ClojureTutorials

(gensym)
(type (gensym))

;; instead of using a vector as in the book
;; using a symbol, which generates a unique value
;; vectors in clojure won't be unique unless
;; there's a unique value in them anyway
;; adding a variable name for easier debugging
(defn lvar
  ([] (lvar "var_"))
  ([ident] (gensym (str "var_" ident "_"))))

(defn lvar? [v]
  (and (symbol? v)
       (.startsWith (str v) "var_")))

(comment

  (lvar)

  (lvar? (lvar))

  ())

;; using a two element vector as pair
(defn pair [a b]
  [a b])

;; (defn pair? [p]
;;   (and (vector? p) (= (count p) 2)))
;; using count as 2 doesn't always work

(defn pair? [p] (vector? p))

;; pair is an association
;; when the car of the pair is variable
;; and cdr is value containing zero or more variables
(def pcar first)

(def pcdr second)

(defn association? [p]
  (and
    (lvar? (pcar p))
    (or
     (lvar? (pcdr p))
     true))) ;; returning true for anything for now

(comment

  (pcdr (pair (lvar) `(~(lvar) e ~(lvar))))

  ())

;; a substitution is special kind of
;; list of associations
;;
;; substitution, an association
;; where the cdr is also a variable
;; which represents them fusing
;; two variables

(defn empty-s []
  []) ;; no associations

;; substitutions cannot contain
;; two or more associations with
;; for same variable
;; ((z a) (x w) (z b)) -> invalid

(defn assv [v s]
  (first (filterv (fn [a] (= v (pcar a))) s)))


(comment

 (let [v (lvar)
       u (lvar)
       s `[~(pair v 'b) ~(pair u 'y)]]
  (assv v s))

 (let [v (lvar)
       u (lvar)
       s `[~(pair v 'b) ~(pair u 'y)]]
  (assv u s))

 (let [v (lvar)
       u (lvar)
       s `[~(pair v 'b) ~(pair u 'y)]]
   (assv (lvar) s))

 ())

(defn walk [v s]
  (let [a (and (lvar? v) (assv v s))]
    (cond
      (pair? a) (walk (pcdr a) s)
      :else v)))
;; if after a walk
;; the variable `v` procuded a
;; another variable `x`,
;; then `x` is fresh
;; no ground value found for it

(comment

  (let
      [w (lvar)
       x (lvar)
       y (lvar)
       z (lvar)]
    (walk z `(~(pair z 'a) ~(pair x w) ~(pair y z))))


  (let
      [w (lvar)
       x (lvar)
       y (lvar)
       z (lvar)]
    (walk y `(~(pair z 'a) ~(pair x w) ~(pair y z))))

  (let
      [w (lvar)
       x (lvar)
       y (lvar)
       z (lvar)]
    (walk x `(~(pair z 'a) ~(pair x w) ~(pair y z))))

  (let
      [w (lvar)
       x (lvar)
       y (lvar)
       v (lvar)
       z (lvar)]
    (walk w `(~(pair x 'b) ~(pair z y) ~(pair w (list x 'e z)))))

  ())

;; does variable x occur in value v
;; in given substitutions
(defn occurs? [x v s]
  (let [v (walk v s)]
    (cond
      (lvar? v) (= v x)
      (pair? v) (or (occurs? x (pcar v) s)
                    (occurs? x (pcdr v) s))
      :else nil)))

(defn ext-s [x v s]
  (cond
    (occurs? x v s) nil ;; substitution cannot contain cycles
    ;; by construction
    :else (conj s (pair x v))))

(comment

  (let [x (lvar)]
    (occurs? x x (empty-s)))

  (let [x (lvar "x")
        y (lvar "y")]
    (occurs? x (pair y nil) `[~(pair y x)]))

  (let [x (lvar "x")]
    (ext-s x (pair x nil) (empty-s)))


  (let [x (lvar "x")
        y (lvar "y")]
    (ext-s x (pair y nil) `[~(pair y x)]))

  (let [x (lvar "x")
        y (lvar "y")
        z (lvar "z")
        s [(pair z x) (pair y z)] ; z -> x, y -> z
        s (ext-s x 'e s)] ; x -> e
    (and s (walk y s)))

  ())

(defn unify [u v s]
  (let [u (walk u s) ; u walks to it's value,
                     ; if still a variable, then it's fresh
        v (walk v s)] ; likewise
    (cond
      (= u v) s ;; they are infact, so dont' extend anything
      (lvar? u) (ext-s u v s) ; u is fresh, so s is extended, by assigning
                              ; value to u
      (lvar? v) (ext-s v u s) ; v is fresh, so s is extended, by assigning
                              ; value to v
      (and (pair? u) ;; recursively unify
           (pair? v)) (let [s (unify (pcar u) (pcar v) s)]
                        (and s (unify (pcdr u) (pcdr v) s)))
      :else nil)))
; Either returns false or an extended
; substitution with more associations

(comment

 (unify (lvar "x") (lvar "y") (empty-s)) ;; unified by making them equal

 (let [x (lvar "x")
       y (lvar "y")
       z (lvar "z")
       s [(pair x y) (pair z 42)]]
   (unify x z s)) ;; unified, by making y as 42

 (let [x (lvar "x")
       y (lvar "y")
       s [(pair x 42) (pair y 43)]]
   (unify x y s)) ;; not possible to unify them

 ())

;; a stream is either empty list
;; a pair whose cdr is stream, or a suspension
;;
;; suspension: A function formed from (fn [] body)
;; where ((fn [] body)) is a stream
;;

(pair 'a (pair 'b (pair 'c (pair 'd nil))))

(pair 'a
      (pair 'b
            ;; below is a suspension
            (fn [] (pair 'c (pair 'd nil)))))

;; another stream
(fn [] (pair 'a (pair 'b (pair 'c (pair 'd nil)))))

(defn == [u v]
  (fn [s]
    (if-let [s (unify u v s)]
      [s]
      [])))
;; produces a goal

(def s# (fn [s] [s]))
(def u# (fn [s] []))

;; a goal is a function
;; which expects a substitution map
;; and if it returns,  produces a
;; stream of substitutions
;; again, substitutions is a list of associations

((== true false) (empty-s))

((== false false) (empty-s))

(u# (empty-s))

(s# (empty-s))

(let [x (lvar "x")
      y (lvar "y")]
  ((== x y) (empty-s)))

(let [x (lvar "x")
      y (lvar "y")]
  ((== y x) (empty-s)))

;; right now these both are not equal to each other
;; but they should be, fixed later

(let [x (lvar "x")]
  ((== 1 x) (empty-s)))

(let [x (lvar "x")]
  ((== x 1) (empty-s)))

(defn append- [s t]
  (cond
    (empty? s) t
    ;; (pair? s) (conj (append- (pcdr s) t) (pcar s))
    ;; (pair? s) [(append- (pcdr s) t) (pcar s)]
    (pair? s) (pair (pcar s) (append- (pcdr s) t))
    :else (fn [] (append- t (s)))))

(defn disj2 [g1 g2]
  (fn [s]
    (append- (g1 s) (g2 s))))

(let [x (lvar "x")]
  ((disj2 (== 'olive x) (== 'oil x)) (empty-s)))

(defn nevero []
  (fn [s]
    (fn [] ((nevero) s))))

((nevero) (empty-s))
(((nevero) (empty-s)))
((((nevero) (empty-s))))

(let [x (lvar "x")
      s- ((disj2
           (== 'olive x)
           (nevero))
          (empty-s))]
  (println (pcar s-))
  (println (pcdr s-))
  s-)

(let [x (lvar "x")
      s- ((disj2
           (nevero)
           (== 'olive x))
          (empty-s))]
  s-)
