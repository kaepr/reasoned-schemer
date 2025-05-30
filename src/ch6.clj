(ns ch6
  (:require [clojure.core.logic :as l]))

(defn alwayso []
  (l/conde
   [l/s#]
   [(alwayso)]))

(l/run 1 [q]
  (alwayso))

(l/run 1 [q]
  (l/conde
   [l/s#]
   [(alwayso)]))

;; s# succeeds only once
;; alwayso succeeds any number of times

(comment

  (l/run* [q]
    (alwayso))
  ;; never finishes
  ;; always succeeds

  ())

(l/run 5 [q]
  (l/== 'onion q))
;; (onion)

(l/run 5 [q]
  (l/== 'onion q)
  (alwayso))
;; (onion onion onion onion onion)

(comment

  (l/run 1 [q]
    l/s#
    l/u#) ;; this fails

  (l/run 1 [q]
    (alwayso)
    l/u#)

  ;; never finishes
  ;; first succeeds
  ;; then fails
  ;; which success
  ;; but fails
  ;; and so on

  ())

(l/run 1 [q]
  (l/== 'garlic q)
  l/s#
  (l/== 'onion q))
;; ()

(comment

  (l/run 1 [q]
    (l/== 'garlic q)
    (alwayso)
    (l/== 'onion q))
  ;; no value
  ;; always keep retying

  ())

(l/run 1 [q]
  (l/conde
   [(l/== 'garlic q) (alwayso)]
   [(l/== 'onion q)])
  (l/== 'onion q))
;; (onion)

(comment

 (l/run 2 [q]
   (l/conde
    [(l/== 'garlic q) (alwayso)]
    [(l/== 'onion q)])
   (l/== 'onion q))

 ;; no value, stuck

 (l/run 5 [q]
   (l/conde
    [(l/== 'garlic q) (alwayso)]
    [(l/== 'onion q)])
   (l/== 'onion q))

 ())

(defn nevero []
  (nevero))
;; never fails or succeeds
;; u# just fails

(comment

  (l/run 1 [q]
    (nevero))
  ;; no value
  ;; never succeeds or fails


  (l/run 1 [q]
    l/u#
    (nevero))
  ;; should have given ()
  ;; as it fails in first u#
  ;; but results in stack overflow

  ())

(l/run 1 [q]
  (l/conde
   [l/s#]
   [(nevero)]))
; (_0)
; q can be anything
; first line succeeds

(comment

 (l/run 1 [q]
   (l/conde
    [(nevero)]
    [l/s#]))
 ;; should have given same as above
 ;; but fails in stack overflow

 (l/run 2 [q]
   (l/conde
    [l/s#]
    [(nevero)]))
 ;; no value

 (l/run 1 [q]
   (l/conde
    [l/s#]
    [(nevero)])
   l/u#)
 ;; no value

 (l/run 5 [q]
   (l/conde
    [(nevero)]
    [(alwayso)]
    [(nevero)]))
 ;; at first suceeds, give (_0)
 ;; but then stack overflow
 ;; probably missing some core logic feature
 ;; which does not evaluate all forms
 ;; probably because I used defn ??
 ;; TODO: find out

 ())

(l/run 6 [q]
  (l/conde
   []
   []))
