(ns core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer [run* ==]]))

(run* [q]
  (== 'olive q))

