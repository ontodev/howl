(ns howl.api
  "Cross-platform API for HOWL."
  (:require [clojure.string :as string]
            [howl.core :as core]))

(defn append-newline
  [s]
  (str s \newline))

(defn parse-howl-strings
  "Given one or more HOWL strings,
   return an environment with a :blocks sequence."
  [& howl-strings]
  (reduce
   (fn [env howl-string]
     (->> (string/split-lines howl-string)
          (map append-newline)
          (core/lines->blocks (core/reset-environment env))))
   {}
   howl-strings))

(defn ^:export howl-to-nquads
  "Given an input string in HOWL format,
   return a string of N-Quads."
  [& howl-strings]
  (->> (apply parse-howl-strings howl-strings)
       :blocks
       (mapcat core/block->nquads)
       (map core/nquad->nquad-string)
       (string/join \newline)
       append-newline))
