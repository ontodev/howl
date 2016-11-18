(ns howl.api
  "Cross-platform API for HOWL."
  (:require [clojure.string :as string]
            [howl.core :as core]))

(defn append-newline
  [s]
  (str s \newline))

(defn ^:export howl-to-nquads
  "Given an input string in HOWL format,
   return a string of N-Quads."
  [content]
  (->> (string/split-lines content)
       (map append-newline)
       (core/lines->blocks {})
       :blocks
       (mapcat core/block->nquads)
       (map core/nquad->nquad-string)
       (string/join \newline)
       append-newline))
