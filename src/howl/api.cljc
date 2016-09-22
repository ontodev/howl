(ns howl.api
  "Cross-platform API for HOWL."
  (:require [clojure.string :as string]
            [howl.core :as core]))

(defn ^:export howl-to-nquads
  "Given an input string in HOWL format,
   return a string of N-Quads."
  [content]
  (->> (string/split-lines content)
       core/parse-lines
       core/blocks->nquads
       (map core/nquad->string)
       (string/join \newline)))
