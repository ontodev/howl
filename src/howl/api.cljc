(ns howl.api
  "Cross-platform API for HOWL."
  (:require [clojure.string :as string]
            [howl.core :as core]
            [howl.nquads :as nq]))

(defn ^:export parse-string
  "Given an input string in HOWL format,
   return a lazy sequence of parse results."
  [content]
  (transduce
     (comp
      (core/merge-lines "local")
      (map core/parse-block))
     conj
     (string/split-lines content)))

(defn ^:export convert-to-quads
  "Given an input string in HOWL format,
   return an output string with a sequence of N-Quads."
  [content]
  (->> content
       parse-string
       (transduce (nq/render-quads) conj)
       (map nq/quad-to-string)
       (string/join "\n")))
