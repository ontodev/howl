(ns howl.api
  "Cross-platform API for HOWL."
  (:require [clojure.string :as string]
            [howl.core :as core]
            [howl.nquads :as nq]))

(defn ^:export howl-to-nquads
  "Given an input string in HOWL format,
   return a string of N-Quads."
  [content]
  (->> (string/split-lines content)
       (nq/lines-to-quads
        (fn [state line]
          (->> (core/merge-line state line)
               core/parse-block
               core/annotate-block
               core/expand-names
               nq/convert-quads))
        {:file-name "local"})
       (map nq/quad-to-string)
       (string/join "\n")))
