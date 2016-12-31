(ns howl.json
  "Convert HOWL to and from JSON."
  (:require [clojure.walk :refer [postwalk keywordize-keys]]
            [howl.util :as util]))

;; ## JSON

; The JSON conversion is almost trivial.
; The only trick is converting certain JSON strings to Clojure keywords.

(defn block->json-string
  [block]
  (util/write-json block))

(defn keywordize-child-maps
  "Given a map where the values are maps,
   convert the keys of the child maps to keywords."
  [coll]
  (->> coll
       (map (fn [[k v]] [k (keywordize-keys v)]))
       (into {})))

(defn json->parse
  "Given the JSON representation of a parse vector,
   return the EDN representation."
  [json]
  (postwalk
   (fn [form]
     (if (vector? form)
       (assoc form 0 (keyword (first form)))
       form))
   json))

(defn json->value
  "Given a JSON key and value,
   return an EDN value."
  [key value]
  (case key
    "block-type" (keyword value)
    "labels"     (keywordize-child-maps value)
    ("parse-tree" "graph-name" "subject-name" "predicate-name" "target-name"
                  "content" "object")
    (json->parse value)
    "datatype-names"
    (vec (map json->parse value))
    value))

(defn json->block
  "Given a JSON representation of block,
   return the EDN representation."
  [json]
  (->> (util/read-json json)
       ;; FIXME TODO - this value traversal needs to be a tree-walk, not a naive map.
       ;;              should really use tree-seq https://clojuredocs.org/clojure.core/tree-seq
       (map (fn [[k v]] [k (json->value k v)]))
       (map (fn [[k v]] [(keyword k) v]))
       (into {})))
