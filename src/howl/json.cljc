(ns howl.json
  "Convert HOWL to and from JSON."
  (:require [clojure.walk :refer [postwalk keywordize-keys]]
            [clojure.data.json :as json]))

;; ## JSON

; The JSON conversion is almost trivial.
; The only trick is converting certain JSON strings to Clojure keywords.

(defn block->json-string
  [block])

(defn json-string->block
  [json])

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
    ("parse-tree" "graph-name" "subject-name" "predicate-name"
                  "datatype-name" "format-name" "target-name"
                  "content" "object")
    (json->parse value)
    value))

(defn json->block
  "Given a JSON representation of block,
   return the EDN representation."
  [json]
  (->> (json/read-str json :value-fn json->value)
       (map (fn [[k v]] [(keyword k) v]))
       (into {})))
