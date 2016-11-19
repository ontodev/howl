(ns howl.json
  "Convert HOWL to and from JSON."
  (:require [clojure.walk :refer [postwalk keywordize-keys]]
            [clojure.data.json :as json]))

;; ## JSON

; The JSON conversion is almost trivial.
; The only trick is converting certain JSON strings to Clojure keywords.

(defn block->json-string
  [block]
  )

(defn json-string->block
  [json]
  )

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
  [key value]
  (case key
    "block-type" (keyword value)
    "labels"     (->> value (map (fn [[k v]] [k (keywordize-keys v)])) (into {}))
    "parse-tree" (json->parse value)
    "graph-name" (json->parse value)
    "subject"    (json->parse value)
    "predicate"  (json->parse value)
    "datatype"   (json->parse value)
    "content"    (json->parse value)
    value))

(defn json->block
  "Given a JSON representation of block,
   return the EDN representation."
  [json]
  (->> (json/read-str json :value-fn json->value)
       (map (fn [[k v]] [(keyword k) v]))
       (into {})))



