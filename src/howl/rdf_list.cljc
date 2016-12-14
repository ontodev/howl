(ns howl.rdf-list
  "Parse and process RDF Lists syntax."
  (:require [clojure.string :as string]
            [clojure.walk :refer [postwalk]]
            [instaparse.core :as insta]
            [howl.util :as util :refer [<> owl> rdf>]]
            [howl.link :as link]
            [howl.core :as core]
            [howl.howl :as howl]
            [howl.nquads :as nquads]))

(def rdf-list-iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#List")
(def rdf-first "http://www.w3.org/1999/02/22-rdf-syntax-ns#first")
(def rdf-next "http://www.w3.org/1999/02/22-rdf-syntax-ns#next")
(def rdf-nil "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")

(def list-grammar
  (str "
<ITEMS> = WHITESPACE ITEM+
ITEM = '-' DATATYPES COLON #'.+' WHITESPACE
COLON = #' *' ':'  #'(\n| )+'
WHITESPACE  = #'(\\r|\\n|\\s)*'"
       link/link-grammar))

(def list-parser (insta/parser list-grammar))

(defn parse-list
  [content]
  (let [result (list-parser content)]
    (when (insta/failure? result)
      (println result)
      (throw (Exception. "RDF List parser failure")))
    result))

(defmethod howl/parse-content ["LINK" rdf-list-iri]
  [env datatypes unparsed]
  (vec (concat [:RDF_LIST] (parse-list unparsed))))

(defmethod core/content-names->iris ["LINK" rdf-list-iri]
  [env datatypes content]
  (->> content
       (filter vector?)
       (filter #(= :ITEM (first %)))
       (map
        (fn [item]
          (let [dt (get-in item [2 2])
                dt (when dt (link/unpack-datatype env dt))
                dt (or dt (get datatypes 2) "PLAIN")]
            {:object (get item 4) :datatype dt})))
       (concat [:RDF_LIST])
       vec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; to NQuads

(defn reduce-list
  "Build an RDF List, starting at the end."
  [graph items]
  (reduce
   (fn [quads {:keys [object datatype]}]
     (let [n (get-in quads [0 1] rdf-nil)
           b (link/random-blank-node)]
       (vec
        (concat
         [[graph b rdf-first object datatype]
          [graph b rdf-next n "LINK"]]
         quads))))
   []
   (->> items rest reverse)))

(defmethod nquads/object->nquads ["LINK" rdf-list-iri]
  [graph datatypes object]
  (let [result (reduce-list graph object)]
    [(second (first result)) result]))
