(ns howl.constants.rdf
  (:refer-clojure :exclude [type list first next rest]))

(defn rdf> [name] (str "http://www.w3.org/1999/02/22-rdf-syntax-ns#" name))
(defn rdf-schema> [name] (str "http://www.w3.org/2000/01/rdf-schema#" name))

(def ^:const type (rdf> "type"))
(def ^:const intersection-of (rdf> "intersectionOf"))
(def ^:const union-of (rdf> "unionOf"))
(def ^:const list (rdf> "List"))
(def ^:const first (rdf> "first"))
(def ^:const next (rdf> "next"))
(def ^:const rest (rdf> "rest"))
(def ^:const NIL (rdf> "nil"))

(def ^:const label (rdf-schema> "label"))
(def ^:const sub-class-of (rdf-schema> "subClassOf"))
