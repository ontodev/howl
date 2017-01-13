(ns howl.constants.rdf
  (:require [howl.util :as util :refer [rdf>]]))

(def ^:const type (rdf> "type"))
(def ^:const intersection-of (rdf> "intersectionOf"))
(def ^:const union-of (rdf> "unionOf"))
(def ^:const list (rdf> "List"))
(def ^:const first (rdf> "first"))
(def ^:const next (rdf> "next"))
(def ^:const rest (rdf> "rest"))
(def ^:const NIL (rdf> "nil"))
