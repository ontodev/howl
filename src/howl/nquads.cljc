(ns howl.nquads
  "Convert HOWL to and from NQuads."
  (:require [clojure.string :as string]
            [howl.core :as core]
            [howl.link :as link]
            [howl.util :as util]))

;; ## NQUADS
;
; NQuads is a line-based concrete syntax for RDF.
; Each line consists of a subject, predicate, object, and optional graph.
; The predicate and graph are always absolute IRIs.
; The subject is an absolute IRI or blank node.
; The object can be: an IRI, a blank node, or a literal.
; Literals are quoted strings with an optional language tag or type IRI.
;
; We represent NQuads as vectors of strings:
; [graph subject predicate object datatype]

; When converting blocks to NQuads
; Be default, do nothing.

(defmulti block->nquads
  "Given a (fully expanded) block,
   return a sequence of zero or more NQuad vectors."
  :block-type)

(defmethod block->nquads :default
  [block]
  [])


(defmethod block->nquads :STATEMENT_BLOCK
  [{:keys [graph-iri subject-iri predicate-iri object datatype-iri]}]
  [[graph-iri
    subject-iri
    predicate-iri
    (-> object
        (string/replace "\n" "\\n")
        (string/replace "\"" "\\\""))
    datatype-iri]])

(defn nquad->blocks
  "Given an environment and an nquad vector,
   return the updated environment and zero or more block maps."
  [{:keys [current-graph current-subject iri-labels] :as env}
   [graph subject predicate object datatype]]
  (remove
   nil?
   [; many new graph
    ; maybe new subject
    ; new statement
    ]))



(defn nquad-string->nquad
  "Given a line from an NQuads file,
   return an NQuad vector."
  [line]
  [])

(defn subject->string
  [subject]
  (if (util/starts-with? subject "_")
    subject
    (str "<" subject ">")))

(defn object->string
  [object datatype]
  (cond
   (or (nil? datatype) (= "PLAIN" datatype)) (str "\"" object "\"")
   (= "LINK" datatype) (str "<" object ">")
   (util/starts-with? datatype "@") (str "\"" object "\"" datatype)
   :else (str "\"" object "\"^^<" datatype ">")))

(defn nquad->nquad-string
  [[graph subject predicate object datatype]]
  (->> [(subject->string subject)
        (str "<" predicate ">")
        (object->string object datatype)
        (when graph (str "<" graph ">"))
        "."]
       (remove nil?)
       (string/join " ")))

(defn nquad->ntriple-string
  [nquad]
  )

