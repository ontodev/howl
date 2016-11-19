(ns howl.nquads
  "Convert HOWL to and from NQuads."
  (:require [clojure.string :as string]
            [howl.core :as core]
            [howl.link :as link]))

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
; [graph subject predicate object type/language]

(defn nquad-string->nquad
  "Given a line from an NQuads file,
   return an NQuad vector."
  [line]
  [])

(defn nquad->nquad-string
  [[graph-iri subject-iri predicate-iri {:keys [value datatype language]}]]
  (->> [(str "<" subject-iri ">")
        (str "<" predicate-iri ">")
        (cond
         language                      (str "\"" value "\"@" language)
         (= link/rdf:PlainLiteral datatype) (str "\"" value "\"")
         datatype                      (str "\"" value "\"^^<" datatype ">")
         :else                         (str "<" value ">"))
        (when graph-iri (str "<" graph-iri ">"))
        "."]
       (remove nil?)
       (string/join " ")))

(defn nquad->ntriple-string
  [nquad]
  )

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
  [{:keys [graph-iri subject-iri predicate-iri object datatype-iri language]}]
  [[graph-iri
    subject-iri
    predicate-iri
    {:value (-> object
                (string/replace "\n" "\\n")
                (string/replace "\"" "\\\""))
     :datatype datatype-iri
     :language language}]])




