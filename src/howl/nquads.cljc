(ns howl.nquads
  "Convert HOWL to and from NQuads."
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [howl.core :as core]
            [howl.link :as link]
            [howl.util :as util]))

;; ## NQUADS
;
; NQuads is a line-based concrete syntax for RDF.
; Each line consists of a subject, predicate, object, and optional graph.
; The predicate and graph are always absolute IRIs.
;<http://example.com/subject> <http://www.w3.org/2000/01/rdf-schema#label> "This is a label." <http://example.com/graph> . The subject is an absolute IRI or blank node.
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
  [{:keys [graph subject predicate object datatype]}]
  [[graph
    subject
    predicate
    (-> object
        (string/replace "\n" "\\n")
        (string/replace "\"" "\\\""))
    datatype]])

(defn nquad->blocks
  "Given an environment and an nquad vector,
   return the updated environment and zero or more block maps."
  [{:keys [graph subject] :as env}
   [new-graph new-subject predicate object datatype]]
  (->> [(when-not (= graph new-graph)
          {:block-type :GRAPH_BLOCK
           :graph new-graph})
        (when-not (= subject new-subject)
          {:block-type :SUBJECT_BLOCK
           :subject new-subject})
        {:block-type :STATEMENT_BLOCK
         :graph new-graph
         :subject new-subject
         :predicate predicate
         :object object
         :content
         (if (= "LINK" datatype)
           (link/iri->name env object)
           (-> object
               (string/replace "\\n" "\n")
               (string/replace "\\\"" "\"")))
         :datatype datatype}]
       (remove nil?)
       (map (partial core/iris->names env))))

; TODO: Fix lexical value
(def nquad-grammar-partial "
NQUAD = SUBJECT ' ' PREDICATE ' ' OBJECT (' ' GRAPH)? ' .'
GRAPH = IRIREF
SUBJECT = IRIREF | BLANK_NODE_LABEL
PREDICATE = IRIREF
OBJECT = IRIREF | BLANK_NODE_LABEL | TYPED_LITERAL | LANGUAGE_LITERAL | PLAIN_LITERAL
TYPED_LITERAL = '\"' LEXICAL_VALUE '\"^^' IRIREF
LANGUAGE_LITERAL = '\"' LEXICAL_VALUE '\"' LANGUAGE_TAG
PLAIN_LITERAL = '\"' LEXICAL_VALUE '\"'
LEXICAL_VALUE = (#'[^\"\\\\]+' | ESCAPED_CHAR)*
<ESCAPED_CHAR> = #'\\\\.'")

(def nquad-grammar
  (string/join
   \newline
   [nquad-grammar-partial
    link/link-grammar]))

(def nquad-parser (insta/parser nquad-grammar))

(def nquad-transformations
  (merge
   link/link-transformations
   {:LEXICAL_VALUE
    (fn [& xs] [:LEXICAL_VALUE (apply str xs)])}))

(defn parse-nquad
  [line]
  (let [result (nquad-parser line)]
    (when (insta/failure? result)
      (println result)
      (throw (Exception. "NQuad parse failure")))
    (->> result
         (insta/transform nquad-transformations)
         vec)))

(defn nquad-string->nquad
  "Given a line from an NQuads file,
   return an NQuad vector."
  [line]
  (let [result (parse-nquad line)]
    [(when (= 9 (count result)) (get-in result [7 1 2]))
     (case (get-in result [1 1 0])
       :IRIREF           (get-in result [1 1 2])
       :BLANK_NODE_LABEL (str "_:" (get-in result [1 1 2])))
     (get-in result [3 1 2])
     (case (get-in result [5 1 0])
       :IRIREF           (get-in result [5 1 2])
       :BLANK_NODE_LABEL (str "_:" (get-in result [5 1 2]))
       :TYPED_LITERAL    (get-in result [5 1 2 1])
       :LANGUAGE_LITERAL (get-in result [5 1 2 1])
       :PLAIN_LITERAL    (get-in result [5 1 2 1]))
     (case (get-in result [5 1 0])
       :IRIREF           "LINK"
       :BLANK_NODE_LABEL "LINK"
       :TYPED_LITERAL    (get-in result [5 1 4 2])
       :LANGUAGE_LITERAL (str "@" (get-in result [5 1 4 2]))
       :PLAIN_LITERAL    "PLAIN")]))

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
  [nquad])

(defn lines->blocks
  "Given a sequence of lines,
   and return a lazy sequence of block maps."
  [{:keys [source] :or {source "interactive"} :as env} lines]
  (reduce
   (fn [env nquad]
     (let [blocks (nquad->blocks env nquad)]
       (-> env
           (assoc :graph (:graph (last blocks)))
           (assoc :subject (:subject (last blocks)))
           (update-in [:blocks] (fnil concat []) blocks)
           (update-in [:line] inc))))
   (assoc env :source source :line 1)
   (map nquad-string->nquad lines)))
