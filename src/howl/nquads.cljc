(ns howl.nquads
  "Convert HOWL to and from NQuads."
  (:require [clojure.string :as string]
            clojure.set
            [instaparse.core :as insta]
            [howl.core :as core]
            [howl.link :as link]
            [howl.util :as util]))

(def rdf:type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
(def owl:Axiom "http://www.w3.org/2002/07/owl#Axiom")
(def annSource "http://www.w3.org/2002/07/owl#annotatedSource")
(def annProperty "http://www.w3.org/2002/07/owl#annotatedProperty")
(def annTarget "http://www.w3.org/2002/07/owl#annotatedTarget")
(def annPredicates #{rdf:type annSource annProperty annTarget})

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

(defn format-object
  [object-string]
  (-> object-string
      (string/replace "\n" "\\n")
      (string/replace "\"" "\\\"")))

(defn annotation-nquads
  "Given a subject (blank node label) and a vector for the annotation target,
   return four NQuads representing the OWL annotation axiom."
  [subject [source property target target-datatype]]
  [[nil subject rdf:type owl:Axiom "LINK"]
   [nil subject annSource source "LINK"]
   [nil subject annProperty property "LINK"]
   [nil subject annTarget (format-object target) target-datatype]])

(defmethod block->nquads :STATEMENT_BLOCK
  [{:keys [annotation-target graph subject predicate object datatype]}]
  (conj
   (when annotation-target (annotation-nquads subject annotation-target))
   [graph subject predicate (format-object object) datatype]))

(defn insert-annotations
  [depth annotation-map {:keys [subject predicate object datatype] :as block}]
  (let [annotations (get annotation-map [subject predicate object datatype])]
    (concat
     [(assoc block :arrows (apply str (repeat depth ">")))]
     (->> annotations
          (sort-by :object)
          (mapcat (partial insert-annotations (inc depth) annotation-map))))))

(defn nquad->blocks
  "Given an environment and an nquad vector,
   return the updated environment and zero or more block maps."
  [{:keys [graph subject] :as env}
   annotation-map
   [new-graph new-subject predicate object datatype]]
  (insert-annotations
   0
   annotation-map
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
    :datatype datatype}))

; We do not collapse annotations
; if the subject is used as an object
; that's not also an annotation.

(defn annotation-spoilers
  "Given a sequence of NQuads for a graph,
   return a set of blank node labels
   that are not valid subjects for OWL annoations."
  [nquads]
  (->> nquads
       (remove #(= annSource (nth % 2)))
       (map #(nth % 3))
       (filter #(util/starts-with? % "_:"))
       set))

; A collapsible annotation:
; - subject is not a spoiler
; - has a blank node as subject
; - has exactly five nquads
; - rdf:type is owl:Axiom
; - contains these three predicates: annotatedSource, annotatedProperty, annotatedTarget

(defn is-collapsible-annotation?
  [spoilers subject nquads]
  (and
   (not (contains? spoilers subject))
   (util/starts-with? subject "_:")
   (= 5 (count nquads))
   (->> nquads
        (map (fn [[g s p o d]] (= [rdf:type owl:Axiom] [p o])))
        first)
   (->> nquads
        (map #(nth % 2))
        set
        (clojure.set/subset? annPredicates))))

(defn process-annotation
  [env nquads]
  (let [coll
        (reduce (fn [c [g s p o d]] (assoc-in c [p] [o d])) {} nquads)
        [graph subject predicate object datatype]
        (->> nquads
             (remove #(contains? annPredicates (nth % 2)))
             first)]
    {:block-type :STATEMENT_BLOCK
     :arrows nil
     :annotation-target
     [(get-in coll [annSource 0])
      (get-in coll [annProperty 0])
      (get-in coll [annTarget 0])
      (get-in coll [annTarget 1])]
     :graph graph
     :subject subject
     :predicate predicate
     :object object
     :content object
     :datatype datatype}))

(defn process-annotations
  "Given stuff
   return a map from quads to sets of annotation blocks for that quad
   plus a :subjects key for remaining subjects?
   plus a :quads sequence of remaining keys?"
  [env nquads]
  (let [spoilers (annotation-spoilers nquads)]
    (->> nquads
         (group-by second)
         (filter (partial apply is-collapsible-annotation? spoilers))
         (map #(process-annotation env (second %))))))

(defn process-subject
  [env annotation-map subject nquads]
  (concat
   [{:block-type :SUBJECT_BLOCK
     :subject subject}]
   (->> nquads
        (mapcat (partial nquad->blocks env annotation-map))
        (remove nil?))))

; Given a sequence of nquads for a graph
; first generate annotations
; then normal subjects

(defn process-graph
  [env graph nquads]
  ;(println "TREE" (tree nquads))
  (let [annotations (process-annotations env nquads)
        annotation-subjects (set (map :subject annotations))
        annotation-map
        (->> annotations
             (map (juxt :annotation-target identity))
             (reduce (fn [c [t b]] (update-in c [t] (fnil conj #{}) b)) {}))]
    (concat
     (when graph
       [{:block-type :GRAPH_BLOCK
         :graph graph}])
     (->> nquads
          (remove #(contains? annotation-subjects (second %)))
          (group-by second)
          (mapcat (partial apply process-subject env annotation-map))))))

(defn process-graphs
  "Given a sequence of nquads,
   process each graph
   and return a sequence of blocks."
  [env nquads]
  (->> nquads
       (group-by first)
       (mapcat (partial apply process-graph env))
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
  (if (util/starts-with? subject "_:")
    subject
    (str "<" subject ">")))

(defn object->string
  [object datatype]
  (cond
    (or (nil? datatype) (= "PLAIN" datatype)) (str "\"" object "\"")
    (= "LINK" datatype) (subject->string object)
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
  (assoc
   env
   :blocks
   (process-graphs
    (assoc env :source source :line 1)
    (map nquad-string->nquad lines))))
