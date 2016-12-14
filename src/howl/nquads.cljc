(ns howl.nquads
  "Convert HOWL to and from NQuads."
  (:require [clojure.string :as string]
            clojure.set
            [instaparse.core :as insta]
            [howl.core :as core]
            [howl.link :as link]
            [howl.util :as util]))

;; ## NQuads
;
; NQuads is a line-based concrete syntax for RDF.
; Each line consists of a subject, predicate, object, and optional graph.
; The subject is an absolute IRI or blank node.
; The predicate and graph are always absolute IRIs.
; The object can be: an IRI, a blank node, or a literal.
; Literals are quoted strings with an optional language tag or type IRI.
; The graph is optional, and comes in the final position of the string.
;
; Example:
; <http://example.com/subject> <http://www.w3.org/2000/01/rdf-schema#label> "This is a label." <http://example.com/graph> .
;
; We represent NQuads as vectors of strings:
; [graph subject predicate object datatype]

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

; ## Blank Nodes
;
; HOWL uses random blank nodes,
; but sometimes we want them to be predictable.

(defn sequential-blank-nodes
  "Given a sequence of NQuads,
   return a sequence of NQuads with sequential blank nodes."
  [nquads]
  (let [counter (atom 0)]
    (->> nquads
         (reduce
          (fn [coll [g s p o d]]
            (let [[coll s] (link/replace-blank coll s "LINK")
                  [coll o] (link/replace-blank coll o d)]
              (update-in coll [:nquads] conj [g s p o d])))
          {:counter counter
           :nquads []})
         :nquads)))

; ## NQuads to Strings
;
; To convert an Nquad vector back to an NQuad string,
; set just have to wrap IRIs and literals properly.

(defn subject->string
  [subject]
  (if (link/blank? subject)
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
  [[subject predicate object datatype]]
  (nquad->nquad-string [nil subject predicate object datatype]))

; ## Blocks to NQuads
;
; Converting blocks to NQuads is straightforward.
; We're only interested in HOWL statement blocks.
; If the statement is an annotation,
; then we generate five nquad vectors.
; Otherwise we just generate one nquad vector.

(defn format-object
  [object-string]
  (-> object-string
      (string/replace "\n" "\\n")
      (string/replace "\"" "\\\"")))

(def rdf:type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
(def owl:Axiom "http://www.w3.org/2002/07/owl#Axiom")
(def annSource "http://www.w3.org/2002/07/owl#annotatedSource")
(def annProperty "http://www.w3.org/2002/07/owl#annotatedProperty")
(def annTarget "http://www.w3.org/2002/07/owl#annotatedTarget")
(def annPredicates #{rdf:type annSource annProperty annTarget})

(defn annotation-nquads
  "Given a subject (blank node label) and a vector for the annotation target,
   return four NQuads representing the OWL annotation axiom
   without its content."
  [subject [source property target target-datatype]]
  [[nil subject rdf:type owl:Axiom "LINK"]
   [nil subject annSource source "LINK"]
   [nil subject annProperty property "LINK"]
   [nil subject annTarget (format-object target) target-datatype]])

(defmulti object->nquads
  "Given a format IRI and the :object of a block,
   return the pair of the object for this NQuad
   and a sequence of zero or more NQuad vectors."
  (fn [graph datatypes object] (vec (take 2 datatypes))))

(defmethod object->nquads :default
  [graph datatypes object]
  [(format-object object) []])

(defmethod object->nquads ["LINK"]
  [graph datatypes object]
  [object []])

(defmulti block->nquads
  "Given a (fully expanded) block,
   return a sequence of zero or more NQuad vectors."
  :block-type)

(defmethod block->nquads :default
  [block]
  [])

(defmethod block->nquads :STATEMENT_BLOCK
  [{:keys [annotation-target graph subject predicate object datatypes]}]
  (let [datatype (first datatypes)
        [object nquads] (object->nquads graph datatypes object)]
    (vec
     (concat
      (when annotation-target (annotation-nquads subject annotation-target))
      [[graph subject predicate object datatype]]
      nquads))))

; ## Nquads to Blocks
;
; The Nquads to Block conversion is a little trickier.
; One NQuad more-or-less corresponds to one HOWL statement block.
; We also have to make sure to add graph and subject blocks.
; The hardest part is handling OWL annotations.

(defn nquad->block
  "Given an NQuad, return a HOWL statement block."
  [[graph subject predicate object datatype]]
  {:block-type :STATEMENT_BLOCK
   :graph graph
   :subject subject
   :predicate predicate
   :object object
   :datatypes [datatype]})

; ### Annotations
;
; The hardest part of NQuads to HOWL conversion is handling OWL Annotations.
; An OWL annotation has a blank node as subject and five nquads:
;
; - the rdf:type is owl:Axiom
; - the owl:annotatedSource is the subject of the annotated NQuad
; - the owl:annotatedProperty is the predicate of the annotated NQuad
; - the owl:annotatedTarget is the object (with datatype) of the annotated NQuad
; - then there's one normal NQuad that is the content of the annotation.
;
; We can't assume that these NQuads occur in any particular order,
; so we end up collecting all the NQuads into a nested data structure
; (see below).
;
; We try to collapse the five NQuads into a single HOWL STATEMENT_BLOCK
; that we insert after the target statement.
; We don't collapse annotations that are referred to by non-annotations.
; we apply all these rules to determine
; whether we will collapse the annotation into a single HOWL statement:
;
; - subject is a blank node
; - the annotatedSource is a subject in this graph
; - subject only occurs as the object of other annotations
; - all the annotations on this subject are collapsible (WARN: recursive)
; - subject has exactly five NQuads
; - the rdf:type is owl:Axiom
; - three predicates are present: annotatedSource, annotatedProperty, annotatedTarget
;
; Otherwise we treat the annotation as a normal subject
; with a HOWL subject block
; and five HOWL statement blocks for its five NQuads.

(defn is-collapsible-annotation?
  "Given a subject map and a subject to check,
   return true only if we can safely collapse this annotation
   into a single HOWL statement block."
  [subject-map subject]
  (let [predicate-map (get subject-map subject)]
    (and
     (link/blank? subject)
     (contains? subject-map (get-in predicate-map [annSource 0 :object]))
     (->> (get-in subject-map [:blank-object-uses subject])
          (map second)
          (remove #(= annSource %))
          count
          (= 0))
     (->> (get-in subject-map [:blank-object-uses subject])
          (map first)
          (map (partial is-collapsible-annotation? subject-map))
          and)
     (= 5 (count predicate-map))
     (->> predicate-map vals (apply concat) count (= 5))
     (= owl:Axiom (get-in predicate-map [rdf:type 0 :object]))
     (clojure.set/subset? annPredicates (set (keys predicate-map))))))

(defn process-annotation
  "Given a graph IRI, a subject map, and a subject,
   return a HOWL annotation statement block."
  [graph subject-map subject]
  (let [predicate-map (get subject-map subject)
        predicate
        (-> predicate-map
            (dissoc rdf:type annSource annProperty annTarget)
            keys
            first)
        {:keys [object datatype]} (get-in predicate-map [predicate 0])]
    (assoc
     (nquad->block [graph subject predicate object datatype])
     :annotation-target
     [(get-in predicate-map [annSource   0 :object])
      (get-in predicate-map [annProperty 0 :object])
      (get-in predicate-map [annTarget   0 :object])
      (get-in predicate-map [annTarget   0 :datatype])])))

(defn process-annotations
  "Given a graph IRI, and a subject map,
   return sequence of HOWL annotation statement blocks."
  [graph subject-map]
  (->> subject-map
       keys
       (filter (partial is-collapsible-annotation? subject-map))
       (map (partial process-annotation graph subject-map))))

(defn insert-annotations
  "Given depth integer, the annotation-map, and a block,
   return a sequence starting with
   the block with the :arrows key updated for the depth,
   and recursively including any annotations on that block."
  [depth annotation-map {:keys [subject predicate object datatypes] :as block}]
  (let [annotations (get annotation-map [subject predicate object (first datatypes)])]
    (concat
     [(assoc block :arrows (apply str (repeat depth ">")))]
     (->> annotations
          (sort-by :object)
          (mapcat (partial insert-annotations (inc depth) annotation-map))))))

; ### Processing NQuads
;
; Now that annotations are out of the way,
; converting NQUads to HOWL is pretty straightforward.
; We start by reducing the sequence of NQuad strings
; to a nested map:
;
; 1. a graph-map maps graph IRI strings to subject-maps
; 2. a subject-map maps subject IRI strings to predicate-maps
; 3. a predicate-map maps predicate-IRIs to object-lists
; 4. an object-list is a sequence of object-maps
; 5. an object-map contains :object and :datatype IRI strings,
;    and other information about the source, line, string, etc.
;
; Then we iterate over graphs, subjects, predicates, and objects,
; generating a concatenated sequence of HOWL blocks.

(defn make-graph-map
  "Given a source file name and a sequence of NQuad lines,
   parse the lines and return a nested map:
   graph, subject, predicate, to sequence of object maps
   with line and source information.
   Each graph map will also have a special :blank-object-uses map
   from blank nodes to lists of the subject-predicate pairs for which
   that blank node occurs in object position."
  [source lines]
  (reduce-kv
   (fn [coll line string]
     (let [[graph subject predicate object datatype] (nquad-string->nquad string)]
       (update-in
        (if (link/blank? object)
          (update-in
           coll
           [graph :blank-object-uses object]
           (fnil conj [])
           [subject predicate])
          coll)
        [graph subject predicate]
        (fnil conj [])
        {:object object
         :datatype datatype
         :source source
         :line line
         :string string})))
   (sorted-map)
   (vec lines)))

(defn nquad->blocks
  "Given the annotation-map and an nquad vector,
   return a sequence of HOWL statement blocks."
  [annotation-map nquad]
  (insert-annotations 0 annotation-map (nquad->block nquad)))

(defn process-object
  [annotation-map graph subject predicate {:keys [object datatype]}]
  (nquad->blocks annotation-map [graph subject predicate object datatype]))

(defn process-predicate
  [annotation-map graph subject predicate objects]
  (mapcat
   (partial process-object annotation-map graph subject predicate)
   objects))

(defn process-subject
  [annotation-map graph subject predicate-map]
  (concat
   [{:block-type :SUBJECT_BLOCK
     :subject subject}]
   (->> predicate-map
        (mapcat
         (partial apply process-predicate annotation-map graph subject))
        (remove nil?))))

(defn process-graph
  [graph subject-map]
  (let [annotations (process-annotations graph subject-map)
        annotation-subjects (set (map :subject annotations))
        annotation-map
        (->> annotations
             (map (juxt :annotation-target identity))
             (reduce (fn [c [t b]] (update-in c [t] (fnil conj #{}) b)) {}))]
    (concat
     (when graph
       [{:block-type :GRAPH_BLOCK
         :graph graph}])
     (->> subject-map
          (remove #(keyword? (key %)))
          (remove #(contains? annotation-subjects (key %)))
          (mapcat (partial apply process-subject annotation-map graph))))))

(defn update-datatypes
  "Given an environment and a block,
   try to update the :format."
  [env {:keys [block-type predicate datatypes] :as block}]
  (if (= :STATEMENT_BLOCK block-type)
    (let [predicate-label (get-in env [:iri-label predicate])
          predicate-datatypes (get-in env [:labels predicate-label :datatypes] ["PLAIN"])
          use-default-datatypes (= (first datatypes) (first predicate-datatypes))]
      (assoc
       block
       :use-default-datatypes use-default-datatypes
       :datatypes (if use-default-datatypes predicate-datatypes datatypes)))
    block))

(defn update-content
  "Given an environment and a block,
   if the block has a :datatypes and :object,
   update it with :content."
  [env {:keys [block-type datatypes object] :as block}]
  (if (= :STATEMENT_BLOCK block-type)
    (assoc
     block
     :content
     (if (= "LINK" (first datatypes))
       (link/iri->name env object)
       (-> object
           (string/replace "\\n" "\n")
           (string/replace "\\\"" "\""))))
    block))

(defn lines->blocks
  "Given an environment and a sequence of lines,
   return a sequence of HOWL block maps."
  [{:keys [source] :or {source "interactive"} :as env} lines]
  (->> lines
       (make-graph-map source)
       (mapcat (partial apply process-graph))
       (map (partial update-datatypes env))
       (map (partial core/block-iris->names env))
       (map (partial update-content env))))
