(ns howl.nquads
  "Render parsed HOWL to N-Quads."
  (:require [clojure.string :as string]
            [clojure.set]
            [edn-ld.core :as edn-ld]
            [howl.util :as util]
            [howl.core :as core]))

(def rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(def rdfs "http://www.w3.org/2000/01/rdf-schema#")
(def owl "http://www.w3.org/2002/07/owl#")
(def obo "http://purl.obolibrary.org/obo/")
(def plain-literal "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral")
(def rdfs-label "http://www.w3.org/2000/01/rdf-schema#label")
(def xsd-string "http://www.w4.org/2001/XMLSchema#string")


;; We convert HOWL to EDN-LD, then to N-Quads.
;; EDN-LD quads are vectors [g s p o],
;; where g, s, and p are absolute IRI strings or blank node strings,
;; and o is either a IRI/blank string or a map
;; with :value, :lang, and :type keys.

(defn convert-object
  "Given a state map with a :block that is a literal or link,
   return an object: an object map for a literal or an IRI string for a link."
  [{:keys [block] :as state}]
  (case (:block-type block)
    :LITERAL_BLOCK
    (merge
     (get-in state [:iri-type (get-in state [:block :predicate 1])])
     (select-keys block [:value :lang :type]))
    :LINK_BLOCK
    (get-in state [:block :object 1])
    ;else
    nil))

(defn convert-single-statement
  "Given a state map with :current-subject and :block keys,
   where the :block has no :arrows,
   form a single quad,
   and return the update state."
  [state]
  (let [g (:current-graph state)
        s (:current-subject state)
        p (get-in state [:block :predicate 1])
        o (convert-object state)
        statement [g s p o]]
   (assoc state :quads [statement] :statements [statement])))

(defn convert-annotation
  "Given a state map with :current-subject and :block keys,
   where the :block has no :arrows,
   form a single quad,
   and return the updated state."
  [state]
  (let [g (:current-graph state)
        s (str "_:b" (get state :blank-node-count 0))
        p (get-in state [:block :predicate 1])
        o (convert-object state)
        statement [g s p o]
        a (count (get-in state [:block :arrows]))
        annotated (get-in state [:statements (dec a)])]
   (-> state
       (update :blank-node-count (fnil inc 0))
       (assoc
        :statements
        (conj (vec (take a (:statements state))) statement))
       (assoc
        :quads
        [[g s (str rdf "type")              (str owl "Axiom")]
         [g s (str owl "annotatedSource")   (get annotated 1)]
         [g s (str owl "annotatedProperty") (get annotated 2)]
         [g s (str owl "annotatedTarget")   (get annotated 3)]
         statement]))))

(defn convert-statement
  [{:keys [current-graph current-subject block statements] :as state}]
  (if (string/blank? (:arrows block))
   (convert-single-statement state)
   (convert-annotation state)))


;; The most complex conversion is the Manchester syntax.
;; The expression is a tree,
;; but we represent it in very flat RDF graph.

(defn filter-ce
  "Given a parse vector,
   remove the elements that don't matter for Manchester."
  [parse]
  (->> parse
       (filter vector?)
       (remove #(= :SPACE (first %)))
       (remove #(= :SPACES (first %)))))

(declare convert-expression)

;; This is very imperative code that is somewhat awkward in Clojure.
;; The code is a little cleaner if the nodes are rendered in reverse order,
;; but this way the results are easier to read.

;; First get a new blank node for the complementOf class.
;; Then clear the state (s2) and render the negated class expression.
;; Update that result (s3) with quads for the complement and s3.

(defn convert-negation
  "Given a state and a parse vector,
   convert the first elements in the parse vector
   and update the state with new quads for the
   complement class and first element."
  [s1 parse]
  (let [g  (:current-graph s1)
        bs (get s1 :blank-node-count 0)
        b1 (str "_:b" (+ bs 1))
        s2 (assoc s1 :blank-node-count (+ bs 1) :quads [])
        s3 (convert-expression s2 (->> parse filter-ce first))]
    (assoc
     s3
     :node b1
     :quads
     (concat
      (:quads s1)
      [[g b1 (str rdf "type")         (str owl "Class")]
       [g b1 (str owl "complementOf") (:node s3)]]
      (:quads s3)))))

;; Like convert-negation, but with two elements:
;; the object property expression and the class expression.

(defn convert-restriction
  "Given a state, a parse vector, and a predicate IRI string,
   convert the first and second elements in the parse vector
   and update the state with new quads for the:
   restriction class, and first and second elements."
  [s1 parse predicate]
  (let [g  (:current-graph s1)
        bs (get s1 :blank-node-count 0)
        b1 (str "_:b" (+ bs 1))
        s2 (assoc s1 :blank-node-count (+ bs 1) :quads [])
        s3 (convert-expression s2 (->> parse filter-ce first))
        s4 (convert-expression s3 (->> parse filter-ce second))]
    (assoc
     s4
     :node b1
     :quads
     (concat
      (:quads s1)
      [[g b1 (str rdf "type")       (str owl "Restriction")]
       [g b1 (str owl "onProperty") (:node s3)]
       [g b1 predicate              (:node s4)]]
      (:quads s4)))))

;; Unions and intersections are trickier because they include an RDF list.
;; First generate some blank nodes for the combination class and RDF list.
;; Then clear the state, and render the first and second elements,
;; storing the results in new states.
;; Then update the state resulting from the second element (s4)
;; with previous quads,
;; the combination element and RDF list,
;; and the quads from s4, which include the first and second elements.

(defn convert-combination
  "Given a state, a parse vector, and a predicate IRI string,
   render the first and second elements in the parse vector
   and update the state with new quads for the:
   combination class (i.e. unionOf, intersectionOf),
   RDF list of elements,
   first and second elements."
  [s1 parse predicate]
  (let [g  (:current-graph s1)
        bs (get s1 :blank-node-count 0)
        b1 (str "_:b" (+ bs 1))
        b2 (str "_:b" (+ bs 2))
        b3 (str "_:b" (+ bs 3))
        s2 (assoc s1 :blank-node-count (+ bs 3) :quads [])
        s3 (convert-expression s2 (->> parse filter-ce first))
        s4 (convert-expression s3 (->> parse filter-ce second))]
    (assoc
     s4
     :node b1
     :quads
     (concat
      (:quads s1)
      [[g b1 (str rdf "type")  (str owl "Class")]
       [g b1 predicate         b2]
       [g b2 (str rdf "first") (:node s3)]
       [g b2 (str rdf "rest")  b3]
       [g b3 (str rdf "first") (:node s4)]
       [g b3 (str rdf "rest")  (str rdf "nil")]]
      (:quads s4)))))

(defn convert-expression
  "Given a state map, and a parse vector with a Manchester expression
   return an updated state with quads for the expression."
  [state parse]
  (case (first parse)
    :CLASS_EXPRESSION
    (convert-expression state (->> parse filter-ce first))

    :NEGATION
    (convert-negation state parse)

    :DISJUNCTION
    (convert-combination state parse (str owl "unionOf"))

    :CONJUNCTION
    (convert-combination state parse (str owl "intersectionOf"))

    :OBJECT_PROPERTY_EXPRESSION
    (convert-expression state (->> parse filter-ce first))

    :SOME
    (convert-restriction state parse (str owl "someValuesFrom"))

    :ONLY
    (convert-restriction state parse (str owl "allValuesFrom"))

    :NAME
    (convert-expression state (->> parse filter-ce first))

    :ABSOLUTE_IRI
    (assoc state :node (second parse))

    ; else
    state))

(defn convert-quads
  "Given a state (usually the output of expand-names),
   if it has a :block key,
   return an updated state with a :quads vector (maybe nil)
   with entries in EDN-LD Quads format."
  [state]
  (try
   (if-let [block (:block state)]
     (case (:block-type block)
       :LITERAL_BLOCK
       (convert-statement state)

       :LINK_BLOCK
       (convert-statement state)

       :EXPRESSION_BLOCK
       (let [new-state (convert-expression state (:expression block))
             g (:current-graph state)
             s (:current-subject state)
             p (get-in state [:block :predicate 1])
             o (:node new-state)
             statement [g s p o]]
         (-> new-state
             (dissoc :node)
             (assoc :statements [statement])
             (assoc :quads (concat [statement] (:quads new-state)))))

       ; else
       (dissoc state :quads))
     (dissoc state :quads))
   (catch #?(:clj Exception :cljs :default) e
     (util/throw-exception e (core/locate state)))))

(defn line-to-quads-transducer
  "Given a processing function
   (that takes a state map and a line, and returns updated state with a :quads key),
   a starting state, and a sequence of lines,
   return stateful transducer that emits a sequence of quads."
  [processing-function starting-state]
  (fn
    [xf]
    (let [state (volatile! starting-state)]
      (fn
        ([] (xf))
        ([result] (apply xf result (get (processing-function @state "EOL") :quads [])))
        ([result line]
         (let [new-state (processing-function @state line)]
           (when (:errors new-state)
             (util/throw-exception
              (string/join " " (:errors new-state))
              (core/locate new-state)))
           (vreset! state new-state)
           (if (:quads new-state)
             (apply xf result (:quads new-state))
             result)))))))

(defn lines-to-quads
  "Given a processing function
   (that takes a state map and a line, and returns updated state with a :quads key),
   a starting state, and a sequence of lines,
   return a sequence of quads"
  [processing-function starting-state lines]
  (transduce
   (line-to-quads-transducer processing-function starting-state)
   conj
   []
   lines))


;; It's easy to convert an EDN-LD quad to an N-Quad:

(defn wrap-iri
  "Return an IRI string wrapped in arrows (<iri>)
   or a blank node string unchanged."
  [iri]
  (cond
   (not (string? iri)) iri
   (.startsWith iri "_:") iri
   :else (str "<" iri ">")))

(defn object-to-string
  "Given an EDN-LD object, return an N-Quads literal or IRI."
  [o]
  (let [value (-> (get o :value "NULL")
                  (string/replace "\n" "\\n")
                  (string/replace "\"" "\\\""))]
    (cond
     (and (map? o) (:lang o)) (str "\"" value "\"@" (:lang o))
     (and (map? o) (:type o)) (str "\"" value "\"^^" (wrap-iri (:type o)))
     (map? o) (str "\"" value "\"")
     (string? o) (wrap-iri o)
     :else nil)))

(defn quad-to-string
  "Given an EDN-LD Quad vector return an N-Quads string."
  [[g s p o]]
  (->> [(wrap-iri s)
        (wrap-iri p)
        (object-to-string o)
        (when g (wrap-iri g))
        "."]
       (remove nil?)
       (string/join " ")))



(defn render-statement
  "Given an arrow string (for depth),
   a predicate IRI, and an object,
   return a HOWL statement block map."
  [arrows predicate-iri object]
  (merge
   {:arrows
    (if (string/blank? arrows)
      arrows
      (str arrows " "))
    :predicate [:ABSOLUTE_IRI predicate-iri]
    :eol "\n"}
   (cond
    (map? object)
    (assoc object :block-type :LITERAL_BLOCK)
    (vector? object)
    {:block-type :EXPRESSION_BLOCK
     :expression object}
    :else
    {:block-type :LINK_BLOCK
     :object [:ABSOLUTE_IRI object]})))

(declare render-predicates)

(defn render-statements
  "Given an arrow string (for depth),
   a predicate IRI, and an object map,
   return a sequence of HOWL statement block maps."
  [arrows predicate-iri object-map]
  (concat
   [(render-statement arrows predicate-iri (first object-map))]
   (render-predicates (str arrows ">") (second object-map))))

(defn render-predicate
  "Given an arrow string (for depth),
   a predicate IRI, and a sequence of object maps,
   return a sequence of HOWL statement block maps."
  [arrows predicate-iri object-list]
  (mapcat (partial render-statements arrows predicate-iri) object-list))

(def predicate-sort-list
  [(str rdfs "label")
   (str rdf "type")
   (str obo "IAO_0000115") ; definition
   (str obo "IAO_0000119") ; definition source
   (str obo "IAO_0000112") ; example of usage
   (str owl "equivalentClass")
   (str rdfs "subClassOf")
   (str obo "IAO_0000118") ; alternative term
   ])

(def predicate-sort
  (->> predicate-sort-list
       (map-indexed (fn [i v] [v i]))
       (into {})))

(defn render-predicates
  "Given an arrow string (for depth), and a predicate map,
   return a sequence of HOWL statement block maps."
  [arrows predicate-map]
  (->> predicate-map
       keys
       (map (juxt #(get predicate-sort % 100) identity))
       sort
       (map second)
       (mapcat #(render-predicate arrows % (get predicate-map %)))))

(defn render-subject
  "Given a subject IRI, and a predicate map for that subject,
   return a sequence of HOWL block maps."
  [subject-iri predicate-map]
  (concat
   [{:block-type :SUBJECT_BLOCK
     :subject [:ABSOLUTE_IRI subject-iri]
     :eol "\n"}]
   (render-predicates "" predicate-map)))


(defn expression-deps
  "Given a subject map,
   return a map from OWL annotation IRIs to sets of their dependencies."
  [subject-map]
  (->> subject-map
       keys
       (filter #(get-in subject-map [% (str rdf "type") (str owl "Restriction")]))
       (filter #(get-in subject-map [% (str owl "onProperty")]))
       (filter #(get-in subject-map [% (str owl "someValuesFrom")]))
       (map
        (fn [node]
          [node
           #{(ffirst (get-in subject-map [node (str owl "onProperty")]))}
           (ffirst (get-in subject-map [node (str owl "someValuesFrom")]))]))
       (into {})))

(defn get-expression
  "Given a subject map and a node,
   if that node exactly matches an OWL pattern,
   return the subject map with that node's contents replaced
   by an OWL logic vector."
  [predicate-map]
  (cond
   ; some
   (and
    (get-in predicate-map [(str rdf "type") (str owl "Restriction")])
    (get-in predicate-map [(str owl "onProperty")])
    (get-in predicate-map [(str owl "someValuesFrom")])
    (= 3 (count predicate-map)))
   [:CLASS_EXPRESSION
    "("
    [:SOME
     [:OBJECT_PROPERTY_EXPRESSION
      (ffirst (get predicate-map (str owl "onProperty")))]
     [:SPACE " "]
     "some"
     [:SPACE " "]
     (ffirst (get predicate-map (str owl "someValuesFrom")))]
     ")"]

   ; only
   (and
    (get-in predicate-map [(str rdf "type") (str owl "Restriction")])
    (get-in predicate-map [(str owl "onProperty")])
    (get-in predicate-map [(str owl "allValuesFrom")])
    (= 3 (count predicate-map)))
   [:CLASS_EXPRESSION
    "("
    [:ONLY
     [:OBJECT_PROPERTY_EXPRESSION
      (ffirst (get predicate-map (str owl "onProperty")))]
     [:SPACE " "]
     "only"
     [:SPACE " "]
     (ffirst (get predicate-map (str owl "allValuesFrom")))]
     ")"]

   ; not
   (and
    (get-in predicate-map [(str rdf "type") (str owl "Class")])
    (get-in predicate-map [(str owl "complementOf")])
    (= 2 (count predicate-map)))
   [:CLASS_EXPRESSION
    "("
    [:NEGATION
     "not"
     [:SPACE " "]
     (ffirst (get predicate-map (str owl "complementOf")))]
    ")"]

   ; and
   (and
    (get-in predicate-map [(str rdf "type") (str owl "Class")])
    (get-in predicate-map [(str owl "intersectionOf")])
    (= 2 (count predicate-map)))
   [:CLASS_EXPRESSION
    "("
    [:CONJUNCTION
     (ffirst (get predicate-map (str owl "intersectionOf")))]
    ")"]

   ; or
   (and
    (get-in predicate-map [(str rdf "type") (str owl "Class")])
    (get-in predicate-map [(str owl "unionOf")])
    (= 2 (count predicate-map)))
   [:CLASS_EXPRESSION
    "("
    [:DISJUNCTION
     (ffirst (get predicate-map (str owl "unionOf")))]
    ")"]

   ; RDF list
   (and
    (get-in predicate-map [(str rdf "first")])
    (get-in predicate-map [(str rdf "rest")])
    (= 2 (count predicate-map)))
   (if (= (ffirst (get-in predicate-map [(str rdf "rest")]))
          "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")
     [(ffirst (get-in predicate-map [(str rdf "first")]))]
     [(ffirst (get-in predicate-map [(str rdf "first")]))
      " AND "
      (ffirst (get-in predicate-map [(str rdf "rest")]))])

   :else
   nil))

(defn merge-expression
  "Given a subject map and a node,
   if the value of that node is a vector (OWL expression)
   walk the subject map and replace the node with its value."
  [subject-map [node value]]
  (if (vector? value)
    (->> subject-map
         (map
          (fn [[k v]]
            [k (clojure.walk/postwalk-replace {node value} v)]))
         (into {}))
    subject-map))

(defn process-expressions
  [subject-map]
  (let [expressions
        (->> subject-map
             (map (fn [[k v]] [k (get-expression v)]))
             (remove (comp nil? second))
             (into {}))]
    (->> (apply dissoc subject-map (keys expressions))
         (map
          (fn [[k v]]
            [k
             (clojure.walk/prewalk
              (fn [x] (get expressions x x))
              v)]))
         (into {}))))

(defn get-annotation-deps
  "Given a subject map,
   return a map from OWL annotation IRIs to sets of their annotatedSource IRIs."
  [subject-map]
  (->> subject-map
       keys
       (filter #(get-in subject-map [% (str rdf "type") (str owl "Axiom")]))
       (filter #(get-in subject-map [% (str owl "annotatedSource")]))
       (filter #(get-in subject-map [% (str owl "annotatedProperty")]))
       (filter #(get-in subject-map [% (str owl "annotatedTarget")]))
       (map
        (fn [node]
          [node
           #{(ffirst (get-in subject-map [node (str owl "annotatedSource")]))}]))
       (into {})))

(defn render-annotation
  "Given a subject map and the IRI of an OWL annotation,
   return an updated subject map with the OWL annotation
   'folded in' to the object map of the appropriate annotatedSource."
  [subject-map node]
  (assoc-in
   (dissoc subject-map node)
   [(ffirst (get-in subject-map [node (str owl "annotatedSource")]))
    (ffirst (get-in subject-map [node (str owl "annotatedProperty")]))
    (ffirst (get-in subject-map [node (str owl "annotatedTarget")]))]
   (dissoc
    (get subject-map node)
    (str rdf "type")
    (str owl "annotatedSource")
    (str owl "annotatedProperty")
    (str owl "annotatedTarget"))))

(defn depth
  "Given a map from node to node, and target node,
   return the 'depth' of the target in the collection,
   i.e. the number of steps before no further ancestor can be found."
  [coll node]
  (loop [ancestry #{node}]
    (let [expanded (apply clojure.set/union ancestry (vals (select-keys coll ancestry)))]
      (if (= expanded ancestry)
        (count ancestry)
        (recur expanded)))))

(defn process-annotations
  [subject-map]
  (let [annotation-deps (get-annotation-deps subject-map)]
    (->> annotation-deps
         keys
         (sort-by (partial depth annotation-deps) >)
         (reduce render-annotation subject-map))))

(defn render-subjects
  "Given a subject map,
   return a sequnce of HOWL block maps for the subjects."
  [subject-map]
  (let [subject-map
        (->> subject-map
             process-annotations
             process-expressions)]
    (->> subject-map
         keys
         ; TODO: better subject sorting
         (map (juxt #(if (.startsWith % "_:") 2 1) identity))
         sort
         (map second)
         (mapcat #(render-subject % (get subject-map %))))))

(def arq-default-graph
 "urn:x-arq:DefaultGraphNode")

(defn render-named-graph
  "Given a named graph IRI, and the subject map for that named graph,
   return a sequence of HOWL block maps for that named graph."
  [graph-iri subject-map]
  (concat
   [{:block-type :GRAPH_BLOCK
     :graph [:ABSOLUTE_IRI graph-iri]
     :eol "\n"}]
   (render-predicates "" (get subject-map graph-iri))
   (render-subjects (dissoc subject-map graph-iri))))

(defn render-labels
  [quads]
  (->> quads
       (filter (fn [[g s p o d l]] (= p (str rdfs "label"))))
       (filter (fn [[g s p o d l]] (map? o)))
       (remove (fn [[g s p o d l]] (.startsWith s "_:")))
       (map (fn [[g s p o d l]]
              {:block-type :LABEL_BLOCK
               :identifier [:ABSOLUTE_IRI s]
               :label (:value o)
               :eol "\n"}))))

(defn render-graphs
  "Given a graph map,
   return a sequence of HOWL block maps for those graphs."
  [graph-map]
  (concat
   ; TODO: better way to get default graph
   (render-subjects (get graph-map nil))
   (->> graph-map
        keys
        (remove nil?)
        sort
        (mapcat #(render-named-graph % (get graph-map %))))))

(defn graphify
  "Given a sequence of Quads, return a GraphMap."
  [quads]
  (reduce
   (fn [coll [graph subject predicate object datatype lang]]
     (assoc-in
      coll
      [graph
       subject
       predicate
       (if datatype
         (edn-ld/literal object datatype lang)
         object)]
      {}))
   nil
   quads))

(defn triplify
  "Given a sequence of Triples, return a SubjectMap."
  [triples]
  (reduce
   (fn [coll [subject predicate object datatype lang]]
     (assoc-in
      coll
      [subject
       predicate
       (if datatype
         (edn-ld/literal object datatype lang)
         object)]
      {}))
   nil
   triples))

(defn quads-to-howl
  "Given a sequence of quads,
   return a sequence of HOWL block maps."
  [quads]
  (concat
   (render-labels quads)
   (render-graphs (graphify quads))))

(defn triples-to-howl
  "Given a sequence of triples,
   return a sequence of HOWL block maps."
  [quads]
  (concat
   (render-labels quads)
   (render-subjects (triplify quads))))
