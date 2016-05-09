(ns howl.nquads
  "Render parsed HOWL to N-Quads."
  (:require [clojure.string :as string]
            [clojure.set]
            [edn-ld.core :as edn-ld]
            [howl.util :as util]
            [howl.core :refer [resolve-name] :as core]))

(def rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(def rdfs "http://www.w3.org/2000/01/rdf-schema#")
(def owl "http://www.w3.org/2002/07/owl#")
(def plain-literal "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral")
(def rdfs-label "http://www.w3.org/2000/01/rdf-schema#label")
(def xsd-string "http://www.w4.org/2001/XMLSchema#string")

(defn iri
  [& parts]
  (str "<" (string/join "" parts) ">"))

(defn unwrap-iri
  [iri]
  (string/replace iri #"^<|>$" ""))

(defn format-literal
  "Given a state map and a block map for a LITERAL_BLOCK,
   return a concrete literal string."
  [state {:keys [predicate content language datatype] :as block}]
  (let [predicate-iri (resolve-name state block predicate)
        language (or language (get-in state [:types-language predicate-iri]))
        datatype (when datatype (resolve-name state block datatype))
        datatype (or datatype (get-in state [:types-datatype predicate-iri]))
        content  (string/replace content "\n" "\\n")]
    (cond
      language
      (util/format "\"%s\"%s" content language)
      datatype
      (util/format "\"%s\"^^<%s>" content datatype)
      :else
      (util/format "\"%s\"" content))))

(defn filter-ce
  "Given a parse vector,
   remove the elements that don't matter for Manchester."
  [parse]
  (->> parse
       (filter vector?)
       (remove #(= :MN_SPACE (first %)))
       (remove #(= :SPACES (first %)))))

(declare render-expression)

;; This is very imperative code, which is somewhat awkward in Clojure.
;; It's a little cleaner if the nodes are rendered in reverse order,
;; but this way the results are easier to read.

;; First get a new blank node for the complementOf class.
;; Then clear the state (s2) and render the negated class expression.
;; Update that result (s3) with quads for the complement and s3.

(defn render-negation
  "Given a state, a block, and a parse vector,
   render the first elements in the parse vector
   and update the state with new quads for the
   complement class and first element."
  [s1 block parse]
  (let [g  (:graph s1)
        bs (get s1 :blank-node-count 0)
        b1 (str "_:b" (+ bs 1))
        s2 (assoc s1 :blank-node-count (+ bs 1) :quads [])
        s3 (render-expression s2 block (->> parse filter-ce first))]
    (assoc
     s3
     :node b1
     :quads
     (concat
      (:quads s1)
      [[g b1 (iri rdf "type")         (iri owl "Class")]
       [g b1 (iri owl "complementOf") (:node s3)]]
      (:quads s3)))))

;; Like render-negation, but with two elements:
;; the object property expression and the class expression.

(defn render-restriction
  "Given a state, a block, a parse vector, and a predicate IRI string,
   render the first and second elements in the parse vector
   and update the state with new quads for the:
   restriction class, and first and second elements."
  [s1 block parse predicate]
  (let [g  (:graph s1)
        bs (get s1 :blank-node-count 0)
        b1 (str "_:b" (+ bs 1))
        s2 (assoc s1 :blank-node-count (+ bs 1) :quads [])
        s3 (render-expression s2 block (->> parse filter-ce first))
        s4 (render-expression s3 block (->> parse filter-ce second))]
    (assoc
     s4
     :node b1
     :quads
     (concat
      (:quads s1)
      [[g b1 (iri rdf "type")       (iri owl "Restriction")]
       [g b1 (iri owl "onProperty") (:node s3)]
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

(defn render-combination
  "Given a state, a block, a parse vector, and a predicate IRI string,
   render the first and second elements in the parse vector
   and update the state with new quads for the:
   combination class (i.e. unionOf, intersectionOf),
   RDF list of elements,
   first and second elements."
  [s1 block parse predicate]
  (let [g  (:graph s1)
        bs (get s1 :blank-node-count 0)
        b1 (str "_:b" (+ bs 1))
        b2 (str "_:b" (+ bs 2))
        b3 (str "_:b" (+ bs 3))
        s2 (assoc s1 :blank-node-count (+ bs 3) :quads [])
        s3 (render-expression s2 block (->> parse filter-ce first))
        s4 (render-expression s3 block (->> parse filter-ce second))]
    (assoc
     s4
     :node b1
     :quads
     (concat
      (:quads s1)
      [[g b1 (iri rdf "type")  (iri owl "Class")]
       [g b1 predicate         b2]
       [g b2 (iri rdf "first") (:node s3)]
       [g b2 (iri rdf "rest")  b3]
       [g b3 (iri rdf "first") (:node s4)]
       [g b3 (iri rdf "rest")  (iri rdf "nil")]]
      (:quads s4)))))

(defn render-expression
  "Given a state map, a block map, and a parse vector with a Manchester expression
   return an updated state with quads for the expression."
  [state block parse]
  (case (first parse)
    :MN_CLASS_EXPRESSION
    (render-expression state block (->> parse filter-ce first))

    :MN_NEGATION
    (render-negation state block parse)

    :MN_DISJUNCTION
    (render-combination state block parse (iri owl "unionOf"))

    :MN_CONJUNCTION
    (render-combination state block parse (iri owl "intersectionOf"))

    :MN_OBJECT_PROPERTY_EXPRESSION
    (render-expression state block (->> parse filter-ce first))

    :MN_SOME
    (render-restriction state block parse (iri owl "someValuesFrom"))

    :MN_ONLY
    (render-restriction state block parse (iri owl "allValuesFrom"))

    :MN_NAME
    (render-expression state block (->> parse filter-ce first))

    :MN_LABEL
    (assoc state :node (iri (resolve-name state block [:LABEL (second parse)])))

    :MN_QUOTED_LABEL
    (assoc state :node (iri (resolve-name state block [:LABEL (nth parse 2)])))

    ; else
    state))

; TODO: This function is too imperative -- factor out the volatile.

(defn render-statement
  "Given a state map, a block map, and a concrete object string.
   mutate the state to update the :subjects key,
   and apply the reducing function to all the generated quads."
  [old-state block object]
  (let [{:keys [arrows predicate content]} block
        state     (volatile! old-state)
        graph     (:graph @state)
        subject   (if (string/blank? arrows)
                    (-> @state :subjects first second)
                    (do
                      (vswap! state update-in [:blank-node-count] (fnil inc 0))
                      (str "_:b" (:blank-node-count @state))))
        predicate (iri (resolve-name @state block predicate))]

    ; When this is a HOWL-valid rdfs:label, add it to the map of labels.
    (when (and (string/blank? arrows)
               (= predicate (iri rdfs "label"))
               (core/valid-label? content))
      (vswap! state assoc-in [:label-iri content] (unwrap-iri subject)))

    ; Add this quad to the stack of subjects.
    (vswap! state
            assoc
            :subjects
            (conj (vec (take (count arrows) (:subjects @state)))
                  [graph subject predicate object]))

    [@state
     ; Two cases
     (if (string/blank? arrows)
       ; normal literal node
       [[graph subject predicate object]]
       ; annotation (maybe nested)
       (let [[_ annotatedSource annotatedProperty annotatedTarget]
             (get-in @state [:subjects (dec (count arrows))])]
         [[graph subject (iri rdf "type") (iri owl "Axiom")]
          [graph subject (iri owl "annotatedSource") annotatedSource]
          [graph subject (iri owl "annotatedProperty") annotatedProperty]
          [graph subject (iri owl "annotatedTarget") annotatedTarget]
          [graph subject predicate object]]))]))

(defn render-block
  "Given a state map and a block map,
   return a vector
   with an updated state map,
   and either a sequence of N-Quad vectors to render
   or nil if there are no new N-Quads to render."
  [state block]
  (try
    (case (:block-type block)
      :BASE_BLOCK
      (let [iri (resolve-name state block (:base block))]
       [(assoc state :base iri)
        nil])

      :PREFIX_BLOCK
      (let [iri (resolve-name state block (:prefixed block))]
       [(assoc-in state [:prefix-iri (:prefix block)] iri)
        nil])

      :LABEL_BLOCK
      (let [iri (resolve-name state block (:identifier block))]
        [(assoc-in state [:label-iri (:label block)] iri)
         nil])

      :TYPE_BLOCK
      (let [predicate-iri (resolve-name state block (:predicate block))]
        [(cond
           (:language block)
           (assoc-in
            state
            [:types-language predicate-iri]
            (:language block))
           (:datatype block)
           (assoc-in
            state
            [:types-datatype predicate-iri]
            (resolve-name state block (:datatype block))))
         nil])

      :GRAPH_BLOCK
      (let [graph (when (:graph block)
                    (resolve-name state block (:graph block)))]
        [(if graph
           (assoc state :graph (iri graph) :subjects [[(iri graph) (iri graph)]])
           (dissoc state :graph :subjects))
         nil])

      :SUBJECT_BLOCK
      (let [subject (resolve-name state block (:subject block))]
        [(assoc state :subjects [[(:graph state) (iri subject)]])
         nil])

      :LITERAL_BLOCK
      (render-statement
       state
       block
       (format-literal state block))

      :LINK_BLOCK
      (render-statement
       state
       block
       (iri (resolve-name state block (:object block))))

      :EXPRESSION_BLOCK
      (let [new-state (render-expression state block (:expression block))
            {:keys [arrows predicate content]} block
            graph     (:graph state)
            subject   (-> state :subjects first second)
            predicate (iri (resolve-name state block predicate))
            object    (:node new-state)]
        [(-> new-state
             (dissoc :node :quads)
             (assoc :subjects [[graph subject predicate object]]))
         (concat
          [[graph subject predicate object]]
          (:quads new-state))])

      ; else
      [state nil])

    (catch #? (:clj Exception :cljs :default) e
      (util/throw-exception
       (util/format
        "Error while serializing to Nquads:\n%s line %d: %s\n%s"
        (:file-name block)
        (:line-number block)
        (:block block)
        e)))))

(defn convert-object
  "Given a state map with a :block that is a literal or link,
   return an object: an object map for a literal or an IRI string for a link."
  [{:keys [block] :as state}]
  (case (:block-type block)
    :LITERAL_BLOCK
    (select-keys block [:value :lang :type])
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
   and return the update state."
  [state]
  (let [g (:current-graph state)
        s (str "_:b" (get state :blank-node-count 0))
        p (get-in state [:block :predicate 1])
        o (convert-object state)
        statement [g s p o]
        a (count (get-in state [:block :arrows]))
        annotated (get-in state [:statements (dec a)])]
   (-> state
       (update :blank-nodes (fnil inc 0))
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

(defn convert-quads
  "Given a state (usually the output of expand-names),
   if it has a :block key,
   return an updated state with a :quads vector (maybe nil)
   with entries in EDN-LD Quads format."
  [state]
  (if-let [block (:block state)]
    (try
     (case (:block-type block)
       :LITERAL_BLOCK
       (convert-statement state)
       :LINK_BLOCK
       (convert-statement state)
       ;:EXPRESSION_BLOCK
       ;(let [new-state (render-expression state block (:expression block))
       ;      {:keys [arrows predicate content]} block
       ;      graph     (:graph state)
       ;      subject   (-> state :subjects first second)
       ;      predicate (iri (resolve-name state block predicate))
       ;      object    (:node new-state)]
       ;  [(-> new-state
       ;       (dissoc :node :quads)
       ;       (assoc :subjects [[graph subject predicate object]]))
       ;   (concat
       ;    [[graph subject predicate object]]
       ;    (:quads new-state))])

       ; else
       state)

     (catch #?(:clj Exception :cljs :default) e
       (util/throw-exception e (core/locate state))))))

(defn render-quads
  "Given a starting state map (or no arguments)
   return a stateful transducer
   that takes parse maps and returns quads (vectors of strings)."
  ([] (render-quads {}))
  ([starting-state]
   (fn
     [xf]
     (let [state (volatile! starting-state)]
       (fn
         ([] (xf))
         ([result] (xf result))
         ([result block]
          (let [[new-state quads] (render-block @state block)]
            (vreset! state new-state)
            (if quads
              (apply xf result quads)
              result))))))))

(defn quad-to-string
  "Given a quad as a vector, subject-predicate-object,
   or graph-subject-predicate-object,
   return an N-Quads string."
  ([quad]
   (apply quad-to-string quad))
  ([s p o]
   (string/join " " [s p o "."]))
  ([g s p o]
   (if g
     (string/join " " [s p o g "."])
     (string/join " " [s p o "."]))))

(defn render-statement-2
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
    {:block-type :LITERAL_BLOCK
     :content (:value object)
     ; TODO: language, datatype
     }
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
   [(render-statement-2 arrows predicate-iri (first object-map))]
   (render-predicates (str arrows ">") (second object-map))))

(defn render-predicate
  "Given an arrow string (for depth),
   a predicate IRI, and a sequence of object maps,
   return a sequence of HOWL statement block maps."
  [arrows predicate-iri object-list]
  (mapcat (partial render-statements arrows predicate-iri) object-list))

(defn render-predicates
  "Given an arrow string (for depth), and a predicate map,
   return a sequence of HOWL statement block maps."
  [arrows predicate-map]
  (->> predicate-map
       keys
       sort
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
   [:MN_CLASS_EXPRESSION
    "("
    [:MN_SOME
     [:MN_OBJECT_PROPERTY_EXPRESSION
      (ffirst (get predicate-map (str owl "onProperty")))]
     [:MN_SPACE " "]
     "some"
     [:MN_SPACE " "]
     (ffirst (get predicate-map (str owl "someValuesFrom")))]
     ")"]

   ; only
   (and
    (get-in predicate-map [(str rdf "type") (str owl "Restriction")])
    (get-in predicate-map [(str owl "onProperty")])
    (get-in predicate-map [(str owl "allValuesFrom")])
    (= 3 (count predicate-map)))
   [:MN_CLASS_EXPRESSION
    "("
    [:MN_ONLY
     [:MN_OBJECT_PROPERTY_EXPRESSION
      (ffirst (get predicate-map (str owl "onProperty")))]
     [:MN_SPACE " "]
     "only"
     [:MN_SPACE " "]
     (ffirst (get predicate-map (str owl "allValuesFrom")))]
     ")"]

   ; not
   (and
    (get-in predicate-map [(str rdf "type") (str owl "Class")])
    (get-in predicate-map [(str owl "complementOf")])
    (= 2 (count predicate-map)))
   [:MN_CLASS_EXPRESSION
    "("
    [:MN_NEGATION
     "not"
     [:MN_SPACE " "]
     (ffirst (get predicate-map (str owl "complementOf")))]
    ")"]

   ; and
   (and
    (get-in predicate-map [(str rdf "type") (str owl "Class")])
    (get-in predicate-map [(str owl "intersectionOf")])
    (= 2 (count predicate-map)))
   [:MN_CLASS_EXPRESSION
    "("
    [:MN_CONJUNCTION
     (ffirst (get predicate-map (str owl "intersectionOf")))]
    ")"]

   ; or
   (and
    (get-in predicate-map [(str rdf "type") (str owl "Class")])
    (get-in predicate-map [(str owl "unionOf")])
    (= 2 (count predicate-map)))
   [:MN_CLASS_EXPRESSION
    "("
    [:MN_DISJUNCTION
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

(defn render-expression-2
  "Given a subject map and the IRI of an OWL expression,
   return an updated subject map with the OWL expression
   'folded in' to the object map of the appropriate parent."
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
        subject-map
        ; TODO: process annotations and expressions
        ;(->> subject-map
        ;     process-annotations
        ;     process-expressions
        ;     )
        ]
    (->> subject-map
         keys
         sort ; TODO: better predicate sorting
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
  (render-graphs (graphify quads)))

(defn triples-to-howl
  "Given a sequence of triples,
   return a sequence of HOWL block maps."
  [quads]
  (render-subjects (triplify quads)))

