(ns howl.ntriples
  (:require [clojure.string :as string]
            [howl.core :as core :refer [resolve-name valid-label?]]))

(def rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(def rdfs "http://www.w3.org/2000/01/rdf-schema#")
(def owl "http://www.w3.org/2002/07/owl#")
(def plain-literal "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral")
(def rdfs-label "http://www.w3.org/2000/01/rdf-schema#label")
(def xsd-string "http://www.w4.org/2001/XMLSchema#string")

(defn iri
  [& parts]
  (str "<" (string/join "" parts) ">"))

(defn format-literal
  [state {:keys [content language datatype] :as block}]
  (let [content (string/replace content "\n" "\\n")]
    (cond
      language
      (format "\"%s\"%s" content language)
      datatype
      (format "\"%s\"^^<%s>"
              content
              (resolve-name state block datatype))
      :else
      (format "\"%s\"" content))))

(defn filter-ce
  "Given a parse vector,
   remove the elements that don't matter for Manchester."
  [parse]
  (->> parse
       (filter vector?)
       (remove #(= :MN_SPACE (first %)))
       (remove #(= :SPACES (first %)))))

(declare ce-triples)

(defn process-list
  [block state element]
  (let [state (ce-triples state block element)]
    (update state :rdf-list (fnil conj []) (:node state))))

(defn rdf-list-element
  "Given the triple of a blank node, a 'first' node, and a 'rest' node,
   return triples representing an RDF list item."
  [[bnode rdf-first rdf-rest]]
  [[bnode (iri rdf "first") rdf-first]
   [bnode (iri rdf "rest") rdf-rest]])

(defn rdf-list
  "Given a state map and a sequence of nodes,
   update the statement with an RDF list of the nodes."
  [state nodes]
  (let [blank-node-count (inc (get state :blank-node-count 0))
        final-count      (+ blank-node-count (count nodes))
        bnodes (for [b (range blank-node-count final-count)]
                 (str "_:b" b))
        lists (map vector bnodes nodes (concat (rest bnodes) [(iri rdf "nil")]))]
    (-> state
        (assoc :node (first bnodes))
        (assoc :blank-node-count (dec final-count))
        (update :triples concat (mapcat rdf-list-element lists)))))

(defn render-combination
  [state block parse predicate]
  (let [first-state  (ce-triples state block (->> parse filter-ce first))
        second-state (ce-triples first-state block (->> parse filter-ce second))
        third-state  (rdf-list second-state [(:node first-state) (:node second-state)])
        blank-node-count (inc (get third-state :blank-node-count 0))
        bnode            (str "_:b" blank-node-count)]
    (-> third-state
        (assoc :blank-node-count blank-node-count)
        (assoc :node bnode)
        (update
         :triples
         concat
         [[bnode (iri rdf "type") (iri owl "Class")]
          [bnode predicate        (:node third-state)]]))))

(defn render-restriction
  [state block parse predicate]
  (let [ope-state (ce-triples state block (->> parse filter-ce first))
        ce-state  (ce-triples ope-state block (->> parse filter-ce second))
        blank-node-count (inc (get ce-state :blank-node-count 0))
        bnode            (str "_:b" blank-node-count)]
    (-> ce-state
        (assoc :blank-node-count blank-node-count)
        (assoc :node bnode)
        (update
         :triples
         concat
         [[bnode (iri rdf "type")       (iri owl "Restriction")]
          [bnode (iri owl "onProperty") (:node ope-state)]
          [bnode predicate              (:node ce-state)]]))))

(defn ce-triples
  [state block parse]
  (case (first parse)
    :MN_NEGATION
    (let [state (ce-triples state block (->> parse filter-ce first))
          blank-node-count (inc (get state :blank-node-count 0))
          bnode            (str "_:b" blank-node-count)]
      (-> state
          (assoc :blank-node-count blank-node-count)
          (assoc :node bnode)
          (update
           :triples
           concat
           [[bnode (iri rdf "type")         (iri owl "Class")]
            [bnode (iri owl "complementOf") (:node state)]])))

    :MN_DISJUNCTION
    (render-combination state block parse (iri owl "unionOf"))

    :MN_CONJUNCTION
    (render-combination state block parse (iri owl "intersectionOf"))

    :MN_SOME
    (render-restriction state block parse (iri owl "someValuesFrom"))

    :MN_CLASS_EXPRESSION
    (ce-triples state block (->> parse filter-ce first))
    
    :MN_OBJECT_PROPERTY_EXPRESSION
    (ce-triples state block (->> parse filter-ce first))

    :MN_NAME
    (ce-triples state block (->> parse filter-ce first))

    :MN_LABEL
    (assoc state :node (iri (resolve-name state block [:LABEL (second parse)])))

    :MN_QUOTED_LABEL
    (assoc state :node (iri (resolve-name state block [:LABEL (nth parse 2)])))

    ; else
    state))

(defn unwrap-iri
  [iri]
  (string/replace iri #"^<|>$" ""))


(defn render-statement
  [xf result state block object]
  (let [{:keys [arrows predicate content]} block
        subject   (if (string/blank? arrows)
                    (-> @state :subjects first first)
                    (do
                      (vswap! state update-in [:blank-node-count] inc)
                      (str "_:b" (:blank-node-count @state))))
        predicate (iri (resolve-name @state block predicate))]

    ; When this is a HOWL-valid rdfs:label, add it to the map of labels.
    (when (and (string/blank? arrows)
               (= predicate (iri rdfs "label"))
               (valid-label? content))
      (vswap! state assoc-in [:labels content] (unwrap-iri subject)))

    ; Add this triple to the stack of subjects.
    (vswap! state
            assoc
            :subjects
            (conj (vec (take (count arrows) (:subjects @state)))
                  [subject predicate object]))

    ; Two cases
    (if (string/blank? arrows)
      ; normal literal node
      (xf result [subject predicate object])
      ; annotation (maybe nested)
      (let [[annotatedSource annotatedProperty annotatedTarget]
            (get-in @state [:subjects (dec (count arrows))])]
        (apply
         xf
         result
         [[subject (iri rdf "type") (iri owl "Axiom")]
          [subject (iri owl "annotatedSource") annotatedSource]
          [subject (iri owl "annotatedProperty") annotatedProperty]
          [subject (iri owl "annotatedTarget") annotatedTarget]
          [subject predicate object]])))))

(def default-state
  {:blank-node-count 0
   :labels {"label" (str rdfs "label")}})

(defn to-triples
  "Given a reducing function,
   return a stateful transducer
   that takes parse maps
   and returns NTriple statement strings."
  [xf]
  (let [state (volatile! default-state)]
    (fn
      ([] (xf))
      ([result] (xf result))
      ([result block]
       (try
         (case (:block-type block)
           :PREFIX_BLOCK
           (do
             (vswap! state assoc-in [:prefixes (:prefix block)] (:iri block))
             result)

           :LABEL_BLOCK
           (let [iri (resolve-name @state block (:identifier block))]
             (vswap! state assoc-in [:labels (:label block)] iri)
             result)

           :TYPE_BLOCK
           (let [predicate-iri (resolve-name @state block (:predicate block))
                 datatype-iri  (resolve-name @state block (:datatype block))]
             (vswap! state assoc-in [:types predicate-iri] datatype-iri)
             result)

           :GRAPH_BLOCK
           (let [graph (resolve-name @state block (:graph block))]
             (vswap! state assoc :subjects [[(iri graph)]])
             (.println ; TODO: better logging
              *err*
              (format "WARN: Graphs are not supported by the NTriples serializer.\n%s line %d: %s"
                      (:file-name block)
                      (:line-number block)
                      (:block block)))
             result)
           
           :SUBJECT_BLOCK
           (let [subject (resolve-name @state block (:subject block))]
             (vswap! state assoc :subjects [[(iri subject)]])
             result)

           :LITERAL_BLOCK
           (render-statement
            xf
            result
            state
            block
            (format-literal @state block))
           
           :LINK_BLOCK
           (render-statement
            xf
            result
            state
            block
            (iri (resolve-name @state block (:object block))))

           :EXPRESSION_BLOCK
           (let [new-state (ce-triples @state block (:expression block))
                 {:keys [arrows predicate content]} block
                 subject   (-> @state :subjects first first)
                 predicate (iri (resolve-name @state block predicate))
                 object    (:node new-state)]
             (vreset!
              state
              (-> new-state
                  (dissoc :node :triples)
                  (assoc :subjects [[subject predicate object]])))
             (apply
              xf
              result
              (concat
               (:triples new-state)
               [[subject predicate object]])))

           ; else
           result)

         (catch Exception e
           (throw
            (Exception.
             (format "Error while serializing to Ntriples:\n%s line %d: %s\n%s"
                     (:file-name block)
                     (:line-number block)
                     (:block block)
                     (.getMessage e))))))))))

