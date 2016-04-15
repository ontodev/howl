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

(defn unwrap-iri
  [iri]
  (string/replace iri #"^<|>$" ""))

(defn format-literal
  "Given a state map and a block map for a LITERAL_BLOCK,
   return a concrete triple string."
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

(declare render-expression)

;; This is very imperative code, which is somewhat awkward in Clojure.
;; It's a little cleaner if the nodes are rendered in reverse order,
;; but this way the results are easier to read.

;; First get a new blank node for the complementOf class.
;; Then clear the state (s2) and render the negated class expression.
;; Update that result (s3) with triples for the complement and s3.

(defn render-negation
  "Given a state, a block, and a parse vector,
   render the first elements in the parse vector
   and update the state with new triples for the
   complement class and first element."
  [s1 block parse]
  (let [bs (get s1 :blank-node-count 0)
        b1 (str "_:b" (+ bs 1))
        s2 (assoc s1 :blank-node-count (+ bs 1) :triples [])
        s3 (render-expression s2 block (->> parse filter-ce first))]
    (assoc
     s3
     :node b1
     :triples
     (concat
      (:triples s1)
      [[b1 (iri rdf "type")         (iri owl "Class")]
       [b1 (iri owl "complementOf") (:node s3)]]
      (:triples s3)))))

;; Like render-negation, but with two elements:
;; the object property expression and the class expression.

(defn render-restriction
  "Given a state, a block, a parse vector, and a predicate IRI string,
   render the first and second elements in the parse vector
   and update the state with new triples for the:
   restriction class, and first and second elements."
  [s1 block parse predicate]
  (let [bs (get s1 :blank-node-count 0)
        b1 (str "_:b" (+ bs 1))
        s2 (assoc s1 :blank-node-count (+ bs 1) :triples [])
        s3 (render-expression s2 block (->> parse filter-ce first))
        s4 (render-expression s3 block (->> parse filter-ce second))]
    (assoc
     s4
     :node b1
     :triples
     (concat
      (:triples s1)
      [[b1 (iri rdf "type")       (iri owl "Restriction")]
       [b1 (iri owl "onProperty") (:node s3)]
       [b1 predicate              (:node s4)]]
      (:triples s4)))))

;; Unions and intersections are trickier because they include an RDF list.
;; First generate some blank nodes for the combination class and RDF list.
;; Then clear the state, and render the first and second elements,
;; storing the results in new states.
;; Then update the state resulting from the second element (s4)
;; with previous triples,
;; the combination element and RDF list,
;; and the triples from s4, which include the first and second elements.

(defn render-combination
  "Given a state, a block, a parse vector, and a predicate IRI string,
   render the first and second elements in the parse vector
   and update the state with new triples for the:
   combination class (i.e. unionOf, intersectionOf),
   RDF list of elements,
   first and second elements."
  [s1 block parse predicate]
  (let [bs (get s1 :blank-node-count 0)
        b1 (str "_:b" (+ bs 1))
        b2 (str "_:b" (+ bs 2))
        b3 (str "_:b" (+ bs 3))
        s2 (assoc s1 :blank-node-count (+ bs 3) :triples [])
        s3 (render-expression s2 block (->> parse filter-ce first))
        s4 (render-expression s3 block (->> parse filter-ce second))]
    (assoc
     s4
     :node b1
     :triples
     (concat
      (:triples s1)
      [[b1 (iri rdf "type")  (iri owl "Class")]
       [b1 predicate         b2]
       [b2 (iri rdf "first") (:node s3)]
       [b2 (iri rdf "rest")  b3]
       [b3 (iri rdf "first") (:node s4)]
       [b3 (iri rdf "rest")  (iri rdf "nil")]]
      (:triples s4)))))

(defn render-expression
  "Given a state map, a block map, and a parse vector with a Manchester expression
   return an updated state with triples for the expression."
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

(defn render-statement!
  "Given a reducing function, a result,
   a reference to a state map,
   a block map, and a concrete object string.
   mutate the state to update the :subjects key,
   and apply the reducing function to all the generated triples."
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
   that takes parse maps and returns triples (vectors of strings)."
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
           (render-statement!
            xf
            result
            state
            block
            (format-literal @state block))
           
           :LINK_BLOCK
           (render-statement!
            xf
            result
            state
            block
            (iri (resolve-name @state block (:object block))))

           :EXPRESSION_BLOCK
           (let [new-state (render-expression @state block (:expression block))
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
