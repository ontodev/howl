(ns howl.nquads
  "Render parsed HOWL to N-Quads."
  (:require [clojure.string :as string]
            [howl.util :as util]
            [howl.core :as core]))

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

(defn resolve-iri
  "Given a state map, a block map, and an IRI parse vector,
   return an absolute IRI string,
   or throw an exception."
  [state block [type left iri right]]
  (cond
    (re-matches #"^\w+://\S+$" iri)
    iri
    (string? (:base state))
    (str (:base state) iri)
    :else
    (util/throw-exception
     (util/format
      "Could not resolve relative IRI '%s' with BASE '%s' in '%s' at line %d:\n%s"
      iri
      (:base state)
      (:file-name block)
      (:line-number block)
      (:line block)))))

(defn resolve-prefixed-name
  "Given a state map, a block map, and a PREFIXED_NAME parse vector,
   return an absolute IRI string,
   or throw an exception."
  [state block [type prefix colon local-name]]
  (let [iri (get-in state [:prefixes prefix])]
    (if (string? iri)
      (str iri local-name)
      (util/throw-exception
       (util/format
        "Could not resolve prefixed name '%s' in '%s' at line %d:\n%s"
        (str prefix colon local-name)
        (:file-name block)
        (:line-number block)
        (:line block))))))

(defn resolve-label
  "Given a state map, a block map, and a LABEL parse vector,
   return an absolute IRI string,
   or throw an exception."
  [state block [type label]]
  (let [iri (get-in state [:labels label])]
    (if (string? iri)
      iri
      (util/throw-exception
       (util/format
        "Could not resolve label '%s' in '%s' at line %d:\n%s"
        label
        (:file-name block)
        (:line-number block)
        (:line block))))))

(defn resolve-absolute
  "Given a state map, a block map, and a parse vector,
   return an absolute IRI string,
   or throw an exception."
  [state block name]
  (case (first name)
    :IRI
    (resolve-iri state block name)
    :PREFIXED_NAME
    (resolve-prefixed-name state block name)
    :LABEL
    (resolve-label state block name)
    ; else
    (util/throw-exception
     (util/format
      "Could not resolve name '%s' in '%s' at line %d:\n%s"
      name
      (:file-name block)
      (:line-number block)
      (:line block)))))

(defn resolve-name
  "Given a state map, a block map, and a parse vector,
   resolve the name to an absolute IRI,
   check the result and return it,
   or throw an exception."
  [state block name]
  (let [iri (resolve-absolute state block name)]
    (if (re-matches #"^\w+://\S+$" iri)
      iri
      (util/throw-exception
       (util/format
        "Resolved IRI '%s' is not absolute in '%s' at line %d:\n%s"
        iri
        (:file-name block)
        (:line-number block)
        (:line block))))))

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
      (vswap! state assoc-in [:labels content] (unwrap-iri subject)))

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
      [(assoc state :base (:iri block))
       nil]

      :PREFIX_BLOCK
      [(assoc-in state [:prefixes (:prefix block)] (:iri block))
       nil]

      :LABEL_BLOCK
      (let [iri (resolve-name state block (:identifier block))]
        [(assoc-in state [:labels (:label block)] iri)
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
