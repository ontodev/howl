(ns howl.manchester
  "Parse and process Manchester syntax."
  (:require [clojure.string :as string]
            [clojure.walk :refer [postwalk]]
            [instaparse.core :as insta]
            [howl.util :as util :refer [<> owl> rdf> rdf-schema>]]
            [howl.link :as link]
            [howl.core :as core]
            [howl.howl :as howl]
            [howl.nquads :as nquads]))

; LABELs that contain spaces must be single-quoted

(def manchester-iri "http://www.w3.org/TR/owl2-manchester-syntax/")

(def manchester-grammar "
CLASS_EXPRESSION = '(' SPACE? CLASS_EXPRESSION SPACE? ')' SPACE?
                 | DISJUNCTION
                 | CONJUNCTION
                 | NEGATION
                 | RESTRICTION
                 | LABEL

DISJUNCTION = CLASS_EXPRESSION SPACE 'or'  SPACE CLASS_EXPRESSION
CONJUNCTION = CLASS_EXPRESSION SPACE 'and' SPACE CLASS_EXPRESSION
NEGATION = 'not' SPACE (RESTRICTION | LABEL)

<RESTRICTION> = SOME | ONLY
SOME = OBJECT_PROPERTY_EXPRESSION SPACE 'some' SPACE CLASS_EXPRESSION
ONLY = OBJECT_PROPERTY_EXPRESSION SPACE 'only' SPACE CLASS_EXPRESSION

OBJECT_PROPERTY_EXPRESSION = 'inverse' SPACE LABEL | LABEL

LABEL = \"'\" #\"[^']+\" \"'\" | #'' #'\\w+' #''
<SPACE> = #'\\s+'")

(def manchester-parser (insta/parser manchester-grammar))

(defn parse-manchester
  [content]
  (let [result (manchester-parser content)]
    (when (insta/failure? result)
      (println result)
      (throw (Exception. "Manchester parser failure")))
    result))

(defn manchester-format
  "Given a parsed Manchester expression tree,
   return the string representation."
  [mn-tree]
  (->> mn-tree flatten (filter string?) (apply str)))

(defmethod howl/parse-content ["LINK" manchester-iri]
  [env datatypes unparsed]
  [:MANCHESTER_EXPRESSION (parse-manchester unparsed)])

; Walk the tree, replacing LABELs with IRIs, and removing other strings.

(defmethod core/content-names->iris ["LINK" manchester-iri]
  [env datatypes content]
  (postwalk
   (fn [item]
     (cond
       (and (vector? item) (= :LABEL (first item)))
       [:IRI (link/->iri env [:LABEL (nth item 2)])]
       (vector? item)
       (remove string? item)
       :else
       item))
   content))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; from NQuads
(defn get-object-in
  [predicate-map key]
  (get-in predicate-map [key 0 :object]))

(defn rdf-type?
  [predicate-map rdf-type]
  (and (contains? predicate-map (rdf> "type"))
       (= rdf-type (get-object-in predicate-map (rdf> "type")))))

(defn manchester-component-type
  [subject-map subject]
  (let [predicate-map (get subject-map subject)
        valid-keys #{(owl> "Class") (owl> "Restriction") (rdf> "type")
                     (owl> "complementOf") (owl> "onProperty") (owl> "someValuesFrom") (owl> "allValuesFrom")
                     (rdf> "intersectionOf") (rdf> "unionOf") (rdf> "first") (rdf> "rest")
                     (rdf-schema> "label") (rdf-schema> "subClassOf")}]
    (when (clojure.set/subset? (set (keys predicate-map)) valid-keys)
      (cond (link/blank? subject)
            (cond (and (rdf-type? predicate-map (owl> "Class"))
                       (contains? predicate-map (owl> "complementOf")))
                  :manchester-negation

                  (and (rdf-type? predicate-map (owl> "Class"))
                       (contains? predicate-map (rdf> "intersectionOf")))
                  :manchester-conjunction

                  (and (rdf-type? predicate-map (owl> "Class"))
                       (contains? predicate-map (rdf> "unionOf")))
                  :manchester-disjunction

                  (and (rdf-type? predicate-map (owl> "Restriction"))
                       (contains? predicate-map (owl> "onProperty")))
                  (if (contains? predicate-map (owl> "someValuesFrom"))
                    :manchester-some
                    :manchester-only)

                  (and (contains? predicate-map (rdf> "first"))
                       (contains? predicate-map (rdf> "rest")))
                  :manchester-sequence)

            (and (contains? predicate-map (rdf-schema> "label"))
                 (contains? predicate-map (rdf-schema> "subClassOf"))
                 (link/blank? (get-object-in predicate-map (rdf-schema> "subClassOf"))))
            :manchester-expression))))

(defmulti chase-expression
  manchester-component-type)

(defmethod chase-expression nil [_ _] nil)

(defmethod chase-expression :manchester-expression
  [subject-map subject]
  (cons subject
        (chase-expression
         subject-map
         (get-object-in (get subject-map subject) (rdf-schema> "subClassOf")))))

(defmethod chase-expression :manchester-some
  [subject-map subject]
  (cons subject
        (chase-expression
         subject-map
         (get-object-in (get subject-map subject) (owl> "someValuesFrom")))))

(defmethod chase-expression :manchester-only
  [subject-map subject]
  (cons subject
        (chase-expression
         subject-map
         (get-object-in (get subject-map subject) (owl> "allValuesFrom")))))

(defmethod chase-expression :manchester-conjunction
  [subject-map subject]
  (cons subject
        (chase-expression
         subject-map
         (get-object-in (get subject-map subject) (rdf> "intersectionOf")))))

(defmethod chase-expression :manchester-sequence
  [subject-map subject]
  (let [sub (get subject-map subject)
        left (get-object-in sub (rdf> "first"))
        right (get-object-in sub (rdf> "rest"))]
    (cons subject
          (concat
           (chase-expression subject-map left)
           (chase-expression subject-map right)))))

(declare make-processed-object)

(defn combination->processed-object
  [env comb rdf-comb subject-map subject]
  (let [children (get subject-map (get-object-in (get subject-map subject) rdf-comb))
        left (get-object-in children (rdf> "first"))
        right (get-object-in (get subject-map (get-object-in children (rdf> "rest"))) (rdf> "first"))]
    [:CLASS_EXPRESSION
     [:CONJUNCTION
      (make-processed-object env subject-map left)
      " " comb " "
      (make-processed-object env subject-map right)]]))

(defmulti make-processed-object
  (fn [_ subject-map subject]
    (manchester-component-type subject-map subject)))

(defmethod make-processed-object nil
  [env subject-map subject]
  [:OBJECT_PROPERTY_EXPRESSION
   (let [res (link/iri->name env subject)]
     (case (first res)
       :LABEL
       (let [label (get res 1)]
         (if (re-find #" " label)
           [:LABEL "'" label "'"]
           [:LABEL label]))
       res))])

(defmethod make-processed-object :manchester-negation
  [env subject-map subject]
  :TODO)

(defmethod make-processed-object :manchester-some
  [env subject-map subject]
  (let [rec
        (fn [prop]
          (make-processed-object
           env subject-map
           (get-object-in (get subject-map subject) prop)))]
    [:CLASS_EXPRESSION
     [:SOME (rec (owl> "onProperty")) " " "some" " " (rec (owl> "someValuesFrom"))]]))

(defmethod make-processed-object :manchester-only
  [env subject-map subject]
  (let [rec
        (fn [prop]
          (make-processed-object
           env subject-map
           (get-object-in (get subject-map subject) prop)))]
    [:CLASS_EXPRESSION
     [:ONLY (rec (owl> "onProperty")) " " "only" " " (rec (owl> "allValuesFrom"))]]))

(defmethod make-processed-object :manchester-conjunction
  [env subject-map subject]
  (combination->processed-object env "and" (rdf> "intersectionOf") subject-map subject))

(defmethod make-processed-object :manchester-disjunction
  [env subject-map subject]
  (combination->processed-object env "or" (rdf> "unionOf") subject-map subject))

(defn process-manchester-expression
  [env subject-map subject predicate-map]
  (let [relevant-subjects (chase-expression subject-map subject)
        without (apply dissoc subject-map (rest relevant-subjects))]
    (println "SUBS:" (manchester-component-type subject-map subject) relevant-subjects)
    (assoc-in
     without
     [subject (rdf-schema> "subClassOf") 0 :processed-object]
     [:MANCHESTER_EXPRESSION
      (make-processed-object env subject-map (second relevant-subjects))])))

(defn process-manchester
  [env graph subject-map]
  [graph (reduce
          (fn [memo [subject predicate-map]]
            (process-manchester-expression env memo subject predicate-map))
          subject-map
          (filter
           (fn [[k v]]
             (= :manchester-expression
                (manchester-component-type subject-map k)))
           subject-map))])

(defn handle-manchester
  [env graph-map]
  (->> graph-map
       (map (partial apply process-manchester env))
       (into {})))

;; (println
;;  (api/nquads-to-howl
;;   {:options {:sequential-blank-nodes true}}
;;   (api/howl-to-environment (slurp "test/format-context/context.howl"))
;;   (slurp "test/nquads/list1.nq")))

;; (println
;;  (api/nquads-to-howl
;;   {:options {:sequential-blank-nodes true}}
;;   (api/howl-to-environment (slurp "test/format-context/context.howl"))
;;   (slurp "test/nquads/manchester1.nq")))

;; (println
;;  (api/howl-to-nquads
;;   {:options {:sequential-blank-nodes true}}
;;   (slurp "test/format-context/context.howl")
;;   (slurp "test/format-context/manchester1.howl")))

;; (map
;;  :blocks
;;  (parse-files
;;   {:options {:sequential-blank-nodes true}}
;;   ["test/format-context/context.howl"
;;    "test/format-context/manchester1.howl"]))

(nquads/register-handler handle-manchester)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; to NQuads

(defn ->obj
  [subtree]
  (let [elem (first subtree)]
    (or (and (string? (first elem)) (first elem))
        (second elem))))

(declare convert-expression)

(defn restriction->nquads
  "Takes a restriction expression and a predicate.
   Returns an nquad sequence that represents the input. That nquad sequence
   uses the given predicate to reference the restriction."
  [[_ left right] pred]
  (let [b (link/random-blank-node)
        left (convert-expression left)
        right (convert-expression right)]
    (concat
     [[b (rdf> "type") (owl> "Restriction")]
      [b (owl> "onProperty") (->obj left)]
      [b pred (->obj right)]]
     left
     right)))

(defn combination->nquads
  "Takes a combination expression and a predicate.
   Returns an nquad sequence that represents the input. That nquad sequence
   uses the given predicate to reference the combination."
  [[_ left right] pred]
  (let [b1 (link/random-blank-node)
        b2 (link/random-blank-node)
        b3 (link/random-blank-node)
        left (convert-expression left)
        right (convert-expression right)]
    (concat
     [[b1 (rdf> "type") (owl> "Class")]
      [b1 pred b2]
      [b2 (rdf> "first") (->obj left)]
      [b2 (rdf> "rest") b3]
      [b3 (rdf> "first") (->obj right)]
      [b3 (rdf> "rest") (rdf> "nil")]]
     left
     right)))

(defn negation->nquads
  "Takes a negation expression. Returns an nquad
   sequence that represents the input. Unlike combination and restriction
   expressions, there is only one possible negation expression predicate, so
   there is no need for the additional argument."
  [[_ target]]
  (let [b (link/random-blank-node)
        target (convert-expression target)]
    (concat
     [[b (rdf> "type") (owl> "Class")]
      [b (owl> "complementOf") (->obj target)]]
     target)))

(defn convert-expression
  "Takes an expression parse tree.
   Returns a sequence of nquads representing that expression."
  [exp]
  (case (first exp)
    (:MANCHESTER_EXPRESSION :OBJECT_PROPERTY_EXPRESSION) (convert-expression (second exp))
    :IRI [exp]
    :CLASS_EXPRESSION (mapcat convert-expression (rest exp))
    :SOME (restriction->nquads exp (owl> "someValuesFrom"))
    :ONLY (restriction->nquads exp (owl> "allValuesFrom"))
    :CONJUNCTION (combination->nquads exp (rdf> "intersectionOf"))
    :DISJUNCTION (combination->nquads exp (rdf> "unionOf"))
    :NEGATION (negation->nquads exp)
    (util/throw-exception "UNSUPPORTED ->NQUADS FORM" exp)))

(defmethod nquads/object->nquads ["LINK" manchester-iri]
  [graph datatypes object]
  (->> object
       convert-expression
       (filter #(= 3 (count %)))
       (map #(concat [graph] % ["LINK"]))
       ((fn [xs] [(second (first xs)) xs]))))
