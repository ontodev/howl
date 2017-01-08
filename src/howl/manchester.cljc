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
(defn rdf-type?
  [predicate-map rdf-type]
  (and (contains? predicate-map (rdf> "type"))
       (= rdf-type (get-in predicate-map [(rdf> "type") 0 :object]))))

(defn get-object-in
  [predicate-map key]
  (get-in predicate-map [key 0 :object]))

(defn manchester-seq?
  [subject-map subject]
  (let [predicate-map (get subject-map subject)]
    (and (link/blank? subject)
         (contains? predicate-map (rdf> "first"))
         (contains? predicate-map (rdf> "rest")))))

(defn manchester-restriction?
  [subject-map subject]
  (let [predicate-map (get subject-map subject)]
    (and (link/blank? subject)
         (contains? predicate-map (owl> "onProperty"))
         (rdf-type? predicate-map (owl> "Restriction")))))

(defn manchester-conjunction?
  [subject-map subject]
  (let [predicate-map (get subject-map subject)]
    (and (link/blank? subject)
         (rdf-type? predicate-map (owl> "Class"))
         (contains? predicate-map (rdf> "intersectionOf")))))

(defn manchester-negation?
  [subject-map subject]
  (let [predicate-map (get subject-map subject)]
    (and (link/blank? subject)
         (contains? predicate-map (owl> "complementOf"))
         (rdf-type? predicate-map (owl> "Class")))))

(defn manchester-expression?
  [subject-map subject]
  (let [predicate-map (get subject-map subject)]
    (and (contains? predicate-map (rdf-schema> "label"))
         (contains? predicate-map (rdf-schema> "subClassOf"))
         (link/blank? (get-object-in predicate-map (rdf-schema> "subClassOf"))))))

(defn chase-expression
  [subject-map subject]
  (when (or (manchester-expression? subject-map subject)
            (link/blank? subject))
    (cons subject
          (let [sub (get subject-map subject)]
            (cond (manchester-expression? subject-map subject)
                  (chase-expression
                   subject-map
                   (get-object-in sub (rdf-schema> "subClassOf")))

                  (manchester-restriction? subject-map subject)
                  (chase-expression
                   subject-map
                   (or (get-object-in sub (owl> "someValuesFrom"))
                       (get-object-in sub (owl> "allValuesFrom"))))

                  (manchester-conjunction? subject-map subject)
                  (chase-expression
                   subject-map
                   (get-object-in sub (rdf> "intersectionOf")))

                  (manchester-seq? subject-map subject)
                  (let [left (get-object-in sub (rdf> "first"))
                        right (get-object-in sub (rdf> "rest"))]
                    (concat
                     (chase-expression subject-map left)
                     (chase-expression subject-map right))))))))

(defn process-manchester-expression
  [env subject-map subject predicate-map]
  (println "PROCESSING EXPRESSION" subject predicate-map)
  (println "--" (get-in predicate-map [(rdf-schema> "subClassOf") 0 :object]))
  (println "--" (chase-expression subject-map subject))
  (apply dissoc subject-map (rest (chase-expression subject-map subject))))

(defn process-manchester
  [env graph subject-map]
  (println "PROCESS-MANCHESTER")
  [graph (reduce
          (fn [memo [subject predicate-map]]
            (process-manchester-expression env memo subject predicate-map))
          subject-map
          (filter
           (fn [[k v]] (manchester-expression? subject-map k))
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
