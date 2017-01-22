(ns howl.manchester
  "Parse and process Manchester syntax."
  (:require [clojure.string :as string]
            [clojure.walk :refer [postwalk]]
            [instaparse.core :as insta]
            [howl.util :as util]
            [howl.link :as link]
            [howl.core :as core]
            [howl.howl :as howl]
            [howl.nquads :as nquads]

            [howl.constants.owl :as owl]
            [howl.constants.rdf :as rdf]))

; LABELs that contain spaces must be single-quoted

(def ^:const manchester-iri "http://www.w3.org/TR/owl2-manchester-syntax/")

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
  [predicate-map keys]
  (get-in predicate-map (concat keys [0 :object])))

(defn rdf-type?
  [predicate-map type-string]
  (and (contains? predicate-map rdf/type)
       (= type-string (get-object-in predicate-map [rdf/type]))))

(def ^:const valid-manchester-predicate-keys
  #{owl/class owl/restriction rdf/type
    owl/complement-of owl/on-property owl/some-values-from owl/all-values-from
    rdf/intersection-of rdf/union-of rdf/first rdf/rest
    rdf/label rdf/sub-class-of})

(def manchester-component-type
  (memoize
   (fn [subject-map subject]
     (let [predicate-map (get subject-map subject)]
       (when (= subject "_:b1")
         (println "manchester-component-type")
         (println
          "CONSIDERING" subject
          (link/blank? subject)
          (rdf-type? predicate-map owl/restriction)
          (contains? predicate-map owl/on-property)
          (contains? predicate-map owl/some-values-from)))
       (cond (link/blank? subject)
             (cond (and (rdf-type? predicate-map owl/class)
                        (contains? predicate-map owl/complement-of))
                   :manchester-negation

                   (and (rdf-type? predicate-map owl/class)
                        (contains? predicate-map rdf/intersection-of))
                   :manchester-conjunction

                   (and (rdf-type? predicate-map owl/class)
                        (contains? predicate-map rdf/union-of))
                   :manchester-disjunction

                   (and (rdf-type? predicate-map owl/restriction)
                        (contains? predicate-map owl/on-property))
                   (if (contains? predicate-map owl/some-values-from)
                     :manchester-some
                     :manchester-only)

                   (and (contains? predicate-map rdf/first)
                        (contains? predicate-map rdf/rest))
                   :manchester-sequence)

             (and (contains? predicate-map rdf/label)
                  (contains? predicate-map rdf/sub-class-of)
                  (link/blank? (get-object-in predicate-map [rdf/sub-class-of])))
             :manchester-expression)))))

(defmulti chase-expression
  manchester-component-type)

(defmethod chase-expression nil [_ subject]
  (cons subject nil))

(defmethod chase-expression :manchester-negation
  [subject-map subject]
  (cons subject
        (chase-expression
         subject-map
         (get-object-in subject-map [subject owl/complement-of]))))

(defmethod chase-expression :manchester-conjunction
  [subject-map subject]
  (cons subject
        (chase-expression
         subject-map
         (get-object-in subject-map [subject rdf/intersection-of]))))

(defmethod chase-expression :manchester-disjunction
  [subject-map subject]
  (cons subject
        (chase-expression
         subject-map
         (get-object-in subject-map [subject rdf/union-of]))))

(defmethod chase-expression :manchester-some
  [subject-map subject]
  (cons subject
        (chase-expression
         subject-map
         (get-object-in subject-map [subject owl/some-values-from]))))

(defmethod chase-expression :manchester-only
  [subject-map subject]
  (cons subject
        (chase-expression
         subject-map
         (get-object-in subject-map [subject owl/all-values-from]))))

(defmethod chase-expression :manchester-sequence
  [subject-map subject]
  (let [sub (get subject-map subject)
        left (get-object-in sub [rdf/first])
        right (get-object-in sub [rdf/rest])]
    (cons subject
          (concat
           (chase-expression subject-map left)
           (chase-expression subject-map right)))))

(defmethod chase-expression :manchester-expression
  [subject-map subject]
  (cons subject
        (chase-expression
         subject-map
         (get-object-in subject-map [subject rdf/sub-class-of]))))

(declare make-processed-object)

(defn combination->processed-object
  [env comb rdf-comb subject-map subject]
  (let [children (get subject-map (get-object-in subject-map [subject rdf-comb]))
        left (get-object-in children [rdf/first])
        right (get-object-in subject-map [(get-object-in children [rdf/rest]) rdf/first])]
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
  [:CLASS_EXPRESSION
   [:NEGATION "not" " "
    (make-processed-object
     env subject-map
     (get-object-in subject-map [subject owl/complement-of]))]])

(defmethod make-processed-object :manchester-some
  [env subject-map subject]
  (let [rec
        (fn [prop]
          (make-processed-object
           env subject-map
           (get-object-in subject-map [subject prop])))]
    [:CLASS_EXPRESSION
     [:SOME (rec owl/on-property) " " "some" " " (rec owl/some-values-from)]]))

(defmethod make-processed-object :manchester-only
  [env subject-map subject]
  (let [rec
        (fn [prop]
          (make-processed-object
           env subject-map
           (get-object-in subject-map [subject prop])))]
    [:CLASS_EXPRESSION
     [:ONLY (rec owl/on-property) " " "only" " " (rec owl/all-values-from)]]))

(defmethod make-processed-object :manchester-conjunction
  [env subject-map subject]
  (combination->processed-object env "and" rdf/intersection-of subject-map subject))

(defmethod make-processed-object :manchester-disjunction
  [env subject-map subject]
  (combination->processed-object env "or" rdf/union-of subject-map subject))

(defn deep-verify
  [subject-map subject relevant-subjects]
  (every? (fn [subject]
            (clojure.set/subset?
             (set (keys (get subject-map subject)))
             valid-manchester-predicate-keys))
          relevant-subjects))

(defn process-manchester-expression
  [env subject-map subject predicate-map]
  (let [relevant-subjects (chase-expression subject-map subject)]
    (if (deep-verify subject-map subject relevant-subjects)
      (let [without (apply dissoc subject-map (rest relevant-subjects))]
        (assoc-in
         without
         [subject rdf/sub-class-of 0 :processed-object]
         [:MANCHESTER_EXPRESSION
          (make-processed-object env subject-map (second relevant-subjects))]))
      subject-map)))

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
     [[b rdf/type owl/restriction]
      [b owl/on-property (->obj left)]
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
     [[b1 rdf/type owl/class]
      [b1 pred b2]
      [b2 rdf/first (->obj left)]
      [b2 rdf/rest b3]
      [b3 rdf/first (->obj right)]
      [b3 rdf/rest rdf/NIL]]
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
     [[b rdf/type owl/class]
      [b owl/complement-of (->obj target)]]
     target)))

(defn convert-expression
  "Takes an expression parse tree.
   Returns a sequence of nquads representing that expression."
  [exp]
  (case (first exp)
    (:MANCHESTER_EXPRESSION :OBJECT_PROPERTY_EXPRESSION) (convert-expression (second exp))
    :IRI [exp]
    :CLASS_EXPRESSION (mapcat convert-expression (rest exp))
    :SOME (restriction->nquads exp owl/some-values-from)
    :ONLY (restriction->nquads exp owl/all-values-from)
    :CONJUNCTION (combination->nquads exp rdf/intersection-of)
    :DISJUNCTION (combination->nquads exp rdf/union-of)
    :NEGATION (negation->nquads exp)
    (util/throw-exception "UNSUPPORTED ->NQUADS FORM '" exp "'")))

(defn tap [thing]
  (println " ====>" thing)
  thing)

(defmethod nquads/object->nquads ["LINK" manchester-iri]
  [graph datatypes object]
  (let [converted (convert-expression object)]
    (if (and (= 1 (count converted))
             (vector? (first converted))
             (= :IRI (first (first converted))))
      [(second (first converted))]
      (->> converted
           (filter #(= 3 (count %)))
           (map #(concat [graph] % ["LINK"]))
           ((fn [xs] [(second (first xs)) xs]))))))
