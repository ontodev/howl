(ns howl.expression
  "Parse and process HOWL expression blocks"
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [howl.util :as util]))

;;;;;;;;;; Converting from string to parse tee
(def manchester-parser
  (insta/parser
   "CLASS_EXPRESSION = '(' SPACE? CLASS_EXPRESSION SPACE? ')'
      | DISJUNCTION
      | CONJUNCTION
      | NEGATION
      | RESTRICTION
      | NAME

    DISJUNCTION = CLASS_EXPRESSION SPACE 'or'  SPACE CLASS_EXPRESSION
    CONJUNCTION = CLASS_EXPRESSION SPACE 'and' SPACE CLASS_EXPRESSION
    NEGATION = 'not' SPACE (RESTRICTION | NAME)

    <RESTRICTION> = SOME | ONLY
    SOME = OBJECT_PROPERTY_EXPRESSION SPACE 'some' SPACE CLASS_EXPRESSION
    ONLY = OBJECT_PROPERTY_EXPRESSION SPACE 'only' SPACE CLASS_EXPRESSION

    OBJECT_PROPERTY_EXPRESSION = 'inverse' SPACE NAME | NAME

    NAME = QUOTED_LABEL | LABEL
    QUOTED_LABEL = \"'\" #\"[^']+\" \"'\"
    LABEL = #'\\w+'
    SPACE = #'\\s+'"))

(defn string-to-parse [string]
  "Given a string representing an unparsed expression, returns the parsed
   expression tree. Currently always returns Manchester expression trees,
   but will eventually try multiple syntaxes, only defaulting to Manchester."
  [:MANCHESTER_EXPRESSION (manchester-parser string)])

(defn parse-expression-block
  "Given an unparsed expression tree, parse the given expression string
   into a parse tree. The parser tries a number of expression syntaxes
   before defaulting to Manchester syntax."
  [[_ exp]]
  (string-to-parse exp))

;;;;;;;;;; Converting from parse tree to string
(defn manchester-format [mn-tree]
  "Given a parsed Manchester expression tree (without leading expression type tag),
   returns the stringified version of that tree."
  (if (string? mn-tree)
    mn-tree
    (case (first mn-tree)
      (:CLASS_EXPRESSION
       :DISJUNCTION :CONJUNCTION :NEGATION
       :SOME :ONLY
       :OBJECT_PROPERTY_EXPRESSION) (apply str (map manchester-format (rest mn-tree)))

      :NAME (manchester-format (second mn-tree))
      (:SPACE :LABEL) (second mn-tree)
      :QUOTED_LABEL (apply str (rest mn-tree)))))

(defn parse-to-string
  "Given a parsed expression tree (with the leading expression type tag),
   emits the stringified version of that tree. The intent is for this tree
   to be character-equivalent to the input expression that generated the
   parse tree to begin with.

   At the moment, deals only with Manchester expression trees, but will
   shortly be able to emit other syntax trees depending on the leading tag."
  [[exp-type parse-tree]]
  (manchester-format parse-tree))

;;;;;;;;;; Converting from parse tree to nquads

(defn rdf+ [s] (str "http://www.w3.org/1999/02/22-rdf-syntax-ns#" s))
(defn owl+ [s] (str "http://www.w3.org/2002/07/owl#" s))

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
      [[g b1 (rdf+ "type")         (owl+ "Class")]
       [g b1 (owl+ "complementOf") (:node s3)]]
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
      [[g b1 (rdf+ "type")       (owl+ "Restriction")]
       [g b1 (owl+ "onProperty") (:node s3)]
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
      [[g b1 (rdf+ "type")  (owl+ "Class")]
       [g b1 predicate         b2]
       [g b2 (rdf+ "first") (:node s3)]
       [g b2 (rdf+ "rest")  b3]
       [g b3 (rdf+ "first") (:node s4)]
       [g b3 (rdf+ "rest")  (rdf+ "nil")]]
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
    (convert-combination state parse (owl+ "unionOf"))

    :CONJUNCTION
    (convert-combination state parse (owl+ "intersectionOf"))

    :OBJECT_PROPERTY_EXPRESSION
    (convert-expression state (->> parse filter-ce first))

    :SOME
    (convert-restriction state parse (owl+ "someValuesFrom"))

    :ONLY
    (convert-restriction state parse (owl+ "allValuesFrom"))

    :NAME
    (convert-expression state (->> parse filter-ce first))

    :ABSOLUTE_IRI
    (assoc state :node (second parse))

    ; else
    state))
