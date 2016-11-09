(ns howl.expression
  "Parse and process HOWL expression blocks"
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [howl.util :as util :refer [<> owl> rdf>]]))

(def manchester-parser
  (insta/parser
   "CLASS_EXPRESSION = '(' SPACE? CLASS_EXPRESSION SPACE? ')' SPACE?
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

(defn manchester-format
  "Given a parsed Manchester expression tree (without leading expression type tag),
   returns the stringified version of that tree."
  [mn-tree]
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

(defn string-to-parse
  "Given a string representing an unparsed expression, returns the parsed
   expression tree. Currently always returns Manchester expression trees,
   but will eventually try multiple syntaxes, only defaulting to Manchester."
  [string]
  [:MANCHESTER_EXPRESSION (manchester-parser string)])

(defn parse-expression-block
  "Given an unparsed expression tree, parse the given expression string
   into a parse tree. The parser tries a number of expression syntaxes
   before defaulting to Manchester syntax."
  [[_ exp]]
  (string-to-parse exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; to NQuads

(defn nquad-relevant-elems
  "Takes an expression tree and returns a sequence of sub-expressions relevant to
   nqud-generation. In particular, this means skipping :SPACE sub-trees and some
   inline literals."
  [exp]
  (filter #(and (vector? %) (not= :SPACE (first %))) exp))

(defn ->obj
  [subtree]
  (first (first subtree)))

(declare convert-expression)

(defn restriction->nquads
  "Takes an id, an environment a restriction expression and a predicate.
   Returns an nquad sequence that represents the input. That nquad sequence
   uses the given predicate to reference the restriction."
  [id env exp pred]
  (let [g (env :graph)
        b (util/fresh-blank! id)
        subs (nquad-relevant-elems exp)
        left (convert-expression id env (first subs))
        right (convert-expression id env (second subs))]
    (concat
     [[g b (rdf> "type") (owl> "Restriction")]
      [g b (owl> "onProperty") (->obj left)]
      [g b pred (->obj right)]]
     left
     right)))

(defn combination->nquads
  "Takes an id, an environment a combination expression and a predicate.
   Returns an nquad sequence that represents the input. That nquad sequence
   uses the given predicate to reference the combination."
  [id env exp pred]
  (let [g (env :graph)
        b1 (util/fresh-blank! id)
        b2 (util/fresh-blank! id)
        b3 (util/fresh-blank! id)
        subs (nquad-relevant-elems exp)
        left (convert-expression id env (first subs))
        right (convert-expression id env (second subs))]
    (concat
     [[g b1 (rdf> "type") (owl> "Class")]
      [g b1 pred b2]
      [g b2 (rdf> "first") (->obj left)]
      [g b2 (rdf> "rest") b3]
      [g b3 (rdf> "first") (->obj right)]
      [g b3 (rdf> "rest") (rdf> "nil")]]
     left
     right)))

(defn negation->nquads
  "Takes an id, an environment and a negation expression. Returns an nquad
   sequence that represents the input. Unlike combination and restriction
   expressions, there is only one possible negation expression predicate, so
   there is no need for the additional argument."
  [id env exp]
  (let [g (env :graph)
        b (util/fresh-blank! id)
        target (convert-expression id env (first (nquad-relevant-elems exp)))]
    (concat
     [[g b (rdf> "type") (owl> "Class")]
      [g b (owl> "complementOf") (->obj target)]]
     target)))

(defn subexp->name
  "Takes an environment and a name.
   Either returns the value associated with that name in that environment, or
   throws an error."
  [env name]
  (or (get-in env [:labels name])
      (util/throw-exception
       "NO SUCH NAME IN ENV."
       "Name:" name
       "Env:" (str env))))

(defn ->exp
  "Takes a subtree, and returns an expression suitable for stitching into an nquads result.
   This is either a wrapped answer (in the case of a string)
   or identity (in the case of a sequence or vector)"
  [subtree]
  (if (string? subtree)
    [[subtree]]
    subtree))

(defn convert-expression
  "Takes an id atom, an environment and an expression parse tree.
   Returns a sequence of nquads representing that expression in that environment
   with blank nodes generated from that atom."
  [id env exp]
  (->exp
   (case (first exp)
     (:MANCHESTER_EXPRESSION :NAME :OBJECT_PROPERTY_EXPRESSION) (convert-expression id env (second exp))
     :LABEL (subexp->name env (second exp))
     :QUOTED_LABEL (subexp->name env (get exp 2))
     :CLASS_EXPRESSION (mapcat #(convert-expression id env %) (nquad-relevant-elems exp))
     :SOME (restriction->nquads id env exp (owl> "someValuesFrom"))
     :ONLY (restriction->nquads id env exp (owl> "allValuesFrom"))
     :CONJUNCTION (combination->nquads id env exp (rdf> "intersectionOf"))
     :DISJUNCTION (combination->nquads id env exp (rdf> "unionOf"))
     :NEGATION (negation->nquads id env exp)
     (util/throw-exception "UNSUPPORTED ->NQUADS FORM" exp))))

(defn expression->nquads
  [id env exp]
  (filter #(= 4 (count %)) (convert-expression id env exp)))
