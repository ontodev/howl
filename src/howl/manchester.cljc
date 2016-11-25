(ns howl.manchester
  "Parse and process Manchester syntax."
  (:require [clojure.string :as string]
            [clojure.walk :refer [postwalk]]
            [instaparse.core :as insta]
            [howl.core :as core]
            [howl.howl :as howl]
            [howl.util :as util :refer [<> owl> rdf>]]))

; LABELs that contain spaces must be single-quoted

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; to NQuads

(defn nquad-relevant-elems
  "Takes an expression tree and returns a sequence of sub-expressions relevant to
   nqud-generation. In particular, this means skipping whitespace and literals."
  [exp]
  (filter vector? exp))

(defn ->obj
  [subtree]
  (let [elem (first subtree)]
    (or (second elem) (first elem))))

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
     (:MANCHESTER_EXPRESSION :OBJECT_PROPERTY_EXPRESSION) (convert-expression id env (second exp))
     :LABEL (subexp->name env (get exp 2))
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
