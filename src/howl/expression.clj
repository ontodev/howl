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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; to nquads

(defn nquad-relevant-elems [exp]
  (filter #(and (vector? %) (not= :SPACE (first %))) exp))

(defn expression->nquads [id env exp]
  (case (first exp)
    (:MANCHESTER_EXPRESSION :NAME :OBJECT_PROPERTY_EXPRESSION) (expression->nquads id env (second exp))
    :LABEL [[(util/fresh-blank! id) "a-label" (second exp) (env :graph)]]
    :QUOTED_LABEL [[(util/fresh-blank! id) "a-quoted-label" (get exp 2) (env :graph)]]
    :CLASS_EXPRESSION (mapcat #(expression->nquads id env %) (nquad-relevant-elems exp))
    :SOME (let [g (env :graph)
                b (util/fresh-blank! id)
                subs (nquad-relevant-elems exp)
                left (expression->nquads id env (first subs))
                right (expression->nquads id env (second subs))]
            (concat
             [[b (<> (rdf> "type")) (<> (owl> "Restriction")) g]
              [b (<> (owl> "onProperty")) (first (first left)) g]
              [b (<> (owl> "someValuesFrom")) (first (first right)) g]]
             left
             right))
    :CONJUNCTION (let [g (env :graph)
                       b1 (util/fresh-blank! id)
                       b2 (util/fresh-blank! id)
                       b3 (util/fresh-blank! id)
                       subs (nquad-relevant-elems exp)
                       left (expression->nquads id env (first subs))
                       right (expression->nquads id env (second subs))]
                   (concat
                    [[b1 (<> (rdf> "type")) (<> (owl> "Class")) g]
                     [b1 (<> (rdf> "intersectionOf")) b2 g]
                     [b2 (<> (rdf> "first")) (first (first left)) g]
                     [b2 (<> (rdf> "rest")) b3 g]
                     [b3 (<> (rdf> "first")) (first (first right))]
                     [b3 (<> (rdf> "rest")) (<> (rdf> "nil"))]]
                    left
                    right))
    [[:TODO (first exp) exp]]))
