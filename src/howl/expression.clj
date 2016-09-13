(ns howl.expression
  "Parse and process HOWL expression blocks"
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [howl.util :as util]))

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
