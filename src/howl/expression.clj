(ns howl.expression
  "Parse and process HOWL expression blocks"
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [howl.util :as util]))

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
  [:MANCHESTER_EXPRESSION (manchester-parser string)])

(defn manchester-format [mn-tree]
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

(defn parse-to-string [expression-parse]
  (let [[exp-type parse-tree] expression-parse]
    (manchester-format parse-tree)))

(defn parse-expression-block [expression-parse]
  (let [[_ exp] expression-parse]
    (string-to-parse exp)))
