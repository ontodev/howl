(ns howl.core
  "Parse HOWL."
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [howl.util :as util]))

;; Parsing works like this:
;;
;; 1. loop over files
;; 2. loop over lines in file
;; 3. merge indented lines into a single "unit"
;; 4. parse the unit and emit a map

(def block-parser
  (insta/parser
   "BLOCK = BASE_BLOCK / PREFIX_BLOCK /
            LABEL_BLOCK / TYPE_BLOCK /
            GRAPH_BLOCK / SUBJECT_BLOCK /
            LITERAL_BLOCK / LINK_BLOCK / EXPRESSION_BLOCK

    BASE_BLOCK       = 'BASE'   SPACES IRI EOL
    PREFIX_BLOCK     = 'PREFIX' SPACES PREFIX     COLON IRI EOL
    LABEL_BLOCK      = 'LABEL'  SPACES IDENTIFIER COLON LABEL EOL
    TYPE_BLOCK       = 'TYPE'   SPACES PREDICATE  COLON DATATYPE EOL
    GRAPH_BLOCK      = 'GRAPH'  EOL /
                       'GRAPH'  SPACES GRAPH EOL
    SUBJECT_BLOCK    = SUBJECT EOL
    LITERAL_BLOCK    = ARROWS PREDICATE COLON LITERAL EOL
    LINK_BLOCK       = ARROWS PREDICATE ARROW_COLON OBJECT EOL
    EXPRESSION_BLOCK = PREDICATE ARROWS_COLON MN_CLASS_EXPRESSION EOL

    MN_CLASS_EXPRESSION = '(' MN_SPACE? MN_CLASS_EXPRESSION MN_SPACE? ')'
      | MN_DISJUNCTION
      | MN_CONJUNCTION
      | MN_NEGATION
      | MN_RESTRICTION
      | MN_NAME

    MN_DISJUNCTION = MN_CLASS_EXPRESSION MN_SPACE 'or'  MN_SPACE MN_CLASS_EXPRESSION
    MN_CONJUNCTION = MN_CLASS_EXPRESSION MN_SPACE 'and' MN_SPACE MN_CLASS_EXPRESSION
    MN_NEGATION = 'not' MN_SPACE (MN_RESTRICTION | MN_NAME)

    <MN_RESTRICTION> = MN_SOME | MN_ONLY
    MN_SOME = MN_OBJECT_PROPERTY_EXPRESSION MN_SPACE 'some' MN_SPACE MN_CLASS_EXPRESSION
    MN_ONLY = MN_OBJECT_PROPERTY_EXPRESSION MN_SPACE 'only' MN_SPACE MN_CLASS_EXPRESSION

    MN_OBJECT_PROPERTY_EXPRESSION = 'inverse' MN_SPACE MN_NAME | MN_NAME

    MN_NAME = MN_QUOTED_LABEL | MN_LABEL
    MN_QUOTED_LABEL = \"'\" #\"[^']+\" \"'\"
    MN_LABEL = #'\\w+'
    MN_SPACE = #'\\s+'

    IDENTIFIER = BLANK_NODE / PREFIXED_NAME / IRI
    GRAPH      = BLANK_NODE / PREFIXED_NAME / IRI / LABEL
    SUBJECT    = BLANK_NODE / PREFIXED_NAME / IRI / LABEL
    PREDICATE  = PREFIXED_NAME / IRI / LABEL
    OBJECT     = BLANK_NODE / PREFIXED_NAME / IRI / LABEL
    DATATYPE   = PREFIXED_NAME / IRI / LABEL
    LITERAL    = #'.+(?=@(\\w|-)+)' LANG /
                 #'.+(?=\\^\\^\\S+)' '^^' DATATYPE /
                 #'(\n|.)+.+'

    PREFIX        = #'(\\w|-)+'
    BLANK_NODE    = '_:' #'(\\w|-)+'
    PREFIXED_NAME = #'(\\w|-)+' ':' #'[^\\s|:]+'
    IRI           = '<' #'[^>\\s]+' '>'
    LANG          = #'@(\\w|-)+'
    COLON         = #' *' ':'  #' +'
    ARROW_COLON   = #' *' ':>' #' +'
    ARROWS_COLON  = #' *' ':>>' #' +'
    SPACES        = #' +'
    ARROWS        = #'>*' #'\\s*'
    LABEL         = #'[^:\n]+'
    EOL           = #'(\r|\n|\\s)*'
    "))

(defn valid-label?
  "Given a string, check whether it can be a HOWL label."
  [label]
  (cond
    (not (string? label)) false
    (.startsWith label ">") false
    (.startsWith label " ") false
    (.startsWith label "BASE") false
    (.startsWith label "PREFIX") false
    (.startsWith label "LABEL") false
    (.startsWith label "TYPE") false
    (.startsWith label "GRAPH") false
    (util/substring? label "\n") false
    (util/substring? label "\t") false
    (util/substring? label ": ") false
    (util/substring? label ":> ") false
    (.endsWith   label " ") false
    :else true))

(defn print-reason
  "Provides special case for printing negative lookahead reasons"
  [r]
  (cond
    (:NOT r)
    (str "NOT " (:NOT r))
    (:char-range r)
    (instaparse.print/char-range->str r)
    (instance? java.util.regex.Pattern r)
    (instaparse.print/regexp->str r)
    :else
    (str r)))

(defn message
  "Given a file-name, line-number, and failed parse map,
   throw an Exception with an informative message."
  [file-name line-number {:keys [column text reason]}]
  (string/join
   "\n"
   (concat
    [(util/format "Parse error in '%s' at line %d:" file-name line-number)
     text
     (instaparse.failure/marker column)
     "Expected:"]
    (->> reason
         (filter :full)
         (map :expecting)
         (map (fn [r] (str (print-reason r) " (followed by end-of-string)"))))
    [""])))

(defn annotate-parse
  "Given a parse vector,
   return a map with the special key-value pairs
   for this type of parse."
  [parse]
  (case (first parse)
    :BASE_BLOCK
    {:iri (get-in parse [3 2])}

    :PREFIX_BLOCK
    {:prefix (get-in parse [3 1])
     :iri    (get-in parse [5 2])}

    :LABEL_BLOCK
    {:identifier (get-in parse [3 1])
     :label      (get-in parse [5 1])}

    :TYPE_BLOCK
    {:predicate (get-in parse [3 1])
     :datatype  (get-in parse [5 1])}

    :GRAPH_BLOCK
    (case (count parse)
      3 {}
      5 {:graph (get-in parse [3 1])}
      {})

    :SUBJECT_BLOCK
    {:subject (get-in parse [1 1])}

    :LITERAL_BLOCK
    (merge
     {:arrows    (get-in parse [1 1])
      :predicate (get-in parse [2 1])
      :content   (get-in parse [4 1])}
     (case (count (get-in parse [4]))
       2 {}
       3 {:language (get-in parse [4 2 1])}
       4 {:datatype (get-in parse [4 3 1])}
       {}))

    :LINK_BLOCK
    {:arrows    (get-in parse [1 1])
     :predicate (get-in parse [2 1])
     :object    (get-in parse [4 1])}

    :EXPRESSION_BLOCK
    {:predicate  (get-in parse [1 1])
     :expression (get-in parse [3])}

    ; else
    {}))

(defn parse-block
  "Given a file-name, line-number, and block to parse,
   return a map with the parse information
   or throw a parsing exception."
  ([[file-name line-number block]]
   (let [parse (block-parser block)]
     (if (insta/failure? parse)
       (util/throw-exception (message file-name line-number parse))
       (merge
        {:file-name   file-name
         :line-number line-number
         :block       block
         :block-type  (-> parse second first)
         :parse       (second parse)
         :eol         (-> parse second last last)}
        (annotate-parse (second parse))))))
  ([file-name line-number block]
   (parse-block [file-name line-number block])))

(defn merge-lines
  "Given a file name,
   return a stateful transducer
   that takes a sequence of lines,
   merges indented and blank lines,
   then emits a sequence of vectors:
   [file-name line-number merged-line]"
  [file-name]
  (fn [xf]
    (let [number (volatile! 0)
          length (volatile! 1)
          unit   (volatile! nil)]
      (fn
        ([] (xf))
        ([result] (xf result [file-name @number @unit]))
        ([result line]
         (cond
           (.startsWith line "  ")
           (do
             (vswap! length inc)
             (vreset! unit (str @unit "\n" (subs line 2)))
             result)

           (string/blank? line)
           (do
             (vswap! length inc)
             (vreset! unit (str @unit "\n" line))
             result)

           :else
           (let [current-line @number
                 current-unit @unit]
             (vreset! number (+ current-line @length))
             (vreset! length 1)
             (vreset! unit line)
             (if current-unit
               (xf result [file-name current-line current-unit])
               result))))))))
