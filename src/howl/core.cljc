(ns howl.core
  "Parse HOWL."
  (:require [clojure.string :as string]
            [instaparse.core :as insta]

            [howl.util :as util]
            [howl.expression :as exp]))

;; Parsing works like this:
;;
;; 1. lines->blocks
;;     - converts a lazy seq of lines to a lazy seq of blocks
;;     - lines will be associated with the previous lines if there's indentation
;;
;; 2. (mapcat block-parser)
;;     - parses each block into a syntax tree represented by a vector
;; 3. (map pre-process-block)
;;     - replaces :EXPRESSION blocks with their parse trees, and condenses :LITERAL blocks
;;
;; 4. environments
;;     - pulls out name information from a series of blocks.
;;     - Each name is valid from the point where it's declared to the point where it's shadowed.
;;     - This section could be greatly simplified if we were ok with disallowing shadowing.
;;

(defn lines->blocks [lines]
  "Given a sequence of lines, returns a lazy sequence of blocks."
  (map (partial string/join \newline)
       (partition-by
        (let [ct (volatile! 0)]
          #(do (when (not (or (string/blank? %) (.startsWith % "  ")))
                 (vswap! ct inc))
               @ct))
        lines)))

(def block-parser
  (insta/parser
   "<BLOCK> = BLANK_BLOCK / COMMENT_BLOCK /
              BASE_BLOCK / PREFIX_BLOCK /
              LABEL_BLOCK / TYPE_BLOCK /
              GRAPH_BLOCK / SUBJECT_BLOCK /
              LITERAL_BLOCK / LINK_BLOCK / EXPRESSION_BLOCK

    BLANK_BLOCK      = EOL
    COMMENT_BLOCK    = #'#+\\s*' #'.*' EOL
    BASE_BLOCK       = 'BASE'   SPACES BASE EOL
    PREFIX_BLOCK     = 'PREFIX' SPACES PREFIX     COLON_ARROW PREFIXED EOL
    LABEL_BLOCK      = 'LABEL'  SPACES IDENTIFIER COLON       LABEL EOL
    TYPE_BLOCK       = 'TYPE'   SPACES PREDICATE  COLON_ARROW (LANG | DATATYPE) EOL
    GRAPH_BLOCK      = 'GRAPH'  EOL /
                       'GRAPH'  SPACES GRAPH EOL
    SUBJECT_BLOCK    = SUBJECT EOL
    LITERAL_BLOCK    = ARROWS PREDICATE COLON LITERAL EOL
    LINK_BLOCK       = ARROWS PREDICATE COLON_ARROW OBJECT EOL
    EXPRESSION_BLOCK = PREDICATE COLON_ARROWS EXPRESSION EOL
    EXPRESSION = #'(?:.|\r?\n)+'

    IDENTIFIER = BLANK_NODE / PREFIXED_NAME / WRAPPED_IRI / ABSOLUTE_IRI
    BASE       = PREFIXED_NAME / WRAPPED_IRI / ABSOLUTE_IRI  / LABEL
    PREFIXED   = PREFIXED_NAME / WRAPPED_IRI / ABSOLUTE_IRI  / LABEL
    GRAPH      = BLANK_NODE / PREFIXED_NAME / WRAPPED_IRI / ABSOLUTE_IRI  / LABEL
    SUBJECT    = BLANK_NODE / PREFIXED_NAME / WRAPPED_IRI / ABSOLUTE_IRI / LABEL
    PREDICATE  = PREFIXED_NAME / WRAPPED_IRI / ABSOLUTE_IRI / LABEL
    OBJECT     = BLANK_NODE / PREFIXED_NAME / WRAPPED_IRI / ABSOLUTE_IRI / LABEL
    DATATYPE   = PREFIXED_NAME / WRAPPED_IRI / ABSOLUTE_IRI / LABEL
    LITERAL    = CHAR+ LANG /
                 CHAR+ '^^' DATATYPE /
                 #'(\n|.)*.+'

    PREFIX        = #'(\\w|-)+'
    BLANK_NODE    = '_:' #'(\\w|-|:)+'
    PREFIXED_NAME = #'(\\w|-)+' ':' #'[^\\s:/][^\\s:]*'
    WRAPPED_IRI   = '<' #'[^>\\s]+' '>'
    ABSOLUTE_IRI  = #'\\w+:/[^>\\s]+'
    LANG          = '@' #'(\\w|-)+'
    COLON         = #' *' ':'  #' +'
    COLON_ARROW   = #' *' ':>' #' +'
    COLON_ARROWS  = #' *' ':>>' #' +'
    SPACES        = #' +'
    ARROWS        = #'>*' #'\\s*'
    LABEL         = #'[^:\n]+'
    EOL           = #'(\r|\n|\\s)*'
    <CHAR> = #'.'
    "))

(defn pre-process-literal-block [parsed-literal-block]
  (map (fn [elem]
         (if (and (vector? elem)
                  (> (count elem) 2)
                  (= :LITERAL (first elem)))
           [:LITERAL (string/join (butlast (rest elem))) (last elem)]
           elem))
       parsed-literal-block))

(defn pre-process-expression-block [parsed-expression-block]
  (map (fn [elem]
         (if (and (vector? elem)
                  (= :EXPRESSION (first elem)))
           (exp/string-to-parse (second elem))
           elem))
       parsed-expression-block))

(defn pre-process-block [parsed-block]
  (case (first parsed-block)
    :LITERAL_BLOCK (pre-process-literal-block parsed-block)
    :EXPRESSION_BLOCK (pre-process-expression-block parsed-block)
    parsed-block))

(defn parsed-block->names [parsed-block]
  (case (first parsed-block)
    :PREFIX_BLOCK (let [[_ _ _ [_ name] _ [_ val]] parsed-block]
                    {:prefixes {name val}})
    :LABEL_BLOCK (let [[_ _ _ val [ name]] parsed-block]
                   {:labels {name val}})
    {}))

(defn environments [parsed-blocks]
  (reverse
   (:collected
    (reduce
     (fn [{:keys [current collected]} parsed-block]
       (let [next (merge-with merge current (parsed-block->names parsed-block))]
         {:current next
          :collected (conj collected {:env next :exp parsed-block})}))
     {}
     parsed-blocks))))

(defn parse-file [filename]
  (->> (line-seq (clojure.java.io/reader filename))
       lines->blocks
       (mapcat block-parser ,,,)
       (map pre-process-block ,,,)
       environments))
