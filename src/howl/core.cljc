(ns howl.core
  "Parse HOWL."
  (:require [clojure.string :as string]
            [instaparse.core :as insta]

            [howl.util :as util]
            [howl.expression :as exp]))

;; Parsing works like this:
;;
;; 1. lines->parse-trees
;;     - converts a lazy seq of lines to a lazy seq of {:exp [:PARSE_TREE ...]},
;;       complete with metadata designating origin name/line-number
;;     - groups lines into blocks before parsing each block (done in one step, 'cause
;;       it's otherwise difficult to factor in the error reporting hooks we need)
;;
;; 2. (map condense-chars)
;;     - some parse blocks emit characters. As in, things like [:TAG "f" "o" "o" "b" "a" "r"].
;;       This step condenses such blocks down to [:TAG "foobar"]
;;
;; 3. (map parse-expressions)
;;     - some blocks contain sub-expressions expressed in a different syntax.
;;       The preliminary parse leaves such expressions as strings to be dealt
;;       with separately. This step deals with them.
;;
;; 4. environments
;;     - some blocks introduce new bindings to be used by other expressions. Mostly prefixes
;;       and new labels.
;;       This step extracts those bindings and puts them in the appropriate :env key.
;;     - Each environment includes the previous, and environments survive past end of file for
;;       usability reasons.
;;     - Each name is valid from the point where it's declared to the point where it's shadowed.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Preliminary parse
(defn group-lines [lines]
  "Given a sequence of lines, returns a lazy sequence of grouped lines"
  (partition-by
   (let [ct (volatile! 0)]
     #(do (when (not (or (string/blank? %) (.startsWith % "  ")))
            (vswap! ct inc))
          @ct))
   lines))

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

(defn lines->parse-trees
  "Takes a seq of lines. Returns a seq of {:exp [:PARSE_TREE ...]} objects."
  [lines & {:keys [source] :or {source "interactive"}}]
  ((fn rec [groups ct]
     (when (not (empty? groups))
       (let [g (first groups)]
         (println "DEALING WITH" (first (insta/parse block-parser (string/join g))))
         (lazy-seq
          (cons (with-meta
                  {:exp (first (insta/parse block-parser (string/join g)))}
                  {:origin {:name source :line ct}})
                (rec (rest groups) (+ ct (count g))))))))
   (group-lines lines) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Condense step
(defn condense-literal-block [parsed-literal-block]
  (vec
   (map (fn [elem]
          (if (and (vector? elem)
                   (> (count elem) 2)
                   (= :LITERAL (first elem)))
            [:LITERAL (string/join (butlast (rest elem))) (last elem)]
            elem))
        parsed-literal-block)))

(defn condense-chars [block]
  (case (first (block :exp))
    :LITERAL_BLOCK (assoc block :exp (condense-literal-block (block :exp)))
    block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Parsing expressions
(defn parse-expressions [parsed-block]
  (if (= :EXPRESSION_BLOCK (first parsed-block))
    (vec
     (map (fn [elem]
            (if (and (vector? elem)
                     (= :EXPRESSION (first elem)))
              (exp/string-to-parse (second elem))
              elem))
          parsed-block))
    parsed-block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Extracting environments
(defn parse-tree->names [parse-tree]
  (case (first parse-tree)
    :PREFIX_BLOCK (let [[_ _ _ [_ name] _ [_ val]] parse-tree]
                    {:prefixes {name val}})
    :LABEL_BLOCK (let [[_ _ _ val [ name]] parse-tree]
                   {:labels {name val}})
    {}))

(defn environments
  ([parsed-blocks] (environments parsed-blocks {}))
  ([parsed-blocks env]
   (when (not (empty? parsed-blocks))
     (lazy-seq
      (let [block (first parsed-blocks)
            next-env (merge-with merge env (parse-tree->names (block :exp)))]
        (cons (assoc block :env next-env)
              (environments (rest parsed-blocks) next-env)))))))

(defn parse-file [filename]
  (->> (line-seq (clojure.java.io/reader filename))
       (#(lines->parse-trees % :source filename))
       (map condense-chars)
       (map parse-expressions)
       environments))

(defn locate [block]
  "PLACEHOLDER ERROR HERE")
