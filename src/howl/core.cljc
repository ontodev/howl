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
;; 5. (map expand-block)
;;     - this step expands prefixed names into literals, and

;; Principal Datastructure
;; type block = {:env environment :exp parse-tree}
;; type parse-tree = Vector (keyword | string | parse-tree)
;; type env = {labels prefixes graph subject}

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
         (lazy-seq
          (cons (with-meta
                  {:exp (first (insta/parse block-parser (string/join \newline g)))}
                  {:origin {:name source :line ct}})
                (rec (rest groups) (+ ct (count g))))))))
   (group-lines lines) 1))

(defn parse-tree->string [parse-tree]
  "Takes a parse-tree that came from the block-parser, and
   returns the corresponding string"
  (if (string? parse-tree)
    parse-tree
    (case (first parse-tree)
      (:LABEL :PREFIX :EOL :SPACES :ABSOLUte_IRI) (second parse-tree)
      (string/join (map parse-tree->string (rest parse-tree))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Condense step
(defn condense-tree [parse-tree]
  "Takes a parse-tree and condenses single-character strings into a single string."
  (if (or (string? parse-tree) (keyword? parse-tree) (nil? parse-tree) (number? parse-tree))
    parse-tree
    (case (first parse-tree)
      :IRIREF [:IRIREF (string/join (rest parse-tree))]
      :LITERAL [:LITERAL (string/join (butlast (rest parse-tree))) (last parse-tree)]
      (:EOL) parse-tree
      (vec (map condense-tree parse-tree)))))

(defn condense-chars [block]
  (assoc block :exp (condense-tree (block :exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Parsing expressions
(defn parse-expressions [parse-tree]
  "Takes a parse-tree. Runs a separate parser on
   any contained :EXPRESSION_BLOCKs"
  (if (= :EXPRESSION_BLOCK (first parse-tree))
    (vec
     (map (fn [elem]
            (if (and (vector? elem)
                     (= :EXPRESSION (first elem)))
              (exp/string-to-parse (second elem))
              elem))
          parse-tree))
    parse-tree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Extracting environments
(defn first-vector-starting-with [keyword parse-tree]
  (first (filter #(and (vector? %) (= keyword (first %))) parse-tree)))

(defn contents-of-vector [keyword parse-tree]
  (second (first-vector-starting-with keyword parse-tree)))

(defn name-from-node [env node]
  "Takes an environment and a node, and returns the absolute name
   contained in the node.
   Does not work on all nodes, just :BLANK_NODE, :ABSOLUTE_IRI,
   :WRAPPED_IRI or :PREFIXED_NAME nodes"
  (case (first node)
    :BLANK_NODE (get node 2)
    :ABSOLUTE_IRI (second node)
    :WRAPPED_IRI (let [iri (get node 2)]
                   (if (.startsWith iri "http")
                     iri
                     (str (env :base) iri)))
    :PREFIXED_NAME (str (get-in env [:prefixes (second node)])
                        (get node 3))))

(defn maybe-name [env keyword parse-tree dest-keyword]
  (if-let [cont (contents-of-vector keyword parse-tree)]
    {dest-keyword (name-from-node env cont)}
    {}))

(defn parse-tree->names [env parse-tree]
  "Takes an environment and a parse tree.
   Returns the names found in the parse tree.
   This function needs an environment, because the given parse-tree
   might contain relative/prefixed names which need to be expanded."
  (case (first parse-tree)
    :PREFIX_BLOCK {:prefixes
                   {(contents-of-vector :PREFIX parse-tree)
                    (name-from-node env (contents-of-vector :PREFIXED parse-tree))}}
    :LABEL_BLOCK {:labels
                  {(contents-of-vector :LABEL parse-tree)
                   (name-from-node env (contents-of-vector :IDENTIFIER parse-tree))}}
    :GRAPH_BLOCK (maybe-name env :GRAPH parse-tree :graph)
    :SUBJECT_BLOCK (maybe-name env :SUBJECT parse-tree :subject)
    :BASE_BLOCK (maybe-name env :BASE parse-tree :base)
    {}))

(defn merge-environments [a b]
  "Merges two environments.
   We could dispense with this if we wanted to introduce an extra
   level of map for the keys [:graph :subject :base]."
  {:prefixes (merge (a :prefixes) (b :prefixes))
   :labels (merge (a :labels) (b :labels))
   :graph (or (b :graph) (a :graph))
   :subject (or (b :subject) (a :subject))
   :base (or (b :base) (a :base))})

(defn environments
  "Takes a sequence of blocks and a starting environment.
   Returns a sequence of blocks decorated with local environments."
  ([blocks] (environments blocks {}))
  ([blocks env]
   (when (not (empty? blocks))
     (lazy-seq
      (let [block (first blocks)
            next-env (merge-environments env (parse-tree->names env (block :exp)))]
        (cons (assoc block :env next-env)
              (environments (rest blocks) next-env)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Name expansion
(defn expand-tree
  "Takes an environment and a parse tree.
   Returns a parse tree with WRAPPED_IRIs and PREFIXED_NAMEs
   expanded to ABSOLUTE_IRIs"
  [env parse-tree]
  (if (or (keyword? parse-tree) (string? parse-tree) (number? parse-tree))
    parse-tree
    (case (first parse-tree)
      (:WRAPPED_IRI :PREFIXED_NAME) [:ABSOLUTE_IRI (name-from-node env parse-tree)]
      (map #(expand-tree env %)))))

(defn expand-block [block]
  (assoc block :exp (expand-tree (block :env) (block :exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Top-level interface
(defn parse-file
  ([filename] (parse-file filename {}))
  ([filename starting-env]
   (->> (line-seq (clojure.java.io/reader filename))
        (#(lines->parse-trees % :source filename))
        (map condense-chars)
        (map parse-expressions)
        (#(environments % starting-env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Converting to nquads
(defn block->nquads [block])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Error basics
(defn locate [block]
  (if-let [m (meta block)]
    (m :origin)
    (str block)))
