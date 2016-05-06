(ns howl.core
  "Parse HOWL."
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [howl.util :as util]))

;; The first step when processing HOWL data
;; is to merge blank and indented lines into 'units'
;; that start at the beginning of the line.
;; We define a reducing function that takes a state and a line
;; and returns the pair of the update state and the merged line vector.
;; Then we define a transducer that applies this reducing function,
;; returning only the sequence of merged line vectors.

(defn get-merged
  "Given a line merging state, return the current merged line
   as a vector [file-name line-number merged-line]. "
  [{:keys [file-name number unit] :as state}]
  [file-name (or number 1) unit])

(defn merge-line
  "Given a merging state and a line
   return the pair of an updated state
   and a vector [file-name line-number merged-line]
   (or nil if we're in the middle of a merge)."
  [state line]
  (cond
   (.startsWith line "  ")
   [(-> state
        (update :length (fnil inc 0))
        (assoc :unit (str (:unit state)
                          (when (:unit state) "\n")
                          (subs line 2))))
    nil]

   (string/blank? line)
   [(-> state
        (update :length (fnil inc 0))
        (assoc :unit (str (:unit state)
                          (when (:unit state) "\n")
                          line)))
    nil]

   :else
   [(-> state
        (assoc :length 1)
        (assoc :number (+ (get state :number 1) (get state :length 1)))
        (assoc :unit line))
    (when (:unit state) (get-merged state))]))

(defn merge-lines
  "Given a file name,
   return a stateful transducer
   that takes a sequence of lines,
   merges indented and blank lines,
   then emits a sequence of vectors:
   [file-name line-number merged-line]"
  [file-name]
  (fn [xf]
    (let [state (volatile! {:file-name file-name})]
      (fn
        ([] (xf))
        ([result] (xf result (get-merged @state)))
        ([result line]
         (let [[new-state merged] (merge-line @state line)]
            (vreset! state new-state)
            (if merged
              (xf result merged)
              result)))))))


;; Parsing works like this:
;;
;; 1. loop over files
;; 2. loop over lines in file
;; 3. merge indented lines into a single "unit"
;; 4. parse the unit and emit a map
;;
;; The actual parsing is handled by Instaparse.

(def block-parser
  (insta/parser
   "BLOCK = BLANK_BLOCK / COMMENT_BLOCK /
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
    EXPRESSION_BLOCK = PREDICATE COLON_ARROWS MN_CLASS_EXPRESSION EOL

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

    IDENTIFIER = BLANK_NODE / PREFIXED_NAME / WRAPPED_IRI / ABSOLUTE_IRI
    BASE       = PREFIXED_NAME / WRAPPED_IRI / ABSOLUTE_IRI  / LABEL
    PREFIXED   = PREFIXED_NAME / WRAPPED_IRI / ABSOLUTE_IRI  / LABEL
    GRAPH      = BLANK_NODE / PREFIXED_NAME / WRAPPED_IRI / ABSOLUTE_IRI  / LABEL
    SUBJECT    = BLANK_NODE / PREFIXED_NAME / WRAPPED_IRI / ABSOLUTE_IRI / LABEL
    PREDICATE  = PREFIXED_NAME / WRAPPED_IRI / ABSOLUTE_IRI / LABEL
    OBJECT     = BLANK_NODE / PREFIXED_NAME / WRAPPED_IRI / ABSOLUTE_IRI / LABEL
    DATATYPE   = PREFIXED_NAME / WRAPPED_IRI / ABSOLUTE_IRI / LABEL
    LITERAL    = #'.+(?=@(\\w|-)+)' LANG /
                 #'.+(?=\\^\\^\\S+)' '^^' DATATYPE /
                 #'(\n|.)+.+'

    PREFIX        = #'(\\w|-)+'
    BLANK_NODE    = '_:' #'(\\w|-)+'
    PREFIXED_NAME = #'(\\w|-)+' ':' #'[^\\s:/][^\\s:]*'
    WRAPPED_IRI   = '<' #'[^>\\s]+' '>'
    ABSOLUTE_IRI  = #'\\w+:/[^>\\s]+'
    LANG          = #'@(\\w|-)+'
    COLON         = #' *' ':'  #' +'
    COLON_ARROW   = #' *' ':>' #' +'
    COLON_ARROWS  = #' *' ':>>' #' +'
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
    (.startsWith label "#") false
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


;; Once we have the parse vector,
;; we process that into a nicer "block map".


(defn annotate-parse
  "Given a parse vector,
   return a map with the special key-value pairs
   for this type of parse."
  [parse]
  (case (first parse)
    :COMMENT_BLOCK
    {:hash    (get-in parse [1])
     :comment (get-in parse [2])}

    :BASE_BLOCK
    {:base (get-in parse [3 1])}

    :PREFIX_BLOCK
    {:prefix   (get-in parse [3 1])
     :prefixed (get-in parse [5 1])}

    :LABEL_BLOCK
    {:identifier (get-in parse [3 1])
     :label      (get-in parse [5 1])}

    :TYPE_BLOCK
    (merge
     {:predicate (get-in parse [3 1])}
     (case (get-in parse [5 0])
       :LANG
       {:language (get-in parse [5 1])}
       :DATATYPE
       {:datatype (get-in parse [5 1])}))

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


;; The next step might be to resolve all names to absolute IRIs,
;; which is what we want to do when converting HOWL to N-Quads.
;; We define a reducing function from state and block to [state block] pairs,
;; then define a transducer that just returns the blocks.
;; These keep track of the labels and prefixes as they are defined.
;; TODO

(defn reverse-labels
  "Given a state map,
   return it with a :reverse-labels map added
   that maps from IRI to label."
  [state]
  (->> state
       :labels
       (map (juxt second first))
       (into {})
       (assoc state :reverse-labels)))

(defn prefix-sequence
  "Given a state map,
   return it with a :prefix-sequence vector added
   where each entry is a [prefix-iri prefix] pair,
   sorted from longest prefix-iri to shortest."
  [state]
  (->> state
       :prefixes
       (map (juxt second first))
       (sort-by (comp count first) >)
       (assoc state :prefix-sequence)))

(defn find-prefix
  "Given a state map with a :prefix-sequence vector and an IRI string,
   return the first [prefix-iri prefix] pair
   for which the given iri starts with the prefix-iri."
  [state iri]
  (->> state
       :prefix-sequence
       (filter
        (fn [[prefix-iri prefix]]
          (.startsWith iri prefix-iri)))
       first))

(defn resolve-iri
  "Given a state map, a block map, and a WRAPPED_IRI parse vector,
   return an absolute IRI string,
   or throw an exception."
  [state block [type left iri right]]
  (cond
    (re-matches #"^\w+://\S+$" iri)
    iri
    (string? (:base state))
    (str (:base state) iri)
    :else
    (util/throw-exception
     (util/format
      "Could not resolve relative IRI '%s' with BASE '%s' in '%s' at line %d:\n%s"
      iri
      (:base state)
      (:file-name block)
      (:line-number block)
      (or (:line block) block)))))

(defn resolve-prefixed-name
  "Given a state map, a block map, and a PREFIXED_NAME parse vector,
   return an absolute IRI string,
   or throw an exception."
  [state block [type prefix colon local-name]]
  (let [iri (get-in state [:prefixes prefix])]
    (if (string? iri)
      (str iri local-name)
      (util/throw-exception
       (util/format
        "Could not resolve prefixed name '%s' in '%s' at line %d:\n%s"
        (str prefix colon local-name)
        (:file-name block)
        (:line-number block)
        (or (:line block) block))))))

(defn resolve-label
  "Given a state map, a block map, and a LABEL parse vector,
   return an absolute IRI string,
   or throw an exception."
  [state block [type label]]
  (let [iri (get-in state [:labels label])]
    (if (string? iri)
      iri
      (util/throw-exception
       (util/format
        "Could not resolve label '%s' in '%s' at line %d:\n%s"
        label
        (:file-name block)
        (:line-number block)
        (or (:line block) block))))))

(defn resolve-absolute
  "Given a state map, a block map, and a parse vector,
   return an absolute IRI string,
   or throw an exception."
  [state block name]
  (case (first name)
    :ABSOLUTE_IRI
    (second name)
    :WRAPPED_IRI
    (resolve-iri state block name)
    :PREFIXED_NAME
    (resolve-prefixed-name state block name)
    :LABEL
    (resolve-label state block name)
    ; else
    (util/throw-exception
     (util/format
      "Could not resolve name '%s' in '%s' at line %d:\n%s"
      name
      (:file-name block)
      (:line-number block)
      (or (:line block) block)))))

(defn resolve-name
  "Given a state map, a block map, and a parse vector,
   resolve the name to an absolute IRI or blank node,
   check the result and return it,
   or throw an exception."
  [state block name]
  (let [iri (resolve-absolute state block name)]
    (if (and (string? iri)
             (re-matches #"^_:\S+$|^mailto:\S+$|^\w+://\S+$" iri))
      iri
      (util/throw-exception
       (util/format
        "Resolved IRI '%s' is not absolute or blank in '%s' at line %d:\n%s"
        iri
        (:file-name block)
        (:line-number block)
        (or (:line block) block))))))


;; We can also change absolute IRIs back into names,
;; which is what we want to do when converting N-Quads to HOWL.
;; Again we define a reducing function that tracks the state,
;; and a transducer that just returns the sequence of blocks.


(defn get-name
  "Given a state map and an IRI string,
   return the best name:
   label, blank node, prefixed name, wrapped (relative) iri, or absolute IRI."
  [state iri]
  (cond
   (get-in state [:reverse-labels iri])
   [:LABEL (get-in state [:reverse-labels iri])]

   (find-prefix state iri)
   (let [[prefix-iri prefix] (find-prefix state iri)]
     [:PREFIXED_NAME prefix ":" (subs iri (count prefix-iri))])

   (and (:base state)
        (.startsWith iri (:base state)))
   [:WRAPPED_IRI "<" (subs iri (count (:base state))) ">"]

   :else
   [:ABSOLUTE_IRI iri]))

(defn rename
  "Given a state map and a block,
   return the block with nicer names."
  [state block]
  (case (:block-type block)
    :GRAPH_BLOCK
    (if (:graph block)
      (assoc
       block
       :graph (get-name state (:graph block)))
      block)

    :SUBJECT_BLOCK
    (assoc block :subject (get-name state (:subject block)))

    :LITERAL_BLOCK
    (assoc block :predicate (get-name state (:predicate block)))

    :LINK_BLOCK
    (assoc
     block
     :predicate
     (get-name state (:predicate block))
     :object
     (get-name state (:object block)))

    :EXPRESSION_BLOCK
    (assoc
     block
     :predicate
     (get-name state (:predicate block))
     :expression
     (clojure.walk/postwalk
      (fn [x]
        (let [label (get-in state [:reverse-labels x])]
          (cond
           (and (string? label) (re-find #"\s" label)) (str "'" label "'")
           label label
           :else x)))
      (:expression block)))

    ;else
    block))

(defn render-name
  "Given the parse vector for a name
   (IRI, blank node, prefixed name, or label)
   return that name as a printable HOWL string."
  [name]
  (apply str (rest name)))

(defn render-block
  "Given a block, return a printable HOWL string."
  [block]
  (case (:block-type block)
    :GRAPH_BLOCK
    (if (:graph block)
      (str "\nGRAPH " (render-name (:graph block)))
      "\nGRAPH")

    :SUBJECT_BLOCK
    (str "\n" (render-name (:subject block)))

    :LITERAL_BLOCK
    (str (:arrows block)
         (render-name (:predicate block))
         ": "
         (:content block))

    :LINK_BLOCK
    (str (:arrows block)
         (render-name (:predicate block))
         ":> "
         (render-name (:object block)))

    :EXPRESSION_BLOCK
    (str (:arrows block)
         (render-name (:predicate block))
         ":>> "
         (->> block
              :expression
              (#(if (and (= "(" (second %)) (= ")" (last %)))
                  (concat [(first %)] (drop 2 (butlast %)) )
                  %))
              flatten
              (filter string?)
              (apply str)))

    ;else
    (str block)))
