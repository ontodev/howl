(ns howl.core
  "Parse HOWL."
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [howl.util :as util]))

;; HOWL is a human-readable format for RDF and OWL.
;; It is a pretty flat "block"-based format,
;; designed to be processed in streams.
;;
;; There are three key data structures:
;;
;; 1. state: a map that tracks the processing state
;;    and the context required to expand and contract names.
;;
;; 2. block: a map representing a few consecutive lines;
;;    the fundamental unit of HOWL meaning.
;;
;; 3. parse: a nested vector generated by Instaparse.
;;    The first element of each vector is a keyword with the type.
;;
;; We break HOWL processing down into a number of different reducing functions,
;; each taking a state and an input (usually a block),
;; and returning an updated state with a :results key.
;; When we want to process streams of HOWL,
;; we use these reducing functions to define transducers
;; that track the state internally,
;; and just emit the blocks we're interested in.
;;
;; Key processing steps include:
;;
;; 1. merge blank and indented lines
;; 2. parse lines
;; 3. annotate the parse block
;; 4. expand names to absolute IRIs
;; 5. contract absolute names to IRIs
;; 6. convert blocks to quads
;; 7. convert quads to blocks


;; ## State Functions

(defn locate
  "Given a state map,
   try to return a location string
   that's useful for error reporting."
  [{:keys [file-name line-number line block] :as state}]
  (str
   (when file-name   (util/format "in '%s'" file-name))
   (when (and file-name line-number) " ")
   (when line-number (util/format "at line %d" line-number))
   (when (or file-name line-number) ":\n")
   (or line block state)))

(defn report-error
  "Given a state and one or more error strings,
   update the state with a vector of :errors."
  [state & messages]
  (update state :errors (fnil concat []) messages))

(defn ensure-sorted-iri-prefix
  "Given a state,
   ensure that it contains a special sorted-map for :iri-prefix."
  [state]
  (if (map? (:iri-prefix state))
    state
    (assoc state :iri-prefix (sorted-map-by (fn [a b] (> (count a) (count b)))))))

(defn find-prefix
  "Given a state map with a sorted :iri-prefix map,
   return the first [prefix-iri prefix] pair
   for which the given iri starts with the prefix-iri."
  [state iri]
  (->> state
       :iri-prefix
       (filter
        (fn [[prefix-iri prefix]]
          (.startsWith iri prefix-iri)))
       first))


;; ## General Processing Functions

(defn line-to-block-transducer
  "Given a processing function
   (that takes a state map and a line, and returns updated state with a :block key),
   a starting state, and a sequence of lines,
   return stateful transducer that emits a sequence of blocks."
  [processing-function starting-state]
  (fn
    [xf]
    (let [state (volatile! starting-state)]
      (fn
        ([] (xf))
        ([result] (xf result (:block (processing-function @state "EOL"))))
        ([result line]
         (let [new-state (processing-function @state line)]
           (when (:errors new-state)
             (util/throw-exception
              (string/join " " (:errors new-state))
              (locate new-state)))
           (vreset! state new-state)
           (if (:block new-state)
             (xf result (:block new-state))
             result)))))))

(defn lines-to-blocks
  "Given a processing function
   (that takes a state map and a line, and returns updated state with a :block key),
   a starting state, and a sequence of lines,
   return a sequence of blocks."
  [processing-function starting-state lines]
  (transduce
   (line-to-block-transducer processing-function starting-state)
   conj
   []
   lines))


;; The first step when processing HOWL data
;; is to merge blank and indented lines into 'units'
;; that start at the beginning of the line.

(defn merge-line
  "Given a state map and a line string
   return the updated state
   with a :block with a :line string if a merge was completed,
   otherwise no :block."
  [state line]
  (cond
   (not (string? line))
   (report-error
    state
    (util/format "Line '%s' is not a string" line))

   (.startsWith line "  ")
   (-> state
       (update :merging-lines (fnil conj []) (subs line 2))
       (dissoc :block))

   (string/blank? line)
   (-> state
       (update :merging-lines (fnil conj []) line)
       (dissoc :block))

   :else
   (-> state
       (update :line-number (fnil + 1) (count (get state :merging-lines)))
       (assoc :block {:line (string/join "\n" (:merging-lines state))})
       (assoc :merging-lines [line]))))


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
    LANG          = '@' #'(\\w|-)+'
    COLON         = #' *' ':'  #' +'
    COLON_ARROW   = #' *' ':>' #' +'
    COLON_ARROWS  = #' *' ':>>' #' +'
    SPACES        = #' +'
    ARROWS        = #'>*' #'\\s*'
    LABEL         = #'[^:\n]+'
    EOL           = #'(\r|\n|\\s)*'
    "))

(defn instaparse-reason
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

(defn instaparse-message
  "Given a failed parse map,
   return an informative error message."
  [{:keys [column text reason] :as parse}]
  (string/join
   "\n"
   (concat
    [text
     (instaparse.failure/marker column)
     "Expected:"]
    (->> reason
         (map :expecting)
         (map (fn [r] (str (instaparse-reason r) " (followed by end-of-string)"))))
    [""])))

(defn parse-block
  "Given a state map,
   if it has a :block key with a :line key,
   parse it and add a :parse key to the :block,
   or report an error."
  [state]
  (if-let [line (get-in state [:block :line])]
    (let [parse (block-parser line)]
      (if (insta/failure? parse)
        (report-error
         state
         "Parsing error"
         (instaparse-message parse))
        (assoc-in state [:block :parse] (second parse))))
    state))


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
       {:lang (get-in parse [5 2])}
       :DATATYPE
       {:type (get-in parse [5 1])}))

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
      :value     (get-in parse [4 1])}
     (case (count (get-in parse [4]))
       2 {}
       3 {:lang (get-in parse [4 2 2])}
       4 {:type (get-in parse [4 3 1])}
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

(defn annotate-block
  "Given a state map,
   if it has a :block key with a :parse key,
   add more annotations to the :block."
  [state]
  (if-let [parse (get-in state [:block :parse])]
    (assoc
     state
     :block
     (merge
      (:block state)
      {:block-type (-> parse first)
       :eol        (if (= "" (-> parse last last))
                     "\n"
                     (-> parse last last))}
      (annotate-parse parse)))
    state))



;; The next step might be to resolve all names to absolute IRIs,
;; which is what we want to do when converting HOWL to N-Quads.
;; To do that, we need to track: BASE, PREFIX, LABEL, and TYPE blocks,
;; and rdfs:label statements.

(defn expand-wrapped-iri
  "Given a state map and a WRAPPED_IRI parse vector,
   return an absolute IRI string,
   or throw an exception."
  [state [type left iri right]]
  (cond
    (and (string? iri) (re-matches #"^\w+://\S+$" iri))
    iri
    (and (string? iri) (string? (:base state)))
    (str (:base state) iri)
    :else
    (util/throw-exception
     (util/format
      "Could not expand wrapped IRI '%s%s%s' with BASE '%s'"
      left iri right (:base state))
     (locate state))))

(defn expand-prefixed-name
  "Given a state map and a PREFIXED_NAME parse vector,
   return an absolute IRI string,
   or throw an exception."
  [state [type prefix colon local-name]]
  (let [iri (get-in state [:prefix-iri prefix])]
    (if (and (string? iri) (string? local-name))
      (str iri local-name)
      (util/throw-exception
       (util/format
        "Could not expand prefixed name '%s%s%s'"
        prefix colon local-name)
       (locate state)))))

(defn expand-label
  "Given a state map and a LABEL parse vector,
   return an absolute IRI string,
   or throw an exception."
  [state [type label]]
  (let [iri (get-in state [:label-iri label])]
    (if (string? iri)
      iri
      (util/throw-exception
       (util/format "Could not expand label '%s'" label)
       (locate state)))))

(defn check-absolute
  "Given an IRI string,
   return it if it is absolute or blank,
   otherwise throw an exception."
  [state iri]
  (if (and (string? iri)
           (re-matches #"^_:\S+$|^mailto:\S+$|^\w+://\S+$" iri))
    iri
    (util/throw-exception
     (util/format "Expanded IRI '%s' is not absolute or blank" iri)
     (locate state))))

(defn expand
  "Given a state map and a parse vector for a name,
   return an absolute IRI string,
   or throw an exception."
  [state name]
  (check-absolute
   state
   (case (first name)
     :ABSOLUTE_IRI
     (second name)
     :WRAPPED_IRI
     (expand-wrapped-iri state name)
     :PREFIXED_NAME
     (expand-prefixed-name state name)
     :LABEL
     (expand-label state name)
     ; else
     (util/throw-exception
      (util/format "Could not expand name '%s'" name)
      (locate state)))))

; TODO: Remove this
(defn resolve-name
  [state block name]
  (expand state name))


;; To track the rdfs:labels, we need to know what a valid label is.

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

(defn expand-name
  "Given a state map and a parse vector for a name,
   expand the name to an absolute IRI or blank node,
   check the result and return a parse vector,
   or throw an exception."
  [state name]
  [:ABSOLUTE_IRI (expand state name)])

(defn expand-manchester-labels
  "Given a nested Manchester expression parse vector,
   return with labels replaced at all depths."
  [state expression]
  (clojure.walk/postwalk
   (fn [x]
     (cond
      (and (vector? x) (= :MN_LABEL (first x)))
      (expand-name state [:LABEL (second x)])
      (and (vector? x) (= :MN_QUOTED_LABEL (first x)))
      (expand-name state [:LABEL (get x 2)])
      :else
      x))
   expression))

(defn expand-names
  "Given a state map,
   if it has a :block key
   expand any names and return the updated state
   or throw an error."
  [state]
  (if-let [block (get state :block)]
    (case (:block-type block)
      :BASE_BLOCK
      (let [absolute (expand-name state (:base block))]
        (-> state
            (assoc :base (second absolute))
            (assoc-in [:block :base] absolute)))

      :PREFIX_BLOCK
      (let [absolute (expand-name state (:prefixed block))]
        (-> state
            ensure-sorted-iri-prefix
            (assoc-in [:prefix-iri (:prefix block)] (second absolute))
            (assoc-in [:iri-prefix (second absolute)] (:prefix block))
            (assoc-in [:block :prefixed] absolute)))

      :LABEL_BLOCK
      (let [absolute (expand-name state (:identifier block))]
        (-> state
            (assoc-in [:label-iri (:label block)] (second absolute))
            (assoc-in [:iri-label (second absolute)] (:label block))
            (assoc-in [:block :identifier] absolute)))

      :TYPE_BLOCK
      (let [absolute (expand-name state (:predicate block))]
        (cond
         (:lang block)
         (-> state
             (assoc-in [:iri-type (second absolute) :lang] (:lang block))
             (assoc-in [:block :predicate] absolute))
         (:type block)
         (let [datatype (expand-name state (:type block))]
           (-> state
               (assoc-in [:iri-type (second absolute) :type] (second datatype))
               (assoc-in [:block :predicate] absolute)
               (assoc-in [:block :type] datatype)))))

      :GRAPH_BLOCK
      (if (:graph block)
        (let [absolute (expand-name state (:graph block))]
          (-> state
              (assoc :current-graph (second absolute))
              (assoc :current-subject (second absolute))
              (assoc-in [:block :graph] absolute)))
        (dissoc state :current-graph :current-subject))

      :SUBJECT_BLOCK
      (let [absolute (expand-name state (:subject block))]
        (-> state
            (assoc :current-subject (second absolute))
            (assoc-in [:block :subject] absolute)))

      :LITERAL_BLOCK
      (let [state (if (:type block)
                    (assoc-in state [:block :type] (expand-name state (:type block)))
                    state)
            absolute (expand-name state (:predicate block))]
        (if (and (= (second absolute) "http://www.w3.org/2000/01/rdf-schema#label")
                 (string? (:current-subject state))
                 (valid-label? (:value block)))
          (-> state
              (assoc-in [:label-iri (:value block)] (:current-subject state))
              (assoc-in [:iri-label (:current-subject state)] (:value block))
              (assoc-in [:block :predicate] absolute))
          (assoc-in state [:block :predicate] absolute)))

      :LINK_BLOCK
      (-> state
          (assoc-in [:block :predicate] (expand-name state (:predicate block)))
          (assoc-in [:block :object]    (expand-name state (:object block))))

      :EXPRESSION_BLOCK
      (-> state
          (assoc-in [:block :predicate] (expand-name state (:predicate block)))
          (assoc-in
           [:block :expression]
           (expand-manchester-labels state (:expression block))))

      ;else
      state)
    state))


;; We can also change absolute IRIs back into names,
;; which is what we want to do when converting N-Quads to HOWL.
;; Again we define a reducing function that tracks the state,
;; and a transducer that just returns the sequence of blocks.


(defn get-name
  "Given a state map and an IRI string,
   return the best name:
   label, blank node, prefixed name, wrapped (relative) iri, or absolute IRI."
  [state block name]
  (let [iri (resolve-name state block name)]
    (cond
     (get-in state [:iri-label iri])
     [:LABEL (get-in state [:iri-label iri])]

     (find-prefix state iri)
     (let [[prefix-iri prefix] (find-prefix state iri)]
       [:PREFIXED_NAME prefix ":" (subs iri (count prefix-iri))])

     (and (:base state)
          (.startsWith iri (:base state)))
     [:WRAPPED_IRI "<" (subs iri (count (:base state))) ">"]

     :else
     [:ABSOLUTE_IRI iri])))

(defn rename
  "Given a state map and a block,
   return the block with nicer names."
  [state block]
  (case (:block-type block)
    :GRAPH_BLOCK
    (if (:graph block)
      (assoc
       block
       :graph (get-name state block (:graph block)))
      block)

    :SUBJECT_BLOCK
    (assoc block :subject (get-name state block (:subject block)))

    :LITERAL_BLOCK
    (assoc block :predicate (get-name state block (:predicate block)))

    :LINK_BLOCK
    (assoc
     block
     :predicate
     (get-name state block (:predicate block))
     :object
     (get-name state block (:object block)))

    :EXPRESSION_BLOCK
    (assoc
     block
     :predicate
     (get-name state block (:predicate block))
     :expression
     (clojure.walk/postwalk
      (fn [x]
        (let [label (get-in state [:iri-label x])]
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

(defn space-blocks
  [blocks this-block]
  (let [this-type  (:block-type this-block)
        last-block (last blocks)
        last-type  (:block-type last-block)
        last-eol   (:eol last-block)]
    (cond
     (and (nil? last-block)
          (= :BLANK_BLOCK this-type)
          (= "\n" (:eol this-block)))
     []

     (and (= :PREFIX_BLOCK last-type)
          (not= :PREFIX_BLOCK this-type))
     (conj (vec (butlast blocks))
           (assoc last-block :eol (str last-eol "\n"))
           this-block)

     (and (= :LABEL_BLOCK last-type)
          (not= :LABEL_BLOCK this-type))
     (conj (vec (butlast blocks))
           (assoc last-block :eol (str last-eol "\n"))
           this-block)

     (and (= :TYPE_BLOCK last-type)
          (not= :TYPE_BLOCK this-type))
     (conj (vec (butlast blocks))
           (assoc last-block :eol (str last-eol "\n"))
           this-block)

     (and last-type
          (= :GRAPH_BLOCK this-type))
     (conj (vec (butlast blocks))
           (assoc last-block :eol (str last-eol "\n"))
           this-block)

     (and last-type
          (= :SUBJECT_BLOCK this-type))
     (conj (vec (butlast blocks))
           (assoc last-block :eol (str last-eol "\n"))
           this-block)

     :else
     (conj blocks this-block))))

(defn block-to-line
  "Given a block map, return a printable HOWL string."
  [block]
  (case (:block-type block)
    :BLANK_BLOCK
    (:eol block)

    :COMMENT_BLOCK
    (str (:hash block)
         (:comment block)
         (:eol block))

    :BASE_BLOCK
    (str "BASE "
         (render-name (:base block))
         (:eol block))

    :PREFIX_BLOCK
    (str "PREFIX "
         (:prefix block)
         ":> "
         (render-name (:prefixed block))
         (:eol block))

    :LABEL_BLOCK
    (str "LABEL "
         (render-name (:identifier block))
         ": "
         (:label block)
         (:eol block))

    :TYPE_BLOCK
    (str "TYPE "
         (render-name (:predicate block))
         ":> "
         (when (:lang block) (str "@" (:lang block)))
         (when (:type block) (render-name (:type block)))
         (:eol block))

    :GRAPH_BLOCK
    (str "GRAPH"
         (when (:graph block) (str " " (render-name (:graph block))))
         (:eol block))

    :SUBJECT_BLOCK
    (str (render-name (:subject block))
         (:eol block))

    :LITERAL_BLOCK
    (str (:arrows block)
         (render-name (:predicate block))
         ": "
         (:value block)
         (when (:lang block) (str "@" (:lang block)))
         (when (:type block) (str "^^" (render-name (:type block))))
         (:eol block))

    :LINK_BLOCK
    (str (:arrows block)
         (render-name (:predicate block))
         ":> "
         (render-name (:object block))
         (:eol block))

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
              (apply str))
         (:eol block))

    ;else
    ""))

(defn render-howl
  [state blocks]
  (->> blocks
       (map (partial rename state))
       (reduce space-blocks [])
       (map block-to-line)
       (apply str)))
