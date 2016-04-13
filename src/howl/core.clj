(ns howl.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            clojure.set
            [instaparse.core :as insta]
            [clojure.tools.cli :refer [parse-opts]])
  (:import [java.net URI])
  (:gen-class))

(def owl "http://www.w3.org/2002/07/owl#")
(def plain-literal "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral")
(def rdf-type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
(def rdfs-label "http://www.w3.org/2000/01/rdf-schema#label")
(def xsd-string "http://www.w4.org/2001/XMLSchema#string")

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
            LINK_BLOCK / LITERAL_BLOCK

    BASE_BLOCK       = 'BASE'   SPACES IRI EOL
    PREFIX_BLOCK     = 'PREFIX' SPACES PREFIX     COLON IRI EOL
    LABEL_BLOCK      = 'LABEL'  SPACES IDENTIFIER COLON LABEL EOL
    TYPE_BLOCK       = 'TYPE'   SPACES PREDICATE  COLON DATATYPE EOL
    GRAPH_BLOCK      = 'GRAPH'  EOL /
                       'GRAPH'  SPACES GRAPH EOL
    SUBJECT_BLOCK    = SUBJECT EOL
    LITERAL_BLOCK    = ARROWS PREDICATE COLON LITERAL EOL
    LINK_BLOCK       = ARROWS PREDICATE ARROW_COLON OBJECT EOL

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
    PREFIXED_NAME = #'(\\w|-)+' ':' #'(\\w|-)+'
    IRI           = '<' #'[^>\\s]+' '>'
    LANG          = #'@(\\w|-)+'
    COLON       = #' *' ':'  #' +'
    ARROW_COLON = #' *' ':>' #' +'
    SPACES      = #' +'
    ARROWS      = #'>*' #'\\s*'
    LABEL       = #'[^:\n]+'
    EOL         = #'(\r|\n|\\s)*'
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
    (.contains   label "\n") false
    (.contains   label "\t") false
    (.contains   label ": ") false
    (.contains   label ":> ") false
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
    [(format "Parse error in '%s' at line %d:" file-name line-number)
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

    :LINK_BLOCK
    {:arrows    (get-in parse [1 1])
     :predicate (get-in parse [2 1])
     :object    (get-in parse [4 1])}

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

    ; else
    {}))

(defn parse-block
  "Given a file-name, line-number, and block to parse,
   return a map with the parse information
   or throw a parsing exception."
  ([[file-name line-number block]]
   (let [parse (block-parser block)]
     (if (insta/failure? parse)
       (throw (Exception. (message file-name line-number parse)))
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

(defn parse-file
  "Given a file name,
   return a lazy sequence of parse results."
  [file-name]
  (with-open [reader (io/reader file-name)]
    (transduce
     (comp
      (merge-lines file-name)
      (map parse-block))
     conj
     (line-seq reader))))

(defn parse-files
  "Given a sequence of file names,
   return a lazy sequence of parse results."
  [& file-names]
  (mapcat parse-file file-names))


;; TODO: Reformat HOWL

(defmulti format-block :block-type)

(defmethod format-block :PREFIX_BLOCK
  [{:keys [PREFIX IRI]}]
  (format "PREFIX %s: %s" PREFIX IRI))

(defmethod format-block :default
  [block]
  (str block))



(defn print-parses
  "Given a sequence of file names,
   print a sequence of parse maps."
  [file-names]
  (->> (apply parse-files file-names)
       (map println)
       doall))


;; TODO: extract labels

;(defn collect-labels
;  [parse-vector]
;  (tree-seq #(= (first %) :LABEL) identity parse-vector))

;(defn defined-labels
;  [parse-vectors]
;  (->> parse-vectors
;       (filter #(contains? #{:LABEL_BLOCK :SUBJECT_BLOCK} (:type %)))
;       (map :LABEL)
;       set))

;(defn used-labels
;  [parse-vectors]
;  (->> parse-vectors
;       flatten
;       (partition 2 1)
;       (filter #(= :LABEL (first %)))
;       (map second)
;       set))

;(defn print-labels
;  [file-names]
;  (let [parse-vectors (apply parse-files file-names)]
;    (->> (clojure.set/difference
;          (used-labels parse-vectors)
;          (defined-labels parse-vectors))
;         ;(used-labels parse-vectors)
;         sort
;         (map println)
;         doall)))








;; The state map contains:
;;
;; 1. labels map from label string to IRI string
;; 2. prefixes map from prefix string to expanded string
;; 3. base-iri string
;; 5. quads vector of processed quads
;;
;; Names are resolved in this order.
;; It may also contain:
;;
;; 4. types map from predicates IRI string to datatype IRI string

(defn resolve-label
  "Given a state map with a labels map and a label,
   return the full IRI or nil."
  [{:keys [labels] :as state} label]
  (get labels label))

(defn resolve-prefixed-name
  "Given a state map with a prefixes map and a prefixed name,
   return the full IRI or nil."
  [{:keys [prefixes] :as state} prefixed-name]
  (let [[prefix suffix] (string/split prefixed-name #":" 2)]
    (when (find prefixes prefix)
      (str (get prefixes prefix) suffix))))

(defn resolve-relative-iri
  "Given a state map with a base-iri and an IRI string,
   return an absolute IRI string."
  [{:keys [base-iri] :as state} iri]
  ; TODO: make it work
  (if base-iri
    (.toString (.resolve (URI. base-iri) iri))
    iri))

(defn resolve-name
  "Given a state map and a name,
   try to resolve it as a label, then a prefix, then an IRI,
   and return the full IRI."
  [state name]
  (or (resolve-label         state name)
      (resolve-prefixed-name state name)
      (resolve-relative-iri  state name)))

;(defn resolve-name-2
;  [state block name]
;  (let [LABEL (second name)
;        {:keys [PREFIX LOCAL_NAME IRI] :as name-map}
;        (parse-vector-to-map name)]
;    (cond
;      (and PREFIX LOCAL_NAME)
;      (str (get-in state [:prefixes PREFIX]) LOCAL_NAME)
;      IRI
;      IRI
;      LABEL
;      (get-in state [:labels LABEL])
;      :else
;      (throw
;       (Exception.
;        (format "Could not resolve name '%s' in '%s' at line %d:\n%s"
;                name
;                (:file-name block)
;                (:line-number block)
;                (:line block)))))))

(defn resolve-name-3
  [state block name]
  (case (first name)
    :IRI
    (nth name 3)
    :PREFIXED_NAME
    (let [[type prefix colon local-name] name]
      (str (get-in state [:prefixes prefix]) local-name))
    :LABEL
    (get-in state [:labels (second name)])
    :else
    (throw
     (Exception.
      (format "Could not resolve name '%s' in '%s' at line %d:\n%s"
              name
              (:file-name block)
              (:line-number block)
              (:line block))))))

(defn format-literal
  [state {:keys [content language datatype] :as block}]
  (let [content (string/replace content "\n" "\\n")]
    (cond
      language
      (format "\"%s\"%s" content language)
      datatype
      (format "\"%s\"^^<%s>"
              content
              (resolve-name-3 state block datatype))
      :else
      (format "\"%s\"" content))))

(defn unwrap-iri
  [iri]
  (string/replace iri #"^<|>$" ""))

(def default-state
  {:blank-node-count 0
   :labels {"label" rdfs-label}})

(defn to-triples
  "Given a reducing function,
   return a stateful transducer
   that takes parse maps
   and returns NTriple statement strings."
  [xf]
  (let [state (volatile! default-state)]
    (fn
      ([] (xf))
      ([result] (xf result))
      ([result block]
       (try
         (case (:block-type block)
           :PREFIX_BLOCK
           (do
             (vswap! state assoc-in [:prefixes (:prefix block)] (:iri block))
             result)

           :LABEL_BLOCK
           (let [iri (resolve-name-3 @state block (:identifier block))]
             (vswap! state assoc-in [:labels (:label block)] iri)
             result)

           :TYPE_BLOCK
           (let [predicate-iri (resolve-name-3 @state block (:predicate block))
                 datatype-iri  (resolve-name-3 @state block (:datatype block))]
             (vswap! state assoc-in [:types predicate-iri] datatype-iri)
             result)

           :GRAPH_BLOCK
           (let [graph (resolve-name-3 @state block (:graph block))]
             (vswap! state assoc :subjects [[(format "<%s>" graph)]])
             (.println ; TODO: better logging
              *err*
              (format "WARN: Graphs are not supported by the NTriples serializer.\n%s line %d: %s"
                      (:file-name block)
                      (:line-number block)
                      (:block block)))
             result)
           
           :SUBJECT_BLOCK
           (let [subject (resolve-name-3 @state block (:subject block))]
             (vswap! state assoc :subjects [[(format "<%s>" subject)]])
             result)

           :LITERAL_BLOCK
           (let [{:keys [arrows predicate content]} block
                 subject   (if (string/blank? arrows)
                             (-> @state :subjects first first)
                             (do
                               (vswap! state update-in [:blank-node-count] inc)
                               (str "_:b" (:blank-node-count @state))))
                 predicate (format "<%s>" (resolve-name-3 @state block predicate))
                 object    (format-literal @state block)]

             ; When this is a HOWL-valid rdfs:label, add it to the map of labels.
             (when (and (string/blank? arrows)
                        (= predicate (format "<%s>" rdfs-label))
                        (valid-label? content))
               (vswap! state assoc-in [:labels content] (unwrap-iri subject)))

             ; Add this triple to the stack of subjects.
             (vswap! state
                     assoc
                     :subjects
                     (conj (vec (take (count arrows) (:subjects @state)))
                           [subject predicate object]))

             ; Two cases
             (if (string/blank? arrows)
               ; normal literal node
               (xf result (format "%s %s %s ." subject predicate object))
               ; annotation (maybe nested)
               (let [[annotatedSource annotatedProperty annotatedTarget]
                     (get-in @state [:subjects (dec (count arrows))])]
                 (apply
                  xf
                  result
                  [(format "%s <%s> <%s> ." subject rdf-type (str owl "Axiom"))
                   (format "%s <%s> %s ."   subject (str owl "annotatedSource") annotatedSource)
                   (format "%s <%s> %s ."   subject (str owl "annotatedProperty") annotatedProperty)
                   (format "%s <%s> %s ."   subject (str owl "annotatedTarget") annotatedTarget)
                   (format "%s %s %s ."     subject predicate object)]))))
           
           :LINK_BLOCK
           (let [{:keys [arrows predicate object]} block
                 subject   (if (string/blank? arrows)
                             (-> @state :subjects first first)
                             (do
                               (vswap! state update-in [:blank-node-count] inc)
                               (str "_:b" (:blank-node-count @state))))
                 predicate (format "<%s>" (resolve-name-3 @state block predicate))
                 object    (format "<%s>" (resolve-name-3 @state block object))]

             ; Add this triple to the stack of subjects.
             (vswap! state
                     assoc
                     :subjects
                     (conj (vec (take (count arrows) (:subjects @state)))
                           [subject predicate object]))

             ; Two cases
             (if (string/blank? arrows)
               ; normal literal node
               (xf result (format "%s %s %s ." subject predicate object))
               ; annotation (maybe nested)
               (let [[annotatedSource annotatedProperty annotatedTarget]
                     (get-in @state [:subjects (dec (count arrows))])]
                 (apply
                  xf
                  result
                  [(format "%s <%s> <%s> ." subject rdf-type (str owl "Axiom"))
                   (format "%s <%s> %s ."   subject (str owl "annotatedSource") annotatedSource)
                   (format "%s <%s> %s ."   subject (str owl "annotatedProperty") annotatedProperty)
                   (format "%s <%s> %s ."   subject (str owl "annotatedTarget") annotatedTarget)
                   (format "%s %s %s ."     subject predicate object)]))))

           ; else
           result)

         (catch Exception e
           (throw
            (Exception.
             (format "Error while serializing to Ntriples:\n%s line %d: %s\n%s"
                     (:file-name block)
                     (:line-number block)
                     (:block block)
                     (.getMessage e))))))))))

(defn print-triples
  [file-names]
  (->> (apply parse-files file-names)
       (transduce to-triples conj)
       (map println)
       doall))

(def cli-options
  ;; An option with a required argument
  [["-o" "--output FORMAT" "Output format"
    :default "ntriples"]
   ; TODO: replacement character, default "-"
   ; TODO: replacement regex, default "\\W"
   ; TODO: statement sorting
   ["-h" "--help"]])

(defn -main
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (case (:output options)
      "ntriples"   (print-triples arguments)
      "parses"     (print-parses arguments)
      ;"labels"     (print-labels arguments)
      
      (throw (Exception. "Unknown output format")))))
