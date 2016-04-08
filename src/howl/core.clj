(ns howl.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [instaparse.core :as insta])
  (:import [java.net URI])
  (:gen-class))

(def plain-literal "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral")
(def rdfs-label "http://www.w3.org/2000/01/rdf-schema#label")
(def xsd-string "http://www.w4.org/2001/XMLSchema#string")

;; Parsing works like this:
;;
;; 1. loop over files
;; 2. loop over lines in file
;; 3. merge indented lines into a single "unit"
;; 4. parse the unit and emit a vector
;;
;; Serializing works like this:
;;
;; 1. reduce over a sequence of parse vectors
;; 2. usually normalize it into a map
;; 3. process the map

(def unit-parser
  (insta/parser
   "BLOCK = PREFIX_BLOCK / LABEL_BLOCK / TYPE_BLOCK / SUBJECT_BLOCK /
            LINK_BLOCK / LITERAL_BLOCK
    PREFIX_BLOCK  = 'PREFIX' SPACES PREFIX ':' SPACES IRI EOL
    LABEL_BLOCK   = 'LABEL' SPACES PREFIXED_OR_IRI ':' SPACES LABEL EOL
    TYPE_BLOCK    = 'TYPE' SPACES PREDICATE ':' SPACES NAME EOL
    SUBJECT_BLOCK = 'SUBJECT' SPACES SUBJECT ':' SPACES LABEL EOL
    LITERAL_BLOCK = PREDICATE ':' SPACES LITERAL EOL
    LINK_BLOCK    = PREDICATE ':>' SPACES NAME EOL

    PREFIXED_OR_IRI = PREFIXED_NAME / IRI
    NAME            = PREFIXED_NAME / IRI / LABEL
    PREDICATE       = PREFIXED_NAME / IRI / LABEL
    SUBJECT         = PREFIXED_NAME / IRI / LABEL

    PREFIXED_NAME = PREFIX ':' LOCAL_NAME
    SPACES = ' '+
    PREFIX = #'\\w+'
    LOCAL_NAME = #'\\w+'
    WRAPPED_IRI = <'<'> IRI <'>'>
    IRI = #'[^>\\s]+'
    LANG = #'@\\w+'
    DATATYPE = '^^' (PREFIXED_NAME | IRI)
    LABEL = #'[^:]+'
    LITERAL = #'(\n|.)+'
    EOL = #'(\n|\\s*)'"))

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

(defn parse-unit
  "Given a [file-name line-number line] vector
   return the parse result as a vector
   or throws an Exception."
  [[file-name line-number unit]]
  (let [parse-result (unit-parser unit)]
    (if (insta/failure? parse-result)
      (throw (Exception. (message file-name line-number parse-result)))
      (concat [file-name line-number] (second parse-result)))))

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
      (map parse-unit))
     conj
     (line-seq reader))))

(defn parse-files
  "Given a sequence of file names,
   return a lazy sequence of parse results."
  [& file-names]
  (mapcat parse-file file-names))


(defn parse-vector-to-map
  [results]
  (->> results
       (remove keyword?)
       (remove string?)
       (remove #(contains? #{:SPACES :EOL} (first %)))
       (map (juxt first #(if (= 1 (count (rest %))) (second %) (rest %))))
       (into {})))

(defn parse-map
  "Given a parse result vector,
   return a map with just relevant information."
  [[file-name line-number unit-type & results :as parse-vector]]
  (try
    (merge
     (parse-vector-to-map results)
     {:type        unit-type
      :line-number line-number
      :file-name   file-name
      ;:parse-vector parse-vector
      })
    (catch Exception e
      (println parse-vector)
      (throw e))))


(defmulti format-block :type)

(defmethod format-block :PREFIX_BLOCK
  [{:keys [PREFIX IRI]}]
  (format "PREFIX %s: %s" PREFIX IRI))

(defmethod format-block :default
  [block]
  (str block))

(defn print-files
  [& file-names]
  (doseq [line (apply parse-files file-names)]
    (println (format-block (parse-map line)))))









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

(defn resolve-name-2
  [state block name]
  (let [LABEL (second name)
        {:keys [PREFIX LOCAL_NAME IRI] :as name-map}
        (parse-vector-to-map name)]
    (cond
      (and PREFIX LOCAL_NAME)
      (str (get-in state [:prefixes PREFIX]) LOCAL_NAME)
      IRI
      IRI
      LABEL
      (get-in state [:labels LABEL])
      :else
      (throw
       (Exception.
        (format "Could not resolve name '%s'" name))))))

(defn to-triples
  "Given a reducing function,
   return a stateful transducer
   that takes parse maps
   and returns NTriple statement strings."
  [xf]
  (let [state (volatile! {})] 
    (fn
      ([] (xf))
      ([result] (xf result))
      ([result block]
       (case (:type block)
         :PREFIX_BLOCK
         (do
           (vswap! state assoc-in [:prefixes (:PREFIX block)] (:IRI block))
           result)

         :LABEL_BLOCK
         (let [iri (resolve-name-2 @state block (:PREFIXED_OR_IRI block))]
           (vswap! state assoc-in [:labels (:LABEL block)] iri)
           result)

         :TYPE_BLOCK
         (let [predicate-iri (resolve-name-2 @state block (:PREDICATE block))
               datatype-iri  (resolve-name-2 @state block (:NAME block))]
           (vswap! state assoc-in [:types predicate-iri] datatype-iri)
           result)

         :SUBJECT_BLOCK
         (let [subject (resolve-name-2 @state block (:SUBJECT block))]
           (vswap! state assoc :subject subject)
           (xf result
               (format "<%s> <%s> \"%s\" ."
                       subject
                       rdfs-label
                       (:LABEL block))))

         :LITERAL_BLOCK
         (xf result
             (format "<%s> <%s> \"%s\" ."
                     (get @state :subject)
                     (resolve-name-2 @state block (:PREDICATE block))
                     (:LITERAL block)))

         :LINK_BLOCK
         (xf result
             (format "<%s> <%s> <%s> ."
                     (get @state :subject)
                     (resolve-name-2 @state block (:PREDICATE block))
                     (resolve-name-2 @state block (:NAME block))))

         ; else
         result)))))

(defn print-triples
  [file-name]
  (with-open [reader (io/reader file-name)]
    (->> (line-seq reader)
         (transduce
          (comp
           (merge-lines file-name)
           (map parse-unit)
           (map parse-map)
           to-triples)
          conj)
         (map println)
         doall)))

(defn -main
  [& args]
  (apply print-triples args))
