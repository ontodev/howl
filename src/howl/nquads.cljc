(ns howl.nquads
  "Render parsed HOWL to N-Quads."
  (:require [clojure.string :as string]
            [clojure.set]
            [edn-ld.core :as edn-ld]
            [howl.util :as util]
            [howl.core :as core]))

(def default-graph "urn:x-arq:DefaultGraphNode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Human-readable compression

(defn facts->urls [facts]
  (filter
   #(and (string? %)
         (not (blank-name? %))
         (not= % default-graph))
   (apply concat facts)))

(defn partition-url [url]
  (map
   #(apply str %)
   (partition-by
    (let [v (volatile! 0)
          prev (volatile! nil)]
      (fn [elem]
        (when (or (contains? #{\/ \#} elem)
                  (contains? #{\/ \#} @prev))
          (vswap! v inc))
        (vreset! prev elem)
        @v))
    url)))

(defn url->prefixes [url]
  (let [s (partition-url url)
        len (count s)]
    ((fn rec [at]
       (when (> len at)
         (lazy-seq
          (cons (string/join (take at s))
                (rec (inc (inc at)))))))
     5)))

(defn url->prefix-name [url]
  (let [elem (last (string/split url #"[/#]"))
        comps (string/split elem #"\.")]
    (case (count comps)
      1 elem
      2 (first comps)
      (second comps))))

(defn facts->prefixes
  [facts]
  ;; TODO - filter for accepted prefixes
  ;;      - ensure prefix names are unique
  ;;      - filter out the annotation URLs
  (reduce
   (fn [memo url]
     (assoc memo (url->prefix-name url) url))
   {}
   (reverse
    (sort-by
     count
     (filter
      #(not (re-find #"\d+[/#]$" %))
      (set (mapcat url->prefixes (facts->urls facts))))))))

(defn facts->labels
  ;; TODO - use prefixes to establish labels
  ;;      - do some kind of decision on whether a given label is worth it given the length of its prefixed form
  ;;      - filter out the annotation URLs (since they'll be removed anyhow)
  [facts]
  (reduce
   (fn [memo url]
     (assoc memo (url->prefix-name url) url))
   {}
   (map
    first
    (filter
     #(>= (second %) 3)
     (into (list) (dissoc (frequencies (facts->urls facts)) default-graph))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Pull out annotations
(def rdf-type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
(defn owl> [s] (str "http://www.w3.org/2002/07/owl#" s))
(defn rdf-schema> [s] (str "http://www.w3.org/2000/01/rdf-schema#" s))

(defn blank-name?
  "Given a string, returns true if it represents a blank name in RDF syntax."
  [str]
  (util/starts-with? str "_:"))

(defn annotation?
  "Given a subject and predicate map, returns true if the inputs
represent a Howl annotation block. Returns false otherwise."
  [subject predicate-map]
  (and (blank-name? subject)
       (every? #(contains? predicate-map %)
               [rdf-type
                (owl> "annotatedSource")
                (owl> "annotatedProperty")
                (owl> "annotatedTarget")
                (rdf-schema> "comment")])
       (contains? (get predicate-map rdf-type) (owl> "Axiom"))))

(defn separate-annotations
  "Given a subject-map, returns
[<subject-map-with-no-annotations> <subject-map-of-only-annotations>]"
  [subject-map]
  (reduce
   (fn [[no-annotations annotations] [subject predicate-map]]
     (if (annotation? subject predicate-map)
       [no-annotations (assoc annotations subject predicate-map)]
       [(assoc no-annotations subject predicate-map) annotations]))
   [{} {}]
   subject-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Output howl AST

(defn iriref-or-blank [str]
  (if (blank-name? str)
    [:BLANK_NODE_LABEL str]
    [:IRIREF "<" str ">"]))

(defn render-predicates
  [predicate-map]
  (mapcat
   (fn [[predicate objects]]
     (let [pred [:PREDICATE [:IRIREF "<" predicate ">"]]]
       (map (fn [[object _]]
              {:exp
               (if (string? object)
                 [:LINK_BLOCK
                  pred
                  [:COLON_ARROW "" ":>" " "]
                  [:OBJECT (iriref-or-blank object)]]
                 [:LITERAL_BLOCK
                  pred
                  ;; TODO - language and type annotations go here
                  [:COLON "" ":" " "]
                  [:LITERAL (object :value)]])})
            objects)))
   predicate-map))

(defn render-subjects
  [subject-map]
  (mapcat
   (fn [[subject predicates]]
     (concat
      [{:exp [:SUBJECT_BLOCK
              [:SUBJECT
               (iriref-or-blank subject)]]}]
      (render-predicates predicates)))
   subject-map))

(defn render-graphs
  [collapsed]
  (concat
   (render-subjects (get collapsed default-graph))
   (mapcat
    (fn [[graph subjects]]
      (concat
       [{:exp [:GRAPH_BLOCK "GRAPH" [:SPACES " "] [:GRAPH [:IRIREF "<" graph ">"]]]}]
       (render-subjects subjects)
       [{:exp [:GRAPH_BLOCK "GRAPH"]}]))
    (dissoc collapsed default-graph))))

(defn collapse
  "Given a sequence of Quads or Triples, return a level-wise map"
  [facts]
  (reduce (fn [coll fact] (assoc-in coll fact nil)) nil facts))

(defn quads-to-howl
  "Given a sequence of quads,
   return a sequence of HOWL block maps."
  [quads]
  (render-graphs (collapse quads)))

(defn triples-to-howl
  "Given a sequence of triples,
   return a sequence of HOWL block maps."
  [trips]
  (render-subjects (collapse trips)))


;; collapse
;; render-graphs (or subjects, depending on triples/quads)
;;;; render the default graph first
;;;; render each other graph inside of a graph block
;;;=> render-graph
;;;;; for-each subject
;;;;;;; if there is one predicate
;;;;;;;;; render the predicate
;;;;;;; else
;;;;;;;;; render an annotation
;;;=> render-predicate
;;;;; for-each object
;;;;;;; if url?
;;;;;;;;; render-link-block
;;;;;;; else
;;;;;;;;; render-literal-block
