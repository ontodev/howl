(ns howl.nquads
  "Render parsed HOWL to N-Quads."
  (:require [clojure.string :as string]
            [clojure.set]
            [howl.util :as util]
            [howl.core :as core]))

(def default-graph "urn:x-arq:DefaultGraphNode")

(defn blank-name?
  "Given a string, returns true if it represents a blank name in RDF syntax."
  [str]
  (util/starts-with? str "_:"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Human-readable compression

(defn statements->urls [statements]
  (filter
   #(and (string? %)
         (not (blank-name? %))
         (not= % default-graph))
   (apply concat statements)))

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

(defn unique-assoc
  "Interns names in String -> a maps while avoiding name collisions.
Used only in the two statements->* functions following."
  [map name value]
  (cond
    (not (contains? map name)) (assoc map name value)
    (and (contains? map name) (= (get map name) value))
    map
    :else (loop [i 2
                 i-name (str name "-" i)]
            (cond
              (not (contains? map i-name)) (assoc map i-name value)
              (and (contains? map i-name) (= (get map i-name) value)) map
              :else (recur (inc i) (str name "-" (inc i)))))))

(defn statements->prefixes
  "Given a series of statements, returns a prefix map appropriate for compressing them."
  [statements]
  (reduce
   (fn [memo url]
     (unique-assoc memo (url->prefix-name url) url))
   {}
   (reverse
    (sort-by
     count
     (filter
      #(not (re-find #"\d+[/#]$" %))
      (set (mapcat url->prefixes (statements->urls statements))))))))

(defn statements->labels
  "Given a series of statements, and optionally a map of labels,
   returns a labels map appropriate for compressing the given statements."
  [statements]
  (reduce
   (fn [memo url]
     (assoc memo (url->prefix-name url) url))
   {}
   (map
    first
    (filter
     #(>= (second %) 3)
     (into (list) (dissoc (frequencies (statements->urls statements)) default-graph))))))

(defn statements->env
  [statements]
  {:prefixes (statements->prefixes statements)
   :labels (statements->labels statements)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Pull out annotations
(def rdf-type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
(defn owl> [s] (str "http://www.w3.org/2002/07/owl#" s))
(defn rdf-schema> [s] (str "http://www.w3.org/2000/01/rdf-schema#" s))

(def annotation-predicates
  [rdf-type
   (owl> "annotatedSource")
   (owl> "annotatedProperty")
   (owl> "annotatedTarget")])

(defn annotation?
  "Given a subject and predicate map, returns true if the inputs
represent a Howl annotation block. Returns false otherwise."
  [subject predicate-map]
  (and (blank-name? subject)
       (every? #(contains? predicate-map %) annotation-predicates)
       (contains? (get predicate-map rdf-type) (owl> "Axiom"))))

(defn separate-annotations
  "Given a subject-map, returns
[<subject-map-with-no-annotations> <subject-map-of-only-annotations>]
The <subject-map-of-only-annotations> includes [s p o] indices to annotation
subjects for later ease of indexing."
  [subject-map]
  (let [get-ann (fn [pred-map prop] (first (keys (get pred-map (owl> prop)))))]
    (reduce
     (fn [[no-annotations annotations] [subject predicate-map]]
       (if (annotation? subject predicate-map)
         [no-annotations
          (assoc annotations subject predicate-map)
          (merge
           annotations)]
         [(assoc no-annotations subject predicate-map) annotations]))
     [{} {}]
     subject-map)))

(defn annotations-for [[s p o] annotations-map]
  (filter
   (fn [[k predicates-map]]
     (every?
      identity
      (map #(= %1 (first (keys (get predicates-map (owl> %2)))))
           [s p o] ["annotatedSource" "annotatedProperty" "annotatedTarget"])))
   annotations-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Output howl AST
(defn invert-env [env]
  (merge
   env
   {:labels   (clojure.set/map-invert (get env :labels))
    :prefixes (clojure.set/map-invert (get env :prefixes))}))

(defn longest-prefix [target candidates]
  (first
   (filter
    #(util/starts-with? target %)
    (reverse (sort-by count candidates)))))

(defn leaf-node [env thing]
  (let [inv (invert-env env)]
    (cond
      (blank-name? thing)             [:BLANK_NODE_LABEL thing]
      (contains? (inv :labels) thing) [:LABEL (get-in inv [:labels thing])]
      :else (if-let [pref (longest-prefix thing (keys (inv :prefixes)))]
              [:PREFIXED_NAME [:PREFIX (get-in inv [:prefixes pref])] ":" (subs thing (count pref))]
              [:IRIREF "<" thing ">"]))))

(declare render-predicates)

(defn render-annotation-tree [env [annotation-subject predicate-map] annotations-map arrows]
  (let [pred-map (reduce
                  (fn [memo k] (dissoc memo k))
                  (get annotations-map annotation-subject)
                  annotation-predicates)
        blocks (render-predicates env pred-map annotation-subject annotations-map (str ">" arrows))]
    (cons [:ANNOTATION [:ARROWS arrows] (first blocks)] (rest blocks))))

(defn render-literal [literal]
  (let [lns (string/split-lines literal)]
    (string/join \newline (cons (first lns) (map #(str "  " %) (rest lns))))))

(defn render-predicates
  [env predicate-map subject annotations-map arrows]
  (mapcat
   (fn [[predicate objects]]
     (let [pred [:PREDICATE (leaf-node env predicate)]]
       (mapcat
        (fn [[object _]]
          (cons
           (if (string? object)
             [:LINK_BLOCK
              pred
              [:COLON_ARROW "" ":>" " "]
              [:OBJECT (leaf-node env object)]]
             [:LITERAL_BLOCK
              pred
              ;; TODO - language and type annotations go here
              [:COLON "" ":" " "]
              [:LITERAL (render-literal (object :value))]])
           (mapcat
            #(render-annotation-tree env % annotations-map arrows)
            (annotations-for [subject predicate object] annotations-map))))
        objects)))
   predicate-map))

(defn render-subjects
  ([subject-map] (render-subjects subject-map {}))
  ([subject-map env]
   (let [[blocks annotations] (separate-annotations subject-map)]
     (mapcat
      (fn [[subject predicates]]
        (concat
         [{:exp [:SUBJECT_BLOCK [:SUBJECT (leaf-node env subject)]]}]
         (map (fn [block] {:exp block}) (render-predicates env predicates subject annotations "> "))))
      blocks))))

(defn render-graphs
  ([collapsed] (render-graphs collapsed {}))
  ([collapsed env]
   (concat
    (render-subjects (get collapsed default-graph) env)
    (mapcat
     (fn [[graph subjects]]
       (concat
        [{:exp [:GRAPH_BLOCK "GRAPH" [:SPACES " "] [:GRAPH (leaf-node env graph)]]}]
        (render-subjects subjects env)
        [{:exp [:GRAPH_BLOCK "GRAPH"]}]))
     (dissoc collapsed default-graph)))))

(defn collapse
  "Given a sequence of Quads or Triples, return a level-wise map"
  [statements]
  (reduce (fn [coll statement] (assoc-in coll statement nil)) nil statements))

(defn quads-to-howl
  "Given a sequence of quads,
   return a sequence of HOWL block maps."
  ([quads] (quads-to-howl quads (statements->env quads)))
  ([quads env]
   (render-graphs (collapse quads) env)))

(defn triples-to-howl
  "Given a sequence of triples,
   return a sequence of HOWL block maps."
  ([trips] (triples-to-howl trips (statements->env trips)))
  ([trips env] (render-subjects (collapse trips) env)))

(defn rendered->string
  [rendered-blocks]
  (map
   #(if (= :SUBJECT_BLOCK (first (% :exp)))
      (str \newline (core/block->string %))
      (core/block->string %))
   rendered-blocks))


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
