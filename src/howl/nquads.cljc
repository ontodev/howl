(ns howl.nquads
  "Recover the Howl representation from a collection of NQuads"
  (:require [clojure.string :as string]
            [clojure.set]
            [howl.util :as util :refer [owl> rdf> rdf-schema>]]
            [howl.core :as core]))

(def default-graph "urn:x-arq:DefaultGraphNode")

(defn default-graph-of [graph-map]
  (or (get graph-map default-graph)
      (get graph-map nil)))

(defn without-default-graph [graph-map]
  (dissoc (dissoc graph-map default-graph) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Human-readable compression
(defn statements->urls
  [statements]
  (filter
   #(and (string? %)
         (not (util/blank-name? %))
         (not= % default-graph))
   (apply concat statements)))

(defn partition-url
  "Split the given URL at /s and #s.
   Because of the way we're using this function, we want
   to keep the delimiters as elements in the result (which is
   why we can't just use string/split)."
  [url]
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

(defn url->prefixes
  "Takes a url and returns all of its compound prefixes."
  [url]
  (let [s (partition-url url)
        len (count s)]
    ((fn rec [at]
       (when (> len at)
         (lazy-seq
          (cons (string/join (take at s))
                (rec (inc (inc at)))))))
     5)))

(defn url->prefix-name
  "Takes a URL and returns either the last path element or the
   #-component if present."
  [url]
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
     (into (list) (without-default-graph (frequencies (statements->urls statements))))))))

(defn statements->env
  [statements]
  {:prefixes (statements->prefixes statements)
   :labels (statements->labels statements)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Pull out annotations
(def annotation-predicates
  [(rdf> "type")
   (owl> "annotatedSource")
   (owl> "annotatedProperty")
   (owl> "annotatedTarget")])

(defn annotation?
  "Given a subject and predicate map, returns true if the inputs
represent a Howl annotation block. Returns false otherwise."
  [subject predicate-map]
  (and (util/blank-name? subject)
       (contains? (get predicate-map (rdf> "type")) (owl> "Axiom"))
       (every? #(contains? predicate-map %) annotation-predicates)))

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

(defn leaf-node
  ([env thing] (leaf-node env thing {}))
  ([env thing established]
   (let [inv (invert-env env)]
     (cond
       (util/blank-name? thing) [:BLANK_NODE_LABEL thing]
       (and (contains? established thing)
            (contains? (inv :labels) thing))
         [:LABEL (get-in inv [:labels thing])]
       :else (if-let [pref (longest-prefix thing (keys (inv :prefixes)))]
               [:PREFIXED_NAME [:PREFIX (get-in inv [:prefixes pref])] ":" (subs thing (count pref))]
               [:IRIREF "<" thing ">"])))))

(defn render-literal [literal]
  (let [lns (string/split-lines literal)]
    (string/join \newline (cons (first lns) (map #(str "  " %) (rest lns))))))

(declare render-predicates)

(defn render-annotation-tree [env [annotation-subject predicate-map] annotations-map arrows]
  (let [pred-map (reduce
                  (fn [memo k] (dissoc memo k))
                  (get annotations-map annotation-subject)
                  annotation-predicates)
        blocks (render-predicates env pred-map annotation-subject annotations-map (str ">" arrows))]
    (cons [:ANNOTATION [:ARROWS arrows] (first blocks)] (rest blocks))))

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
             (vec
              (filter
               #(not (nil? %))
               [:LITERAL_BLOCK
                pred
                (when (and (contains? object :lang)
                           (not= (object :lang) (get-in env [:defaults predicate "LANGUAGE"])))
                  [:LANGUAGE [:SPACES " "] (object :lang)])
                (when (and (contains? object :type)
                           (not= (object :type) (get-in env [:defaults predicate "TYPE"])))
                  [:TYPE [:SPACES " "] [:DATATYPE [:IRIREF (object :type)]]])
                [:COLON "" ":" " "]
                [:LITERAL (render-literal (object :value))]])))
           (mapcat
            #(render-annotation-tree env % annotations-map arrows)
            (annotations-for [subject predicate object] annotations-map))))
        objects)))
   predicate-map))

(defn render-subjects
  ([subject-map] (render-subjects subject-map {}))
  ([subject-map env]
   (let [[blocks annotations] (separate-annotations subject-map)
         established (volatile! #{})]
     (mapcat
      (fn [[subject predicates]]
        (concat
         (let [est @established
               subject-block [{:exp [:SUBJECT_BLOCK [:SUBJECT (leaf-node env subject est)]] :env env}]]
           (vswap! established #(clojure.set/union #{subject} %))
           subject-block)
         (map (fn [block] {:exp block :env env})
              (render-predicates env predicates subject annotations "> "))))
      blocks))))

(defn render-graphs
  ([collapsed] (render-graphs collapsed {}))
  ([collapsed env]
   (concat
    (render-subjects (default-graph-of collapsed) env)
    (mapcat
     (fn [[graph subjects]]
       (map
        #(if (empty? (% :env)) (assoc % :env env) %)
        (concat
         [{:exp [:GRAPH_BLOCK "GRAPH" [:SPACES " "] [:GRAPH (leaf-node env graph)]]}]
         (render-subjects subjects env)
         [{:exp [:GRAPH_BLOCK "GRAPH"]}])))
     (without-default-graph collapsed)))))

(defn collapse
  "Given a sequence of Quads or Triples, return a level-wise map"
  [statements]
  (reduce (fn [coll statement] (assoc-in coll statement nil)) nil statements))

(defn quads-to-howl
  "Given a sequence of quads,
   return a sequence of HOWL block maps."
  ([quads]
   (quads-to-howl quads (statements->env quads)))
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
