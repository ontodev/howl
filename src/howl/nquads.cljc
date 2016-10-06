(ns howl.nquads
  "Render parsed HOWL to N-Quads."
  (:require [clojure.string :as string]
            [clojure.set]
            [edn-ld.core :as edn-ld]
            [howl.util :as util]
            [howl.core :as core]))

(def default-graph "urn:x-arq:DefaultGraphNode")

(defn iriref-or-blank [str]
  (if (util/starts-with? str "_:")
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
