(ns howl.rdf-list
  "Parse and process RDF Lists syntax."
  (:require [clojure.string :as string]
            [clojure.walk :refer [postwalk]]
            [instaparse.core :as insta]
            [howl.util :as util :refer [<> owl> rdf>]]
            [howl.link :as link]
            [howl.core :as core]
            [howl.howl :as howl]
            [howl.nquads :as nquads]))

(def rdf-list-iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#List")
(def rdf-first "http://www.w3.org/1999/02/22-rdf-syntax-ns#first")
(def rdf-next "http://www.w3.org/1999/02/22-rdf-syntax-ns#next")
(def rdf-nil "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")

(def list-grammar
  (str "
<ITEMS> = ITEM+
ITEM = WHITESPACE '-' DATATYPES COLON #'.+'
COLON = #' *' ':'  #'(\n| )+'
WHITESPACE  = #'(\\r|\\n|\\s)*'"
       link/link-grammar))

(def list-parser (insta/parser list-grammar))

(defn parse-list
  [content]
  (let [result (list-parser content)]
    (when (insta/failure? result)
      (println result)
      (throw (Exception. "RDF List parser failure")))
    result))

(defmethod howl/parse-content ["LINK" rdf-list-iri]
  [env datatypes unparsed]
  (vec (concat [:RDF_LIST] (parse-list unparsed))))

(defmethod core/content-names->iris ["LINK" rdf-list-iri]
  [env datatypes content]
  (->> content
       (filter vector?)
       (filter #(= :ITEM (first %)))
       (map
        (fn [item]
          (let [dt (get-in item [3 2])
                dt (when dt (link/unpack-datatype env dt))
                ddt (get datatypes 2 "PLAIN")]
            {:object (get item 5)
             :datatype (or dt ddt)
             :use-default-datatypes (= [:DATATYPES] (get item 3))})))
       (concat [:RDF_LIST])
       vec))

(defmethod core/content-iris->names ["LINK" rdf-list-iri]
  [env datatypes objects]
  (->> objects
       (filter map?)
       (map
        (fn [{:keys [object datatype use-default-datatypes] :as item}]
          [:ITEM
           [:WHITESPACE "\n  "]
           "-"
           (if use-default-datatypes
             [:DATATYPES]
             (->> [datatype]
                  (map (partial link/iri->name env))
                  link/datatype-names->parse))
           [:COLON "" ":" " "]
           object]))
       (concat [:RDF_LIST])
       vec))

;; From NQuads

(defn list-item?
  "Given a subject map and a subject to check,
   return true only if it is a list item."
  [subject-map subject]
  (let [predicate-map (get subject-map subject)]
    (and
     (link/blank? subject)
     (clojure.set/subset? #{rdf-first rdf-next} (set (keys predicate-map))))))

(defn list-head?
  "Given a subject map and a subject to check,
   return true only if it is the head of a list
   i.e. it is a list item that is not rdf:next for anything."
  [subject-map subject]
  (and
   (list-item? subject-map subject)
   (->> (get-in subject-map [:blank-object-uses subject])
        (map second)
        (remove #(= rdf-next %))
        count
        (not= 0))))

(defn collapsible-list?
  "Given a subject map and a subject to check,
   return true only if we can safely collapse this RDF list
   into a single HOWL statement block."
  [subject-map subject]
  (let [predicate-map (get subject-map subject)]
    (and
     (list-item? subject-map subject)
     (= 2 (count predicate-map))
     (->> predicate-map vals (apply concat) count (= 2))
     (->> (get-in subject-map [:blank-object-uses subject])
          count
          (= 1))
     (->> (get-in subject-map [:blank-object-uses subject])
          (map first)
          (map (partial collapsible-list? subject-map))
          and))))

(defn chase-list
  "Given a subject map, a sequence of nodes in an RDF list,
   and a node to chance,
   return a sequence of subject-object-datatype triples."
  [subject-map head]
  (loop [subjects []
         subject head]
    (let [predicate-map (get subject-map subject)
          object (get-in predicate-map [rdf-first 0 :object])
          datatype (get-in predicate-map [rdf-first 0 :datatype])
          result {:subject subject :object object :datatype datatype}
          next (get-in predicate-map [rdf-next 0 :object])]
      (if (= next rdf-nil)
        (conj subjects result)
        (recur (conj subjects result) next)))))

(defn process-list
  "Given a subject map and a subject that is the head of a list,
   collapse the list and return the updated subject map."
  [env subject-map head]
  (let [[subject predicate] (get-in subject-map [:blank-object-uses head 0])
        label (get-in env [:iri-label predicate])
        datatypes (or (drop 2 (get-in env [:labels label :datatypes]))
                      ["PLAIN"])
        index (nquads/find-object subject-map subject predicate head "LINK")
        results (chase-list subject-map head)]
    (assoc-in
     (apply dissoc subject-map (map :subject results))
     [subject predicate index :processed-object]
     (->> results
          (map #(dissoc % :subject))
          (map
           (fn [{:keys [datatype] :as item}]
             (if (= [datatype] datatypes)
               (assoc item :use-default-datatypes true)
               item)))
          (concat [:RDF_LIST])))))

(defn process-lists
  "Given a graph IRI, and a subject map,
   process each list and return the updated subject-map."
  [env graph subject-map]
  (->> subject-map
       keys
       (filter (partial list-head? subject-map))
       (filter (partial collapsible-list? subject-map))
       (reduce (partial process-list env) subject-map)
       (conj [graph])))

(defn handle-lists
  [env graph-map]
  (->> graph-map
       (map (partial apply process-lists env))
       (into {})))

(nquads/register-handler handle-lists)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; to NQuads

(defn reduce-list
  "Build an RDF List, starting at the end."
  [graph items]
  (reduce
   (fn [quads {:keys [object datatype]}]
     (let [n (get-in quads [0 1] rdf-nil)
           b (link/random-blank-node)]
       (vec
        (concat
         [[graph b rdf-first object datatype]
          [graph b rdf-next n "LINK"]]
         quads))))
   []
   (->> items rest reverse)))

(defmethod nquads/object->nquads ["LINK" rdf-list-iri]
  [graph datatypes object]
  (let [result (reduce-list graph object)]
    [(second (first result)) result]))
