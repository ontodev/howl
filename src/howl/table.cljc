(ns howl.table
  (:require [clojure.string :as string]
            [clojure.data.csv :as csv]
            [howl.util :as util]
            [howl.link :as link]
            [howl.core :as core]
            [howl.howl :as howl]))

(defn tsv-to-howl
  "Given a SUBJECT/label/type TSV file reader or string, returns the corresponding
lazy sequence of Howl lines"
  [source]
  (mapcat
   (fn [[subject label type]]
     ;; FIXME - sanitize the label properly. (Not doing it now because the _real_ solution
     ;;         is probably to start up table.cljc, and emit Howl blocks from tabular data
     ;;         rather than the below table->string hack)
     (let [l (first (string/split (string/replace label #"[\[\]]" "") #"@"))]
       [[:LABEL_BLOCK
         "LABEL" [:SPACES " "]
         [:LABEL label]
         [:DATATYPES]
         [:COLON "" ":" " "]
         (link/parse-link subject)]
        [:SUBJECT_BLOCK [:LABEL label]]
        [:STATEMENT_BLOCK [:ARROWS "" ""] [:LABEL "label"] [:DATATYPES] [:COLON "" ":" " "] label]
        [:STATEMENT_BLOCK [:ARROWS "" ""] [:LABEL "type"] [:DATATYPES] [:COLON "" ":" " "]
         (link/parse-link type)]]))
   (rest (csv/read-csv source :separator \tab))))

(defn block-from-parse [parse-tree]
  (assoc (howl/parse->block parse-tree) :block-type (first parse-tree)))

(defn tsv-to-environment
  "Given a SUBJECT/label/type TSV file reader or string, and optionally a starting environment,
    returns the corresponding Howl environment"
  ([source] (tsv-to-environment {} source))
  ([env source]
   (reduce
    (fn [env block]
      (core/update-environment
       env (if (contains? block :target-name)
             (assoc block :target-iri (link/->iri env (:target-name block)))
             block)))
    env
    (map
     (fn [parse-tree]
       (assoc (howl/parse->block parse-tree)
              :block-type (first parse-tree)))
     (tsv-to-howl source)))))
