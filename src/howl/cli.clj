(ns howl.cli
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.tools.cli :refer [parse-opts]]
            [edn-ld.jena]
            [howl.core :as core]
            [howl.nquads :as nq])
  (:gen-class))

(defn exit
  [status msg]
  (println msg)
  (System/exit status))

(defn parse-howl-file
  "Given the name of a HOWL file,
   return a lazy sequence of HOWL block maps."
  [file-name]
  (with-open [reader (io/reader file-name)]
    (transduce
     (comp
      (core/merge-lines file-name)
      (map core/parse-block))
     conj
     (line-seq reader))))

(defn parse-rdf-file
  "Given the name of a file that Apache Jena can read,
   and return a sequence of HOWL block maps."
  [file-name]
  (let [[prefixes quads] (edn-ld.jena/read-quads file-name)]
    (if (seq quads)
      (nq/quads-to-howl {} quads)
      (let [[prefixes triples] (edn-ld.jena/read-triples file-name)]
        (if (seq triples)
          (nq/triples-to-howl {} triples)
          (exit 1 (str "Could not find quads or triples in file: " file-name)))))))

(defn parse-file
  "Given a file name,
   return a lazy sequence of HOWL block maps."
  [file-name]
  (cond
   (.endsWith file-name "howl")
   (parse-howl-file file-name)

   ; TODO: more formats

   :else
   (parse-rdf-file file-name)))

(defn parse-files
  "Given a sequence of file names,
   return a lazy sequence of parse results."
  [& file-names]
  (mapcat parse-file file-names))


(defn print-howl
  "Given a sequence of file names, print HOWL."
  [file-names]
  (->> (apply parse-files file-names)
       (map core/render-block)
       (map println)
       doall))

(defn print-parses
  "Given a sequence of file names,
   print a sequence of JSON parse maps."
  [file-names]
  (->> (apply parse-files file-names)
       (map json/write-str)
       (map println)
       doall))

(defn print-quads
  "Given a sequence of file names
   print a sequence of N-Quads."
  [file-names]
  (->> (apply parse-files file-names)
       (transduce (nq/render-quads) conj)
       (map nq/quad-to-string)
       (map println)
       doall))

(defn print-triples
  "Given a sequence of file names
   print a sequence of N-Triples from the default graph."
  [file-names]
  (->> (apply parse-files file-names)
       (transduce (nq/render-quads) conj)
       (filter #(nil? (first %)))
       (map rest)
       (map (partial apply format "%s %s %s ."))
       (map println)
       doall))

(def format-synonyms
  {"ntriples" ["nt" "n-triples" "triples" "n-triple" "ntriple" "triple"]
   "nquads"   ["nq" "n-quads"   "quads"   "n-quad"   "nquad"   "quad"]
   "parses"   ["parse"]
   "howl"     ["howl"]})

(def format-map
  (->> format-synonyms
       (mapcat
        (fn [[format synonyms]]
          (for [synonym (conj synonyms format)]
            [synonym format])))
       (into {})))

; TODO: replacement character, default "-"
; TODO: replacement regex, default "\\W"
; TODO: statement sorting

(def cli-options
  [["-o" "--output FORMAT" "Output format: N-Triples (default), N-Quads, parses (JSON)"]
   ["-V" "--version"]
   ["-h" "--help"]])

(defn usage
  [options-summary]
  (->> ["Process HOWL files, writing to STDOUT."
        ""
        "Usage: howl [OPTIONS] INPUT-FILE+"
        ""
        "Options:"
        options-summary
        ""
        "WARN: This is an early development version."
        "Please see https://github.com/ontodev/howl for more information."]
       (string/join \newline)))

(defn version
  []
  (-> (eval 'howl.cli)
      .getPackage
      .getImplementationVersion
      (or "DEVELOPMENT")))

(defn error-msg
  [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn -main
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    ;; Handle help and error conditions
    (cond
      (:help options) (exit 0 (usage summary))
      (:version options) (exit 0 (version))
      (not= (count arguments) 1) (exit 1 (usage summary))
      errors (exit 1 (error-msg errors)))
    ;; Execute program with options
    (case (-> options (get :output "ntriples") string/lower-case format-map)
      "howl"     (print-howl arguments)
      "parses"   (print-parses arguments)
      "nquads"   (print-quads arguments)
      "ntriples" (print-triples arguments)
      (exit 1 (usage summary)))))
