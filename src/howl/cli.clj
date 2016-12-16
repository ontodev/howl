(ns howl.cli
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]]
            [edn-ld.jena]
            [howl.util :as util]
            [howl.core :as core]
            [howl.howl :as howl]
            [howl.json :as json]
            [howl.nquads :as nq]
            [howl.api :as api])
  (:gen-class))

(defn file-lines
  [filename]
  (line-seq (clojure.java.io/reader filename)))

(defn exit
  [status msg]
  (println msg)
  (System/exit status))

(defn parse-howl-file
  "Takes a filename, and optionally a starting environment, and parses that
  file under the given environment."
  ([filename] (parse-howl-file {:source filename} filename))
  ([env filename]
   (howl/lines->blocks
    (assoc env :source filename)
    (file-lines filename))))

(defn parse-rdf-file
  "Takes a filename, and optionally a starting environment, and parses that
   file as an NQuad sequence, returning an environment with :blocks being
   a sequence of HOWL block maps"
  ([filename] (parse-rdf-file {:source filename} filename))
  ([env filename]
   (let [e (assoc env :source filename)
         blocks (nq/lines->blocks e (file-lines filename))]
     (assoc e :blocks (vec blocks)))))

(defn parse-file
  "Given a file name,
   return an environment with the :blocks key set."
  ([filename] (parse-file {:source filename} filename))
  ([env filename]
   (cond (util/ends-with? filename "howl") (parse-howl-file env filename)
         :else (parse-rdf-file env filename))))

(defn parse-files
  "Given a sequence of filenames, and optionally a starting environment,
   return a lazy sequence of parse results."
  ([filenames] (parse-files {} filenames))
  ([starting-env filenames]
   ((fn rec [env filenames]
      (when (not (empty? filenames))
        (let [f (parse-file env (first filenames))]
          (cons f (lazy-seq (rec (dissoc f :blocks) (rest filenames)))))))
    starting-env filenames)))

(defn print-parses
 "Given a sequence of file names, print HOWL."
  [options file-names]
  (->> (parse-files {:options options} file-names)
       (map :blocks)
       (apply concat)
       (map json/block->json-string)
       (map println)
       doall))

(defn print-howl
 "Given a sequence of file names, print HOWL."
  [options file-names]
  (->> (parse-files {:options options} file-names)
       (map :blocks)
       (apply concat)
       (map howl/block->howl-string)
       (map println)
       doall))

(defn print-quads
 "Given a map of options and a sequence of file names
  print a sequence of N-Quads."
 [options file-names]
  (->> (parse-files {:options options} file-names)
       (map api/howl-to-nquads)
       (map println)
       doall))

;(defn print-triples
;  "Given a map of options and a sequence of file names,
;   print a sequence of N-Triples"
;  [options file-names]
;  (->> (apply parse-files file-names)
;       core/blocks->nquads
;       (map (fn [[a b c d]] [a b c]))
;       core/print-nquads!))

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

(def cli-options
  [["-o" "--output FORMAT" "Output format: N-Triples(default), N-Quads, parses (JSON)"]
   ["-V" "--version"]
   ["-h" "--help"]])

(defn usage [options-summary]
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

(defn version []
  (-> (eval 'howl.cli)
      .getPackage
      .getImplementationVersion
      (or "DEVELOPMENT")))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    ; Handle help and error conditions
    (cond
      (:help options) (exit 0 (usage summary))
      (:version options) (exit 0 (version))
      errors (exit 1 (error-msg errors))
      :else (case (-> options (get :output "nquads") string/lower-case format-map)
              "parses"   (print-parses arguments)
              "howl"     (print-howl options arguments)
              ; "ntriples" (print-triples options arguments)
              "nquads"   (print-quads options arguments)
              (exit 1 (usage summary))))))
