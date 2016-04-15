(ns howl.cli
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]]
            [howl.core :as core]
            [howl.ntriples :as nt])
  (:gen-class))

(defn parse-file
  "Given a file name,
   return a lazy sequence of parse results."
  [file-name]
  (with-open [reader (io/reader file-name)]
    (transduce
     (comp
      (core/merge-lines file-name)
      (map core/parse-block))
     conj
     (line-seq reader))))

(defn parse-files
  "Given a sequence of file names,
   return a lazy sequence of parse results."
  [& file-names]
  (mapcat parse-file file-names))

(defn print-parses
  "Given a sequence of file names,
   print a sequence of parse maps."
  [file-names]
  (->> (apply parse-files file-names)
       (map println)
       doall))

(defn print-triples
  [file-names]
  (->> (apply parse-files file-names)
       (transduce nt/to-triples conj)
       (map (partial apply format "%s %s %s ."))
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
