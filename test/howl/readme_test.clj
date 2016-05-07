(ns howl.readme-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [howl.core :refer :all]))

;; Parse the README.md file
;; looking for pairs of example blocks and their JSON results,
;; then parse the example block and compare it to the expected result.

(defn run-test
  [[block result]]
  (try
    (testing block
      (is (= (clojure.walk/postwalk
              #(if (keyword? %) (name %) %)
              (parse-block "example.howl" 1 block))
             (json/read-str result))))
    (catch Exception e
      (throw (Exception. (str "Failed while parsing block: " (.getMessage e)))))))

#_(deftest test-readme-examples
    (->> "README.md"
         slurp
         string/split-lines
         (drop-while #(not (.startsWith % "## Syntax and Parsing")))
         (partition-by #(.startsWith % "    "))
         (filter #(.startsWith (first %) "    ")) ; keep indented
         (map (fn [lines] (map #(string/replace % #"^    " "") lines)))
         (map (fn [lines] (str (string/join "\n" lines) "\n")))
         (partition 2)
         (map run-test)
         doall
         (#(do (println "Running" (count %) "tests on README") %))
         (apply = true)))
