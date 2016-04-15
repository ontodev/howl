(defproject howl "0.1.0"
  :description "Tools for processing HOWL format"
  :url "https://github.com/ontodev/howl"
  :license {:name "Simplified BSD License"
            :url "http://opensource.org/licenses/BSD-2-Clause"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [instaparse "1.4.1"]
                 [org.clojure/tools.cli "0.3.3"]
                 [org.clojure/data.json "0.2.6"]]
  :main howl.cli
  :aot [howl.cli])
