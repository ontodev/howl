(def project-version "0.2.0-SNAPSHOT")

(defproject howl project-version
  :description "Tools for processing HOWL format"
  :url "https://github.com/ontodev/howl"
  :license {:name "Simplified BSD License"
            :url "http://opensource.org/licenses/BSD-2-Clause"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.8.40"]
                 [com.lucasbradstreet/instaparse-cljs "1.4.1.1"]
                 [org.clojure/tools.cli "0.3.3"]
                 [org.clojure/data.json "0.2.6"]]
  :plugins [[lein-cljsbuild "1.1.3"]
            [lein-project-version "0.1.0"]]
  :main howl.cli
  :aot [howl.cli]
  :manifest {"Implementation-Version" ~project-version}
  :cljsbuild
  {:builds
   [{:source-paths ["src"]
     :compiler
     {:optimizations :advanced
      :output-dir "target"
      :output-to "target/howl.js"
      :source-map "target/howl.js.map"
      :pretty-print true}}]})
