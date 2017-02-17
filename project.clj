(def project-version "0.3.0-SNAPSHOT")

(defproject howl project-version
  :description "Tools for processing HOWL format"
  :url "https://github.com/ontodev/howl"
  :license {:name "Simplified BSD License"
            :url "http://opensource.org/licenses/BSD-2-Clause"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.8.40"]
                 [com.lucasbradstreet/instaparse-cljs "1.4.1.1"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/test.check "0.9.0"]
                 [edn-ld "0.2.2"]
                 [doo "0.1.7-SNAPSHOT"]]
  :plugins [[lein-cljsbuild "1.1.3"]
            [lein-project-version "0.1.0"]
            [lein-cljfmt "0.5.6"]
            [lein-doo "0.1.7"]]
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
      :pretty-print true}}
    {:id "cljs-test"
     :source-paths ["src" "test"]
     :compiler
     {:output-to "out/testable.js"
      :optimizations :none
      :main howl.runner}}]})
