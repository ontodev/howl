(defproject howl "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [instaparse "1.4.1"]
                 [org.clojure/tools.cli "0.3.3"]
                 [org.clojure/data.json "0.2.6"]]
  :main howl.core
  :aot [howl.core])
