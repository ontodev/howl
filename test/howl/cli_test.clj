(ns howl.cli-test
  "Test CLI functions"
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as string]
            [howl.cli :as cli]))

(deftest test-parse-args
  (testing "parse args"
    (is (= (cli/parse-args ["--help"])
           {:arguments ["--help"]
            :options {:help true}
            :errors []
            :inputs []
            :outputs []}))
    (is (= (cli/parse-args ["foo.howl"])
           {:arguments ["foo.howl"]
            :options {}
            :errors []
            :inputs [{:path "foo.howl" :format :howl}]
            :outputs []}))
    (is (= (cli/parse-args ["-c" "foo.howl"])
           {:arguments ["-c" "foo.howl"]
            :options {}
            :errors []
            :inputs [{:path "foo.howl" :format :howl :context true}]
            :outputs []}))
    (is (= (cli/parse-args ["-o" "foo.howl"])
           {:arguments ["-o" "foo.howl"]
            :options {}
            :errors []
            :inputs []
            :outputs [{:path "foo.howl" :format :howl}]}))
    (is (= (cli/parse-args ["-o" "foo.howl" "-h"])
           {:arguments ["-o" "foo.howl" "-h"]
            :options {:help true}
            :errors []
            :inputs []
            :outputs [{:path "foo.howl" :format :howl}]}))
    (is (= (cli/parse-args ["-c" "context.howl" "content.howl" "-o" "data.howl"])
           {:arguments ["-c" "context.howl" "content.howl" "-o" "data.howl"]
            :options {}
            :errors []
            :inputs
            [{:path "context.howl" :format :howl :context true}
             {:path "content.howl" :format :howl}]
            :outputs [{:path "data.howl" :format :howl}]}))
    (is (= (cli/parse-args ["-c" "context.howl" "-c" "context2.howl" "content.howl" "-o" "data.howl"])
           {:arguments ["-c" "context.howl" "-c""context2.howl" "content.howl" "-o" "data.howl"]
            :options {}
            :errors []
            :inputs
            [{:path "context.howl" :format :howl :context true}
             {:path "context2.howl" :format :howl :context true}
             {:path "content.howl" :format :howl}]
            :outputs [{:path "data.howl" :format :howl}]}))))
