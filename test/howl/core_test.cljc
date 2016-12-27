(ns howl.core-test
  "Test core functions."
  (:require [clojure.test :refer [deftest testing is are]]
            [howl.core :refer [merge-environments]]))

(deftest test-merge-environments
  (testing "merges :prefixes and :labels with merge"
    (is (= (merge-environments
            {:labels
             {"foo" {:iri "http://foo"}}
             :prefix-iri
             {"baz" "foobaz"}}
            {:labels
             {"bar" {:iri "http://bar"}}
             :prefix-iri
             {"baz" "foobaz"}})
           {:labels
            {"bar" {:iri "http://bar"}
             "foo" {:iri "http://foo"}}
            :prefix-iri
            {"baz" "foobaz"}})))
  ;; (testing "merges :subject and :base by taking the more recent value"
  ;;   (is (= (merge-environments
  ;;           {:subject "foo" :base "bar"}
  ;;           {:subject nil :base "baz"})
  ;;          {:subject nil :base "baz"})))
  ;; (testing "merges :graph only if the new environment has a graph entry"
  ;;   (is (= (merge-environments {:graph "foo"} {})
  ;;          {:graph "foo"}))
  ;;   (is (= (merge-environments {:graph "foo"} {:graph "bar"})
  ;;          {:graph "bar"})))
  )
