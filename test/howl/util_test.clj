(ns howl.util-test
  "Test utility functions"
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test
             :refer :all :include-macros true]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties
             :as prop :include-macros true]

            [howl.util :as util]))

(deftest test-absolute-uri-string?
  (testing "returns true for http/https/ftp-prefixed urls"
    (is (util/absolute-uri-string? "http://example.com"))
    (is (util/absolute-uri-string? "https://example.com"))
    (is (util/absolute-uri-string? "ftp://example.com")))
  (testing "works on made up protocols"
    (is (util/absolute-uri-string? "mumble://example.com"))
    (is (util/absolute-uri-string? "flarp://example.com"))
    (is (util/absolute-uri-string? "madeUpProtocol://example.com")))
  (testing "returns true on protocol-relative URLs"
    (is (util/absolute-uri-string? "//example.com")))
  (testing "doesn't care about case"
    (is (util/absolute-uri-string? "HTTP://EXAMPLE.com"))
    (is (util/absolute-uri-string? "HTTPS://EXAMPLE.com"))
    (is (util/absolute-uri-string? "FTP://EXAMPLE.com")))
  (testing "returns false for relative paths"
    (is (not (util/absolute-uri-string? "/foo/bar/baz")))
    (is (not (util/absolute-uri-string? "/foo/bar")))
    (is (not (util/absolute-uri-string? "baz")))))
