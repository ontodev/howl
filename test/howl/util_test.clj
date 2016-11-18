(ns howl.util-test
  "Test utility functions"
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test
             :refer :all :include-macros true]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties
             :as prop :include-macros true]

            [howl.util :as util]))

(defspec starts-with?-detects-prefixes
  (prop/for-all
   [s gen/string]
   (util/starts-with? (str s "foo") s)))

(deftest test-starts-with?
  (testing "everything starts with the empty string"
    (is (util/starts-with? "foobar" "")))
  (testing "does not hit a false positive when arguments are reversed"
    (is (util/starts-with? "foobar" "foo"))
    (is (not (util/starts-with? "foo" "foobar")))))

(defspec ends-with?-detects-suffixes
  (prop/for-all
   [s gen/string
    suffix gen/string]
   (util/ends-with? (str s suffix) suffix)))

(deftest test-ends-with?
  (testing "everything ends with the empty string"
    (is (util/ends-with? "foobar" "")))
  (testing "does not hit a false positive when arguments are reversed"
    (is (util/ends-with? "foobar" "bar"))
    (is (not (util/ends-with? "bar" "foobar")))))

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
    (is (not (util/absolute-uri-string? "baz")))
    (is (not (util/absolute-uri-string? "../blarg")))))

(defspec fresh-blank!-returns-a-blank-name
  (prop/for-all
   [init gen/int]
   (util/blank-name? (util/fresh-blank! (atom init)))))

(defspec fresh-blank!-doesnt-return-the-same-name-twice
  (prop/for-all
   [init gen/int]
   (let [atm (atom init)
         size 10
         blanks (take size (repeatedly #(util/fresh-blank! atm)))]
     (= (count blanks) (count (set blanks))))))

(deftest test-fresh-blank!
  (testing "fresh-blank! increments the given atomic counter"
    (is (let [init 0
              atm (atom init)]
          (util/fresh-blank! atm)
          (= @atm (inc init))))))

(defspec blank-name?-returns-true-for-strings-starting-with-underscore-colon
  (prop/for-all
   [base gen/string]
   (util/blank-name? (str "_:" base))))
