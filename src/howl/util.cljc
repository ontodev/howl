(ns howl.util
  "Cross-platform utility functions."
  (:require
   [clojure.string :as string]
   #?@(:cljs
       [[goog.string]
        [goog.string.format]]))
  #?(:clj
     (:refer-clojure :exclude [format])))

(def format
  #?(:clj #'clojure.core/format
     :cljs #'goog.string/format))

(defn throw-exception
  "Given a sequence of arguments,
   throw a cross-platform exception."
  [& messages]
  (throw
   (#?(:clj Exception. :cljs js/Error.)
    (->> messages (map str) (string/join " ")))))

(defn substring?
  "True if needle is a substring of haystack."
  [haystack needle]
  #?(:clj
     (.contains haystack needle)
     :cljs
     (> (.indexOf haystack needle) -1)))

(defn starts-with? [target prefix]
  (and (>= (count target) (count prefix))
       (every? identity (map = target prefix))))

(defn ends-with? [target prefix]
  (and (>= (count target) (count prefix))
       (every? identity (map = (reverse target) (reverse prefix)))))

(defn absolute-uri-string? [s]
  "Cribbed from http://stackoverflow.com/a/19709846/190887. Takes a string
   and returns true if it represents an absolute uri (false otherwise)"
  (not (not (re-find #"(?i)^(?:[a-z]+:)?//" s))))

(defn fresh-blank! [atom]
  (swap! atom inc)
  (str "_:b" @atom))

(defn blank-name?
  "Given a string, returns true if it represents a blank name in RDF syntax."
  [str]
  (starts-with? str "_:"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; General formatting shortcuts
(defn <> [s] (str "<" s ">"))

(def owl "http://www.w3.org/2002/07/owl#")
(defn owl? [iri] (starts-with? owl iri))
(defn owl> [name] (str owl name))

(defn rdf> [name] (str "http://www.w3.org/1999/02/22-rdf-syntax-ns#" name))
(defn rdf-schema> [name] (str "http://www.w3.org/2000/01/rdf-schema#" name))
