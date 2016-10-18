(ns howl.util
  "Cross-platform utility functions."
  (:require
   [clojure.string :as string]
   #?@(:cljs
       [[goog.string]
        [goog.string.format]]))
  #?(:clj
     (:refer-clojure :exclude [format])))

#?(:clj
   (def format #'clojure.core/format)
   :cljs
   (defn format
     "Formats a string using goog.string.format."
     [fmt & args]
     (apply goog.string/format fmt args)))

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
  (every? identity (map = target prefix)))

(defn ends-with? [target prefix]
  (every? identity (map = (reverse target) (reverse prefix))))

(defn absolute-uri-string? [s]
  "Cribbed from http://stackoverflow.com/a/19709846/190887. Takes a string
   and returns true if it represents an absolute uri (false otherwise)"
  (not (not (re-find #"(?i)^(?:[a-z]+:)?//" s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; General formatting shortcuts

(defn <> [s] (str "<" s ">"))
(defn owl> [name] (str "http://www.w3.org/2002/07/owl#" name))
(defn rdf> [name] (str "http://www.w3.org/1999/02/22-rdf-syntax-ns#" name))
(defn rdf-schema> [name] (str "http://www.w3.org/2000/01/rdf-schema#" name))
