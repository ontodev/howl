(ns howl.util
  "Cross-platform utility functions."
  (:require
   [clojure.string :as string]
   #?(:clj [clojure.data.json :as json])
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; JSON reading and writing
(defn write-json
  [thing]
  #?(:clj (json/write-str thing)
     :cljs (clj->js thing)))

(defn read-json
  [string]
  #?(:clj (json/read-str string)
     :cljs (js->clj string)))
