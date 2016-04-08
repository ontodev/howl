(ns howl.core
  (:require [clojure.string :as string]
            [instaparse.core :as insta])
  (:import [java.net URI])
  (:gen-class))

(def plain-literal "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral")
(def rdfs-label "http://www.w3.org/2000/01/rdf-schema#label")
(def xsd-string "http://www.w4.org/2001/XMLSchema#string")

;; The state map contains:
;;
;; 1. labels map from label string to IRI string
;; 2. prefixes map from prefix string to expanded string
;; 3. base-iri string
;; 5. quads vector of processed quads
;;
;; Names are resolved in this order.
;; It may also contain:
;;
;; 4. types map from predicates IRI string to datatype IRI string

(defn resolve-label
  "Given a state map with a labels map and a label,
   return the full IRI or nil."
  [{:keys [labels] :as state} label]
  (get labels label))

(defn resolve-prefixed-name
  "Given a state map with a prefixes map and a prefixed name,
   return the full IRI or nil."
  [{:keys [prefixes] :as state} prefixed-name]
  (let [[prefix suffix] (string/split prefixed-name #":" 2)]
    (when (find prefixes prefix)
      (str (get prefixes prefix) suffix))))

(defn resolve-relative-iri
  "Given a state map with a base-iri and an IRI string,
   return an absolute IRI string."
  [{:keys [base-iri] :as state} iri]
  ; TODO: make it work
  (if base-iri
    (.toString (.resolve (URI. base-iri) iri))
    iri))

(defn resolve-name
  "Given a state map and a name,
   try to resolve it as a label, then a prefix, then an IRI,
   and return the full IRI."
  [state name]
  (or (resolve-label         state name)
      (resolve-prefixed-name state name)
      (resolve-relative-iri  state name)))


;; Prefixes

(defn colon-split
  [line]
  (->> (string/split line #": " 2)
       (map string/trim)))

(defn unwrap-iri
  [iri]
  (-> iri
      string/trim
      (string/replace #"^<" "")
      (string/replace #">$" "")))

(defn add-prefix
  "Given a HOWL state and a PREFIX line,
   update and return the HOWL state."
  [state number line]
  (let [[prefix iri]
        (colon-split (string/replace line "PREFIX" ""))
        iri (unwrap-iri iri)]
    (when (find (:prefixes state) prefix)
      (throw
       (Exception.
        (format "PREFIX '%s' already defined at line %d" prefix number))))
    (assoc-in state [:prefixes prefix] iri)))

(defn add-label
  "Given a HOWL state and a LABEL line,
   update and return the HOWL state."
  [state number line]
  (let [[iri label]
        (colon-split (string/replace line "LABEL" ""))
        iri (or (resolve-prefixed-name state iri)
                (resolve-relative-iri  state iri))]
    (when (find (:labels state) label)
      (throw
       (Exception.
        (format "LABEL '%s' already defined at line %d" label number))))
    (assoc-in state [:labels label] iri)))

(defn add-type
  "Given a HOWL state and a TYPE line,
   update and return the HOWL state."
  [state number line]
  (let [[predicate iri]
        (colon-split (string/replace line "TYPE" ""))
        predicate (resolve-name state predicate)
        iri (resolve-name state iri)]
    (when (find (:types state) predicate)
      (throw
       (Exception.
        (format "TYPE for predicate '%s' already set at line %d" predicate number))))
    (assoc-in state [:types predicate] iri)))

(def subject-parser
  (insta/parser
   "S = <'SUBJECT' SPACES> NAME <':' SPACES> LABEL <EOL>
    SPACES = ' '+
    <NAME> = PREFIXED_NAME | WRAPPED_IRI | IRI
    PREFIXED_NAME = PREFIX ':' LOCAL_NAME
    <PREFIX> = #'\\w+'
    <LOCAL_NAME> = #'\\w+'
    WRAPPED_IRI = <'<'> IRI <'>'>
    <IRI> = #'[^>\\s]+'
    <LABEL> = #'[^:]+'
    EOL = ' '*"))

(defn add-quad
  [state graph subject predicate object datatype]
  (update-in
   state
   [:quads]
   (fnil conj [])
   [graph subject predicate object datatype]))

(defn add-subject
  "Given a HOWL state and a SUBJECT line,
   update and return the HOWL state."
  [state number line]
  (let [[subject label]
        (colon-split (string/replace line "SUBJECT" ""))
        iri (or (resolve-prefixed-name state subject)
                (resolve-relative-iri  state subject))]
    (when (find (:labels state) label)
      (throw
       (Exception.
        (format "Label '%s' already defined at line %d" label number))))
    (-> state
        (assoc-in [:labels label] iri)
        (assoc-in [:subject] iri)
        (add-quad (:graph state) iri rdfs-label label xsd-string))))

(defn process-literal-object
  [state literal]
  (let [[_ lang-content lang]  (re-matches #"^(.*)(@\w+)\s*$" literal)
        [_ type-content curie] (re-matches #"^(.*)\^\^(\S+)\s*$" literal)
        iri (when curie (resolve-prefixed-name state curie))]
    [(or lang-content type-content literal)
     (or lang iri)]))

(defn process-literal-statement
  [state number line]
  (let [[predicate object]       (string/split line #": " 2)
        predicate                (resolve-name state (string/trim predicate))
        [object object-datatype] (process-literal-object state object)]
    (add-quad
     state
     (:graph state)
     (:subject state)
     predicate
     object
     (or object-datatype
         (get-in state [:types predicate])
         plain-literal))))

(defn process-iri-statement
  [state number line]
  (let [graph     (:graph state)
        subject   (:subject state)
        [predicate object] (string/split line #":> " 2)
        predicate (resolve-name state (string/trim predicate))
        object    (resolve-name state (string/trim object))]
    (add-quad state graph subject predicate object nil)))

(defn process-statement
  [state number line]
  (cond
    (.contains line ": ")
    (process-literal-statement state number line)

    (.contains line ":> ")
    (process-iri-statement state number line)

    :else
    (throw (Exception. (str "Unhandled line: " number " " line)))))

(defn process-line
  "Given a HOWL collection and a line of HOWL,
   update the collection."
  [state number line]
  (try
    (cond
      ; TODO
      (.startsWith line "BASE ")
      (throw (Exception. (str "BASE not yet supported: " number " " line)))

      (.startsWith line "PREFIX ")
      (add-prefix state number line)

      (.startsWith line "LABEL ")
      (add-label state number line)

      (.startsWith line "TYPE ")
      (add-type state number line)

      (.startsWith line "GRAPH ")
      (throw (Exception. (str "GRAPH not yet supported: " number " " line)))

      (.startsWith line "SUBJECT ")
      (add-subject state number line)

      ; TODO: ANNOTATION
      (.startsWith line ">")
      (throw (Exception. (str "Annotations not yet supported: " number " " line)))

      ; STATEMENT
      :else
      (process-statement state number line))

    (catch Exception e
      (do (println state)
          (throw e)))))

(defn print-triples!
  [state]
  (doseq [[graph subject predicate object datatype] (:quads state)]
    ;(println subject predicate object datatype)
    (println
     (cond
       (not (string? datatype))
       (format "<%s> <%s> <%s> ."
               subject
               predicate
               object)

       (.startsWith datatype "@")
       (format "<%s> <%s> \"%s\"%s ."
               subject
               predicate
               object
               datatype)

       (= datatype plain-literal)
       (format "<%s> <%s> \"%s\" ."
               subject
               predicate
               object)
       
       :else
       (format "<%s> <%s> \"%s\"^^<%s> ."
               subject
               predicate
               object
               datatype))))

  (dissoc state :quads))

(defn merge-lines
  [lines number line]
  (cond
    (.startsWith line "  ")
    (update-in lines [(dec (count lines)) 1] str "\\n" (subs line 2))
    (string/blank? line)
    (update-in lines [(dec (count lines)) 1] str "\\n" line)
    :else
    (conj lines [(inc number) line])))

(defn trim-line
  [line]
  (-> line
      (string/replace #"^(\\n)+" "")
      (string/replace #"(\\n)+$" "")))


#_(defn howl2n3
    "Convert a HOWL string to N3"
    [text]
    (->> text
         string/split-lines
         (take-while #(not (.startsWith % ">")))
         vec
         (reduce-kv merge-lines [])
         (map trim-line)
         (reduce process-line {})
         println))

(defn howl2n3
  [lines]
  (loop [state  {}
         number 1
         current-line (first lines)
         lines  (rest lines)]
    (let [next-line (first lines)]
      ;(println number current-line)
      (cond
        (not (string? next-line))
        (print-triples!
         (process-line state number (trim-line current-line)))

        (.startsWith next-line "  ")
        (recur
         state
         (inc number)
         (str current-line "\\n" (subs next-line 2))
         (rest lines))

        (string/blank? next-line)
        (recur
         state
         (inc number)
         (str current-line "\\n" next-line)
         (rest lines))

        :else
        (recur
         (print-triples!
          (process-line state number (trim-line current-line)))
         (inc number)
         next-line
         (rest lines))))))

;(def test1 (slurp "test1.howl"))


(defn -main
  [& args]
  (->> args first slurp string/split-lines howl2n3))
