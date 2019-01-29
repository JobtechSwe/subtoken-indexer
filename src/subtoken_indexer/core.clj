(ns subtoken-indexer.core
  (:require [clojure.string :as str])
  (:gen-class)
  )

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
()


(def words ["akrobat" "akrobater" "robottekniker" "maskintekniker"])

(defn update-gap [gap chars]
  (if (= gap (count chars))
    1
    (inc gap)
    )
  )

(defn word-segmenter
  ([word]
   (word-segmenter word (char-array word) 1 [] )
   )
  ([word chars gap acc]
   (if (empty? chars)
     acc
     (let [
           new-gap (update-gap gap chars)
           new-chars (if (= gap (count chars)) (rest chars) chars)
           new-acc (conj acc [(apply str(take gap chars)) word])
           ]
       (recur word new-chars new-gap new-acc)
       )
     )
   )
  )


(def all-jobs-ont (slurp "resources/ontology_all_occupations.txt"))
(def all-jobs-tax (slurp "resources/tax_occupations_lowercase.txt"))

(def all-jobs (set (concat (str/split-lines all-jobs-ont) (str/split-lines all-jobs-tax))))

(defn create-index [words]
  (group-by first (mapcat word-segmenter words))
  )

(defn order-by-token-length-and-members [coll]
  (reverse (sort-by (juxt #(count (second %))  #(count (first %) )) coll))
  )

(defn filter-by-token-length [coll]
  (filter #(< 11 (count (first %))) coll)
  )
