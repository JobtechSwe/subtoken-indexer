(ns subtoken-indexer.core
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
