(ns decisiontree.core
  (:gen-class)
  (:require [clojure.java.io :as io]))
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(defn cast-first-as-int
  "cast the first element of a vector to an int"
  [vec]
  (into [] (concat [(read-string (first vec))] (rest vec))))

(defn load-csv
  "loading a csv, skips the first row"
  [filepath]

  (with-open [in-file (io/reader filepath)]
    (map cast-first-as-int (rest (doall (csv/read-csv in-file))))))

(defn in?
    "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))


(defn response-rate
  "calculate the response rate of a dataset"
  [data]
  (if (= (count data) 0)
      0
      (float (/ (reduce + (map first data)) (count data)))))

(defn gini
  "calculate the gini coefficient of a dataset"
  [data]
  (let [rr (response-rate data)]
    (* rr (- 1 rr))))

(defn counts-and-response-rates
  "get the  counts and response rates of a categorical column for the different levels"
  [data n]
  (let [values (distinct (map (fn [v] (nth v n)) data))]
    (sort-by last
     (map (fn [v]
            (let [subset (filter #(= (nth % n) v) data)]
              (vector v  (count subset) (response-rate subset))))
          values))))

(defn combos
  "get all the combos"
  [coll]

  (map #(take % coll) (range (inc (count coll)))))

(defn ginis
  "get the gini coeffecients of a categorical column for the different combos of levels ordered by individual gini. By default levels of the variable are added in increasing order of conversion rate, if reversed=true then they are successively removed instead"
  [data n &[reversed]]
  (let [values (distinct (map (fn [v] (nth v n)) data))
        levels (map first (counts-and-response-rates data n))
        reverse (if reversed reverse #(into [] %))]
             (reverse (map (fn [level-list]
                    (let [subset (filter #(in? level-list (nth % n)) data)]
                      (vector level-list (count subset) (gini subset))))
                  (combos (reverse levels))))))

(defn ginis-reverse
  "get the gini coeffecients of a categorical column for the different combos of levels ordered by individual gini; (todo merge this into the main ginis function)"
  [data n]
  (let [values (distinct (map (fn [v] (nth v n)) data))
        levels (map first (counts-and-response-rates data n))]
   (reverse  (map (fn [level-list]
           (let [subset (filter #(in? level-list (nth % n)) data)]
             (vector level-list (count subset) (gini subset))))
         (combos (reverse levels))))))

(defn get-best-split-of-variable
  "get the best split of a variable as well as the sum of gini's for that split, by computing the sum of the gini's for all the splits obtained by the combos of splits, ordered by response rate."
  [data n]
  (let [zip (map vector (ginis data n)  (ginis data n true))
        ginis (map #(+ (last (first %)) (last (last %))) zip)
        minindex  (first (apply min-key last (map-indexed vector ginis)))]
         (nth zip minindex)))

(defn split
  "Given a dataset, split into two segments with the best split.
  Assume the responder variable is in the first column, and all variables are categorical"
  [data]
  (let [zip (map #(get-best-split-of-variable data %) (range 1 (count (first data))))
        ginis (map #(+ (last (first %)) (last (last %))) zip)
        minindex (first (apply min-key last (map-indexed vector ginis)));
        leftlevels (first (first (nth zip minindex)))
        rightlevels (first (last (nth zip minindex)))
        n (inc minindex)]
    (vector (filter #(in? leftlevels (nth % n)) data)
            (filter #(in? rightlevels (nth % n)) data)
    n
    leftlevels
    rightlevels)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
