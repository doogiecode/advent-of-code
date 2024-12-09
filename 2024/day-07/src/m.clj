(ns m)

(require
 '[clojure.string :as str]
 '[clojure.edn :as edn])

(def inl (str/split (slurp "input.txt") #"\n"))

; colon split
(def csinl (map #(str/split % #"[:] ") inl))

(defn parse-one [l]
  [(edn/read-string (nth l 0))
   (map edn/read-string (str/split (nth l 1) #" "))])

(def parsed (map parse-one csinl))

(defn sprout-op-combos-n-times [n]
  (loop [acc [[+] [*]] this-n 1]
    (if (= n this-n)
      acc
      (let [next-acc (apply concat
                            (map #(vector (cons + %) (cons * %))
                                 acc))]
        (recur next-acc (+ 1 this-n))))))

(defn eval-nums-and-ops [nums ops]
  (let [op-num-pairs (map #(vector %1 %2) ops (rest nums))]
    (reduce
     #((nth %2 0) %1 (nth %2 1))
     (first nums) op-num-pairs)))

(defn eval-one [[sum nums]]
  (let [spaces (- (count nums) 1)
        ops-lists (sprout-op-combos-n-times spaces)
        res (drop-while 
             #(not= sum (eval-nums-and-ops nums %)) 
             ops-lists)]
    (< 0 (count res))))

(def all-eval'd (map #(vector % (eval-one %)) parsed))

(def part-1-answer 
  (let [successes (filter #(= true (nth % 1)) all-eval'd)
        sum (reduce + (map #(nth (nth % 0) 0) successes))]
    sum))

(defn numeric-concat [n1 n2]
  (edn/read-string (str/join [(.toString n1) (.toString n2)])))

(defn sprout-op-combos-n-times-3-ops [n]
  (loop [acc [[+] [*] [numeric-concat]] this-n 1]
    (if (= n this-n)
      acc
      (let [next-acc (apply concat
                            (map #(vector 
                                   (cons + %) 
                                   (cons * %) 
                                   (cons numeric-concat %))
                                 acc))]
        (recur next-acc (+ 1 this-n))))))


(defn eval-one-3-ops [[sum nums]]
  (let [spaces (- (count nums) 1)
        ops-lists (sprout-op-combos-n-times-3-ops spaces)
        res (drop-while
             #(not= sum (eval-nums-and-ops nums %))
             ops-lists)]
    (< 0 (count res))))

(def all-eval'd-3-ops (map #(vector % (eval-one-3-ops %)) parsed))

(def part-2-answer
  (let [successes (filter #(= true (nth % 1)) all-eval'd-3-ops)
        sum (reduce + (map #(nth (nth % 0) 0) successes))]
    sum))