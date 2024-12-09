(ns m)

(require 
  '[clojure.string :as str]
  '[clojure.edn :as edn])

; in, lines
(def inl (str/split (slurp "input.txt") #"\n"))

; split at spaces, making vectors of strings
(def ss (map #(str/split % #" ") inl))

; parse numbers
(defn parse-line [l] (map #(edn/read-string %) l))
(def pn (map parse-line ss))

; calculate differences
(defn cdl [l] (map #(- % %2) (rest l) l))
(def cd (map cdl pn))

; tester for increasing (all positive differences)
(defn ti [l] (= 0 (count (filter #(< % 1) l))))

; tester for decreasing (all negative differences)
(defn td [l] (= 0 (count (filter #(> % -1) l))))

; tester for magnitude of under 4
; (magnitude zero covered by strict in/decreasing)
(defn tu4 [l] (= 0 (count (filter #(> (abs %) 3) l))))

; test for a single line
(defn tl [l] (and (or (ti l) (td l)) (tu4 l)))

; overall count (answer to part 1)
(def cu4iod (count (filter tl cd)))

; https://stackoverflow.com/a/18319708
(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))
  
(defn vec-remove-cast [pos coll] (vec-remove pos (into [] coll)))

; manifest multiplicities, original and each middle item removed
(defn mm [l] 
  (let [c (count l) ; e.g. 4 for [5 6 7 8]
        indexes (range 0 c) ; e.g. [0 1 2 3] for [5 6 7 8]
        removals (map #(vec-remove-cast % l) indexes)] 
  (cons l removals)))

; make each list of numbers a mm'd list of lists
(def pnmm (map mm pn))

; calculate differences for list of lists
(defn cdll [ll] (map cdl ll))
(def cdmm (map cdll pnmm))

; test for a list of lines
(defn tll [ll] (map tl ll))

; all lists of lines tested
(def allt (map tll cdmm))

; count a list of booleans
(defn cbl [l] (count (filter identity l)))

; count with at least one true (answer to part 2)
(def count-1+-true (count (filter #(> % 0) (map cbl allt))))

(def n 4)

(defn run [argv] 
  (comment print cu4iod) (print count-1+-true)
;  (println (nth pnmm n))
;  (println (nth cdmm n))
;  (println (nth allt n))
;  (println (nth (map cbl allt) n))
  (comment print (filter #(> % 0) (map cbl allt))))