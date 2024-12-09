(ns m 
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(require '[clojure.string :as str])
(require '[clojure.edn :as edn])

; in lines
(def inl (str/split (slurp "input.txt") #"\n"))

; remove spaces
(def rs (map #(str/replace % " " "") inl))

; colon split
(def cs (map #(str/split % #"[:]") rs))

; game del, grab split
(def gdgs (map #(vector (str/replace (% 0) "Game" "") (str/split (% 1) #"[;]")) cs))

; game parse, within grab split
(def gpwgs 
  (map (fn [l] 
         (vector
          (edn/read-string (l 0))
          (map #(str/split % #"[,]") (l 1)))) 
       gdgs))

(def lmap (map #(hash-map :id (% 0) :grabs (% 1)) gpwgs))

(defn parse-game [grabs]
   (reduce conj (map (fn [g]
               (let [nstr (re-find #"\d+" g)
                     c (keyword (str/replace g nstr ""))
                     n (edn/read-string nstr)]
                 (hash-map c n))) grabs)))

(def plmap (map #(assoc % :grabs (map parse-game (% :grabs))) lmap))

(def compare-bag {:red 12 :green 13 :blue 14})

; ridiculous 
; https://stackoverflow.com/questions/9218044/in-clojure-how-to-apply-and-to-a-list
(defn is-possible [game bag]
  (every? identity (map (fn [grab] (and
                                    (>= (bag :red) (grab :red))
                                    (>= (bag :green) (grab :green))
                                    (>= (bag :blue) (grab :blue)))) game)))

(defn is-possible [game bag]
  (every? identity 
          (map (fn [grab] 
                 (every? identity 
                         (let [gks (keys grab)] 
                           (map #(>= (bag %) (grab %)) gks)))) 
               game)))


(def pplmap (map 
             #(assoc % :possible (is-possible (% :grabs) compare-bag)) 
             plmap))

(filter false pplmap)

(reduce + (map #(% :id) (filter #(% :possible) pplmap)))

(defn nsmax [n1 n2] (apply max (filter some? [n1 n2])))
(defn take-maxes [g1 g2]
  (reduce conj (map #(hash-map % (nsmax (g1 %) (g2 %))) (keys g1))))
(defn maxes [grabs]
  (reduce take-maxes {:red 0 :green 0 :blue 0} grabs))

(def mpplmap (map #(assoc % :maxes (maxes (% :grabs))) pplmap))

(def pmpplmap (map #(assoc % :power (* 
                                     ((% :maxes) :red)
                                     ((% :maxes) :blue)
                                     ((% :maxes) :green))) 
                   mpplmap))

(reduce + (map #(% :power) pmpplmap))

(defn run [a] (println "hi")
  (print inl))