(ns m)

(require 
  '[clojure.string :as str]
  '[clojure.edn :as edn])

(def in2 (str/split (slurp "input.txt") #"\n\n"))
(def inrl (str/split (get in2 0) #"\n"))
(def inul (str/split (get in2 1) #"\n"))

; parsed input of rule lines
(def rules 
  (map #(let [s12 (str/split % #"[|]")]
          [(edn/read-string (get s12 0)) 
           (edn/read-string (get s12 1))]) inrl))

(def updates 
  (map #(let [ss (str/split % #",")]
          (map (fn [s] (edn/read-string s)) ss)) inul))

; https://stackoverflow.com/a/8087667
(defn index-of [item coll]
  (count (take-while (partial not= item) coll)))

(defn check-rule [update rule]
  (let [i1 (index-of (get rule 0) update)
        i2 (index-of (get rule 1) update)
        c (count update)]
    (if 
     (or (= i1 c) (= i2 c)) 
      true ; missing case so rule can't be violated
      (< i1 i2))))

(defn check-update [update rules]
  (let [results (map #(check-rule update %) rules)]
    (= (count (filter #(= false %) results)) 0)))

; for part 2
(defn check-update-surface-offender [update rules]
  (let [remainder (drop-while #(check-rule update %) rules)]
    (first remainder)))

; for part 2
(defn swap-until-pass [update rules] 
  (loop [u update] 
    (let [offender (check-update-surface-offender u rules)]
      (println offender)
      (if (= 0 (count offender))
        u 
        (let [o1 (nth offender 0)
              o2 (nth offender 1)
              swapped (replace {o1 o2 o2 o1} u)]
          (recur swapped))))))

(def passing-updates
  (filter #(check-update % rules) updates))

; odd count list
(defn get-middle [ocl]
  (nth ocl (int (/ (count ocl) 2))))

(def middles-passing
  (map #(get-middle %) passing-updates))

; answer to part one
(comment defn run [argv] (print (reduce + middles-passing)))

(def non-passing-updates
  (filter #(not (check-update % rules)) updates))

; naive attempt at "shake the snowglobe" sort for part 2
(defn fix-update [update rules]
  (loop [u update]
    (if (check-update u rules) u (recur (shuffle u)))))

(defn rand-int-until-not [ceiling avoid]
  (loop [r (rand-int ceiling)]
    (if (not= avoid r)
      r (recur (rand-int ceiling)))))

(defn fix-update-swap [update rules]
  (loop [u update]
    (if (check-update u rules) u 
        (recur (let [c (count u)
                     p1 (rand-int c)
                     p2 (rand-int-until-not c p1)
                     e1 (nth update p1)
                     e2 (nth update p2)]
                 (replace {e1 e2 e2 e1} update))))))

(def fixed-updates 
  (map #(swap-until-pass % rules) non-passing-updates))

(def middles-fixed
  (map #(get-middle %) fixed-updates))

; answer to part two
(defn run [argv] (print (reduce + middles-fixed)))


(comment defn run [argv] 
  (let [fixed (swap-until-pass (first non-passing-updates) rules)]
    (println fixed)
    (println (check-update fixed rules))))
