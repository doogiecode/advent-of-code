(ns m)

(require 
  '[clojure.string :as str]
  '[clojure.edn :as edn])

; in, lines
(def inl (str/split (slurp "input.txt") #"\n"))

(defn eval-dir-1d [l idx dir dist]
  (loop [acc "" 
         i idx 
         r dist]
    (if (= 0 r)
      acc
      (let [nexti (+ i dir)
            oob (or (< nexti 0) (> nexti (count l)))]
           (if oob
               (str/join [acc (get l i)])
               (recur (str/join [acc (get l i)]) nexti (- r 1)))))))


(defn eval-oob-2d [ll idx2d]
  (let [x (get (into [] idx2d) 0)
        y (get (into [] idx2d) 1)
        limy (- (count ll) 1)
        limx (- (count (get ll y)) 1)]
    (or (< x 0) (< y 0)
        (> x limx) (> y limy))))

(defn get-2d [ll idx2d]
  ;(println idx2d)
  (get 
    (get ll (get (into [] idx2d) 1)) 
    (get (into [] idx2d) 0)))

(defn eval-dir-2d [ll idx2d dir2d dist]
  (loop [acc "" 
         i2 idx2d
         r dist]
    (if (= 0 r)
      acc
      (let [nexti2 (map + i2 dir2d)
            oob (eval-oob-2d ll nexti2)
            got (get-2d ll i2)
            joined (str/join [acc got])]
            (if oob
              joined
              (recur joined nexti2 (- r 1)))))))

(def dirs-2d
  [[1 0] [0 1] [-1 0] [0 -1] ; hor/ver
   [1 1] [-1 1] [-1 -1] [1 -1]]) ; diags

; takes line contents and line index
(defn eval-line [l i]
  (map 
  (fn [n] (map #(eval-dir-2d inl [n i] % 4) dirs-2d))
  (range 0 (count l))))
  
(defn all-eval'd []
  (map #(eval-line (nth inl %) %) (range 0 (count inl))))


; search output of this for XMAS for part 1 answer
(comment defn run [argv] (print (all-eval'd)))

(def x-dirs 
  [[-1 -1] [1 -1] [-1 1] [1 1]])

(defn mas-check [c]
  (or (= c ["M" "M" "S" "S"])
      (= c ["M" "S" "M" "S"])
      (= c ["S" "S" "M" "M"])
      (= c ["S" "M" "S" "M"])))

(def a-coords-flagged
  (map
    (fn [y] (map (fn [x] [[x y] (= \A (get-2d inl [x y]))]) (range 0 (count (nth inl y)))))
    (range 0 (count inl))))

; needs one level of flattening because it's a list of lists
; https://stackoverflow.com/questions/10723451/whats-the-one-level-sequence-flattening-function-in-clojure
(def a-coords
  (filter #(get % 1) (apply concat a-coords-flagged)))

(defn get-x-neighbor-vals [ll idx2]
  (map #(get-2d ll %) (map #(map + % idx2) x-dirs)))

(def neighbor-x-vals-for-a-coords
  (map #(get-x-neighbor-vals inl (get % 0)) a-coords))

; part 2 answer: get counts present in output of this 
; for "M M S S", "M S M S", "S S M M", "S M S M"
(defn run [argv] (print neighbor-x-vals-for-a-coords))