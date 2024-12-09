(ns m)

(require
 '[clojure.string :as str]
 '[clojure.edn :as edn])

(def in-grid (str/split (slurp "input.txt") #"\n"))

(defn detect-oob [grid [x y]]
  (let [limit-x (count (first grid))
        limit-y (count grid)]
    (or (< x 0) (>= x limit-x)
        (< y 0) (>= y limit-y))))

(defn get-spot [grid [x y]]
  (nth (nth grid y) x))

(def next-dir-map
  {[0 -1] [1 0] 
   [1 0] [0 1] 
   [0 1] [-1 0] 
   [-1 0] [0 -1]})

(defn detect-starter [grid]
  (let [x-search (range 0 (count (first grid)))
        y-search (range 0 (count grid))
        all-points (apply concat 
                          (map 
                           (fn [y] (map #(vector % y) x-search)) 
                           y-search))]
  (first (filter #(= \^ (get-spot grid %)) all-points))))

(defn accumulate-path-until-out [grid start]
  (loop [here start acc [] dir [0 -1]]
    (let [ahead-coord (map + here dir)]
      (if (detect-oob grid ahead-coord)
        (cons here acc)
        (let [next-dir (if (= \# (get-spot grid ahead-coord))
                         (get next-dir-map dir) dir)
              next-here (map + here next-dir)]
          (recur next-here (cons here acc) next-dir))))))

; answer to part 1
(comment defn run [argv] 
  (let [path (accumulate-path-until-out in-grid (detect-starter in-grid))]
    (print (count (set path)))))

; approach for part 2:
; seems like every possible loop will be a rectangle
; the rectangle involves an obstruction on the path
; update: changed approach and the new approach feels like cheesing it

(def grid-for-testing-infinity (str/split (slurp "input2.txt") #"\n"))

(defn probably-true-if-infinite [grid start]
  (let [grid-size (* (count grid) (count (first grid)))]
   (loop [here start acc [] dir [0 -1]]
   (let [ahead-coord (map + here dir)] 
     (if (detect-oob grid ahead-coord)
       false 
       (let [next-dir (if (= \# (get-spot grid ahead-coord))
                        (get next-dir-map dir) dir)
             next-here (map + here next-dir)]
         (if (> (count acc) grid-size) 
           true 
           (recur next-here (cons here acc) next-dir))))))))

(defn check-for-cycle [path]
  (let [f (first path)
        [rp1 rest1] (split-with #(not= f %) (rest path))
        rp2 (take-while #(not= f %) (rest rest1))
        c1 (count rp1)
        c2 (count rp2)]
    (and (< 0 c1) (< 0 c2) (= c1 c2))))

(defn probably-true-if-infinite-faster [grid start]
  (let [grid-size (* (count grid) (count (first grid)))]
    (loop [here start acc [] dir [0 -1]]
      (let [ahead-coord (map + here dir)]
        (if (detect-oob grid ahead-coord)
          false
          (let [next-dir (if (= \# (get-spot grid ahead-coord))
                           (get next-dir-map dir) dir)
                next-here (map + here next-dir)]
            (if (and (< 300 (count acc)) (check-for-cycle acc))
              true
              (recur next-here (cons here acc) next-dir))))))))

; https://stackoverflow.com/a/56393859
(defn replace-at [s idx replacement]
  (str (subs s 0 idx) replacement (subs s (inc idx))))

(defn replace-with-# [grid [x y]]
  (assoc grid y (replace-at (nth grid y) x "#")))

(defn accumulate-infinity-count-until-out [grid start]
  (loop [here start acc [] dir [0 -1] acc2ic []]
    (let [ahead-coord (map + here dir)]
      (if (detect-oob grid ahead-coord) 
        [(cons here acc) acc2ic]
        (let [next-dir (if (= \# (get-spot grid ahead-coord))
                         (get next-dir-map dir) dir)
              next-here (map + here next-dir)
              grid-with-new-obstacle (replace-with-# grid next-here)
              maybe-infinite-with-obstacle (probably-true-if-infinite-faster
                                            grid-with-new-obstacle here)
              next-acc2ic (if maybe-infinite-with-obstacle 
                            acc2ic (cons next-here acc2ic))]
          (if maybe-infinite-with-obstacle (println next-here))
          (recur next-here (cons here acc) next-dir next-acc2ic))))))


(defn run [argv] 
  (let [res (accumulate-infinity-count-until-out 
             in-grid 
             (detect-starter in-grid))]
    (println res)
    (println (count (set (second res))))))
