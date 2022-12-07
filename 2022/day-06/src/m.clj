(ns m)

(require '[clojure.string :as str])

; change 4 to 14 for part 2
(defn eat [i recent remain] 
  (let [nextc (first remain)
        new-rem (rest remain)
        new-rec (if (= 4 (count recent)) 
                  ; the vec part is a technicality
                  ; https://stackoverflow.com/a/57602896
                  (conj (vec (rest recent)) nextc)
                  (conj recent nextc))]
        (if (= 4 (count (set recent))) 
          i
          (eat (+ 1 i) new-rec new-rem))))

; run command:
; clj -X m/run :in input.txt
(defn run [argv] 
  (println (eat 0 [] (slurp (str (get argv :in))))))