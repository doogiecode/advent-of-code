
(def in (slurp "input.txt"))

(def spl (clojure.string/split in #"\n"))


(defn re-pos [re s]
        (loop [m (re-matcher re s)
               res []]
          (if (.find m)
            (recur m (conj res (.start m)))
            res)))
            
(defn positions [s] (re-pos #"(?=(one|two|three|four|five|six|seven|eight|nine))" s))

(def pos (map #(vector % (positions %)) spl))

(def p1p2 (map #(if (< 0 (count (% 1))) (split-at (first (take-last 1 (% 1))) (% 0)) (% 0)) pos))

(def strsub
    (->> spl
        (map #(clojure.string/replace % #"eight" "8"))
        (map #(clojure.string/replace % #"five" "5"))
        (map #(clojure.string/replace % #"four" "4"))
        (map #(clojure.string/replace % #"nine" "9"))
        (map #(clojure.string/replace % #"one" "1"))
        (map #(clojure.string/replace % #"seven" "7"))
        (map #(clojure.string/replace % #"six" "6"))
        (map #(clojure.string/replace % #"three" "3"))
        (map #(clojure.string/replace % #"two" "2"))
    ))
    
(defn subfn [s]
        (-> s
        (clojure.string/replace #"eight" "8")
        (clojure.string/replace #"five" "5")
        (clojure.string/replace #"four" "4")
        (clojure.string/replace #"nine" "9")
        (clojure.string/replace #"one" "1")
        (clojure.string/replace #"seven" "7")
        (clojure.string/replace #"six" "6")
        (clojure.string/replace #"three" "3")
        (clojure.string/replace #"two" "2")
    ))
    
(def subunity (map #(if (string? %) (subfn %) (str (subfn (apply str (% 0))) (subfn (apply str (% 1))))) p1p2))


;(def nums (map #(clojure.string/replace % #"\D" "") spl))
(def nums (map #(clojure.string/replace % #"\D" "") strsub))

; part 1
(def fl (map #(vector (first %) (first (take-last 1 %))) nums))
(def fls (map #(str (% 0) (% 1)) fl))
(reduce + (map #(clojure.edn/read-string %) fls))

; part 2 but this solution (including above) was not totally adequate
; and I ended up getting lucky and handling the line "xoneight2"
; manually after noticing it was not handled successfully
(def nums (map #(clojure.string/replace % #"\D" "") subunity))
(def fl (map #(vector (first %) (first (take-last 1 %))) nums))
(def fls (map #(str (% 0) (% 1)) fl))
(reduce + (map #(clojure.edn/read-string %) fls))
