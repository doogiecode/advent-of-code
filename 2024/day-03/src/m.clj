(ns m)

(require 
  '[clojure.string :as str]
  '[clojure.edn :as edn])

(def in (slurp "input.txt"))

; find all matching muls
(def all-real-mul (re-seq #"mul[(][0-9]+,[0-9]+[)]" in))

; number parser
(defn pns [mul] (re-seq #"[0-9]+" mul))
(def all-parsed (map pns all-real-mul))

; parse and multiply
(defn pam [pmul] (* 
                  (edn/read-string (nth pmul 0)) 
                  (edn/read-string (nth pmul 1))))
(def all-p-m (map pam all-parsed))

; answer to part 1
(def sum-apm (reduce + all-p-m))

; find all mul and do and don't
(def all-real-mul-do-don't 
     (re-seq #"mul[(][0-9]+,[0-9]+[)]|do[(][)]|don[']t[(][)]" in))

; these return two lists, a first with all the stuff before the search term and
; a second with everything that didn't get into the first list (nothing lost)
(defn split-at-don't [pl] (split-with #(not (= "don't()" %)) pl))
(defn split-at-do [pl] (split-with #(not (= "do()" %)) pl))

; recurse through the sequence, splitting in alternating ways
; while alternating the value of the boolean indicating whether to keep ops
(def kept (loop [remainder all-real-mul-do-don't
                 keep? true
                 acc '()] ; initialize accumulator as an empty list literal
                (if (= 0 (count remainder))
                    acc
                    (if keep? 
                        (let [bsas (split-at-don't remainder)]
                             (recur (bsas 1) (not keep?) (concat acc (bsas 0))))
                        (let [bsas (split-at-do remainder)]
                             (recur (bsas 1) (not keep?) acc))))))

; gotta remove all the do() since it all got preserved
(defn remove-do-leftovers [fl] (filter #(not (= "do()" %)) fl))

; answer to part 2
(def kept-parsed (map pns (remove-do-leftovers kept)))
(def kept-mult (map pam kept-parsed))
(def sum-kept-mult (reduce + kept-mult))

(defn run [argv] (comment println sum-apm) (println sum-kept-mult))