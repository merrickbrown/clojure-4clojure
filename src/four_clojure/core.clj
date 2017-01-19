(ns four-clojure.core
  (:gen-class))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
;; Problem 164

(defn map-key-union
  [coll [key value]]
  (assoc coll key (if (contains? coll key)
                    (into value (get coll key))
                    value)))
(defn base-state
  [dfa-def]
  (into {} (for [state (:states dfa-def)] [state #{}])))

(defn setup-machine
  [dfa-def]
  (assoc (base-state dfa-def) (:start dfa-def) #{""}))

(defn apply-transitions
        [machine-state transitions]
        (reduce map-key-union {}
                (for [[state strings] machine-state
                      [letter next-state] (get transitions state)]
                  [next-state (set (map #(str % letter) strings))])))

(defn states
  ([dfa-def]
   (states dfa-def (setup-machine dfa-def)))
  ([dfa-def state]
   (lazy-seq
    (cons state
          (states dfa-def
                  (apply-transitions state (:transitions dfa-def)))))))

(defn accepting
  [accepting-states machine-state]
  (into #{} (for [state accepting-states
                        strings (get machine-state state)]
                    strings)))

(defn accepted-strings-after-n-steps
  [dfa-def n]
  (reduce into '() (filter
                    #(> (count %) 0)
                    (map
                     (partial accepting (:accepts dfa-def))
                     (take n (states dfa-def))))))

(defn accepted-strings-on-nth-step
  [dfa-def n]
  (into '() (accepting (:accepts dfa-def) (nth (states dfa-def) n))))

(defn accepted-strings
  [dfa-def]
  (map (partial accepting (:accepts dfa-def)) (states dfa-def)))


  


(def sample-dfa-def-1 '{:states #{q0 q1 q2 q3 q4}
                        :alphabet #{"v" "w" "x" "y" "z"}
                        :start q0
                        :accepts #{q4}
                        :transitions {q0 {"v" q1, "w" q1, "x" q1, "y" q1, "z" q1}
                                      q1 {"v" q2, "w" q2, "x" q2, "y" q2, "z" q2}
                                      q2 {"v" q3, "w" q3, "x" q3, "y" q3, "z" q3}
                                      q3 {"v" q4, "w" q4, "x" q4, "y" q4, "z" q4}}})

(def sample-dfa-def-2 '{:states #{q0 q1 q2 q3 q4 q5 q6 q7 q8 q9}
                           :alphabet #{"i" "l" "o" "m" "p" "t"}
                           :start q0
                           :accepts #{q5 q8}
                           :transitions {q0 {"l" q1}
                                         q1 {"i" q2, "o" q6}
                                         q2 {"m" q3}
                                         q3 {"i" q4}
                                         q4 {"t" q5}
                                         q6 {"o" q7}
                                         q7 {"p" q8}
                                         q8 {"l" q9}
                                         q9 {"o" q6}}})

(def sample-dfa-def-3 '{:states #{q0 q1}
                           :alphabet #{"0" "1"}
                           :start q0
                           :accepts #{q0}
                           :transitions {q0 {"0" q0, "1" q1}
                                         q1 {"0" q1, "1" q0}}})

(def sample-dfa-def-4 '{:states #{q0 q1 q2 q3 q4 q5 q6 q7}
              :alphabet #{"e" "h" "i" "l" "o" "y"}
              :start q0
              :accepts #{q2 q4 q7}
              :transitions {q0 {h q1}
                            q1 {"i" q2, "e" q3}
                            q3 {"l" q5, y q4}
                            q5 {"l" q6}
                            q6 {"o" q7}}})

;;Problem 156

;;Problem 64
(defn my-group-by
  [f coll]
  (into {} (for [fval (set (map f coll))]
             [fval (into [] (filter #(= fval (f %)) coll))])))
;;Problem 65
(defn list-or-vector
"input must be a lsit or a vector"
  [lorv]
  (if
      (let [s1 (gensym) s2 (gensym)]
        (= s2 (last (conj (conj lorv s1) s2))))
    :vector
    :list)
)

(defn ismap?
  [coll]
  (let [s1 (gensym)
        s2 (gensym)
        s3 (gensym)]
    (= (inc (count coll))
       (count
        (conj
         (conj
          coll
          (vector s1 s2))
         (vector s1 s3))))))


(defn isset?
  "cant be a map"
  [coll]
  (let [s1 (gensym)]
    (= (inc (count coll))
       (count (conj (conj coll s1) s1)))))

(defn get-type
  [coll]
  (if (ismap? coll)
    :map
    (if (isset? coll)
      :set
      (list-or-vector coll))))

;; Problem 67
(defn divisible
  [divide-by m]
  (some (fn [u] (= 0 (mod m u))) divide-by))


(defn next-not-divisible
  [coll]
  (first
   (filter
    (fn [s] (not (divisible coll s)))
    (map (fn [t] (+ t (last coll))) (range)))))

(defn primes [n]
  (seq  (nth (iterate (fn [prev-primes] (conj prev-primes ((fn [coll]
  (first
   (filter
    (fn [s] (not ((fn [divide-by m]
  (some (fn [u] (= 0 (mod m u))) divide-by)) coll s)))
    (map (fn [t] (+ t (last coll))) (range))))) prev-primes))) [2]) (dec n))))


;;problem 69
(defn combine
  [f map1 map2]
  (into {} (for [k (into #{} (concat (keys map1) (keys map2)))]
             [k (if-let [v1 (get map1 k)]
                  (if-let [v2 (get map2 k)]
                    (f v1 v2)
                    v1)
                  (get map2 k))])))

(defn merge-with-func
  [func firstmap & restmaps]
  (if (empty? restmaps)
    firstmap
    (apply (partial  merge-with-func func (combine func firstmap (first restmaps))) (rest restmaps))))

;;problem 70

(defn words
  [s]
  (clojure.string/split
   (clojure.string/replace s "") #"\s+")
  )

(defn case-insenstive-sort
  [str-coll]
  (sort
   #(compare
     (clojure.string/lower-case %1)
     (clojure.string/lower-case %2))
   str-coll))

;;problem 71
(defn babylon-step
  [s guess]
  (/ (+ guess (/ s guess)) 2))

(defn perfect-square?
  [s]
  (loop [guess 1]
    (let [next (babylon-step s guess)]
      (if (and (< -1 (- next guess)) (> 1 (- next guess)))
        (= s (* (int next) (int next)))
        (recur next)))))

(defn strictly-between?
  [x a b]
  (if (> a b)
    (strictly-between? x b a)
    (and (> x a) (< x b))))
;;problem 75

(defn gcd
  [a b]
  (if (> a b)
    (gcd b a)
    (if (= a 0)
      b
      (gcd (mod b a) a))))

(defn coprime?
  [n m]
  (= 1 (gcd n m)))

;;problem 77

(defn sorted-chars
  [s]
  (sort (seq (char-array s))))

(defn set-group-by
  [f coll]
  (map set (vals (group-by f coll))))

;;problem 53

(defn adjacent-pairs
  [coll]
  (partition 2 (rest (flatten (map #(list  % %) coll)))))

(defn longest-inc-subseq
  [seq]
  (let [pairs
        (first (sort
                #(> (count %1) (count %2))
                (filter
                 #(apply < (first %))
                 (partition-by
                  #(apply < %)
                  (adjacent-pairs seq))
                        )))]
    (filter (complement nil?) (cons (first (first pairs)) (for [pair pairs]
                                                            (last pair))))))

;;problem 54

(defn my-partition
  [n coll]
  (if (> n (count coll))
    ()
    (cons
     (take n coll)
     (my-partition n (drop n coll)))))
;;problem 55
(defn my-freq
  [coll]
  (into {} (map #(vector (first %) (count (second %))) (group-by identity coll))))

;;problem 56

(defn my-distinct
  [coll]
  (loop [rest-coll coll
         uniques []]
    (if (empty? rest-coll)
      uniques
      (recur
       (rest rest-coll)
       (if (<= 0 (.indexOf uniques (first rest-coll)))
         uniques
         (conj uniques (first rest-coll)))))
    ))

;;problem 58
(defn my-comp [& fs]
  (loop [f (first fs)
         rfs (rest fs)]
    (if (empty? rfs)
      f
      (recur #(f (apply (first rfs) %&)) (rest rfs)))))


;; graph theory stuff
(defn graph-from-edge-set
  "Produces a graph from edge and vertex data. Assumes each element of edges is a set of two elements"
  [vertices edges]
  (into {} (for [v vertices]
             [v
              (into #{}
                    (map
                     (fn
                       [pair]
                       (first (filter #(not= % v) pair)))
                     (filter #(contains? % v) edges)))])))




;; riddler puzzle
(defn random-mapping
  [n]
  (for
      [i (range n)]
    (let [r (-> n dec rand int)]
      (if  (< r i)
        r
        (inc r)))))

(defn remove-random
  [n]
  (- n (count (into #{} (random-mapping n)))))

(defn number-remaining
  [n]
  (loop [image #{} i n]
    (if (> i 0)
      (let [r (-> n dec rand int)]
        (if  (< r (dec i))
          (recur (conj image r) (dec i))
          (recur (conj image (inc r)) (dec i))))
      (- n (count image)))))

(defn any-left-random?
  [n]
  (loop [remaining n]
    (if (= remaining 0)
      false
      (if (= remaining 1)
        true
        (recur (number-remaining remaining))))))



;;problem 168
(defn infinite-matrix
  ([f]
   (map
    (fn
      [row]
      (map
       (fn
         [col]
         (f row col))
       (range)))
    (range)))
  ([f m n]
   (drop m (map #(drop n %) (infinite-matrix f))))
  ([f m n s t]
   (take s (map #(take t %) (infinite-matrix f m n))))
  )

;;problem 97
(defn reverse-tree
  [tree]
  (if (nil? tree)
    tree
    (list
     (first tree)
     (reverse-tree (last tree))
     (reverse-tree (second tree)))))
