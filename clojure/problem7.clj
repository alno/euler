(defn noDivisors [seq x] (cond
                           (empty? seq) true
                           (> (* (first seq) (first seq)) x) true
                           (= 0 (mod x (first seq))) false
                           :else (noDivisors (rest seq) x) ))

(def primes (lazy-cat [2 3] (filter (partial noDivisors primes) (iterate (partial + 2) 5)) ))

(defn prime_10001 [] (nth primes 10000))

(defn main [] (prime_10001))