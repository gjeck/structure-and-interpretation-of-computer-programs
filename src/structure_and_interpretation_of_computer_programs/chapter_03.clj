;; Exercise 3.1
;; An accumulator is a procedure that is called repeatedly with a single numeric argument
;; and accumulates its arguments into a sum. Write a procedure `make-accumulator` that generates
;; accumulators.
(defn make-accumulator [initial]
  (let [acc (ref {:count initial})]
    (fn inc [amount] (dosync (alter acc update-in [:count] + amount)))))

;; Exercise 3.2
;; In software-testing applications, it is useful to be able to count the number of times
;; a given procedure is called during the course of computation. Write a procedure `make-monitored`
;; that takes as input a procedure and returns a procedure for monitoring the count.
(defn make-monitored [f]
  (let [acc (make-accumulator 0)]
    (fn mf [input]
      (cond (= input :how-many-calls?) ((acc 0) :count)
            (= input :reset) (acc (- ((acc 0) :count)))
            :else (do (acc 1)
                      (f input))))))
