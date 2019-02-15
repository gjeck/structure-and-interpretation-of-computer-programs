(defn factorial-recursive [n]
  (if (= n 1)
    1
    (* n (factorial-recursive (- n 1)))))

(defn factorial-iterative [n]
  (defn iter [product counter]
    (if (> counter n)
      product
      (iter (* counter product)
            (+ counter 1))))
  (iter 1 1))

(defn fib-recursive [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib-recursive (- n 1)) (fib-recursive (- n 2)))))

(defn fib-iterative [n]
  (defn iter [a b count]
    (if (= count 0)
      b
      (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

;; Excercise 1.12
;; The following pattern of numbers is called Pascal's triangle.
;;         1
;;       1   1
;;     1   2   1
;;   1   3   3   1
;; 1   4   6   4   1
;; The numbers at the edge of the triangle are all 1, and each number
;; inside the triangle is the sum of the two numbers above it. The elements
;; of Pascal's triangle are called the binomial coefficients, because
;; the nth row consists of the coefficients of the terms in the expansion
;; of (x + y)^n. Write a procedure that computes elements of Pascal's
;; triangle.

(defn k-combination [n k]
  "Mathematical combination n choose k"
  (if (> k n)
    0
    (/ (factorial-iterative n) (* (factorial-iterative k) (factorial-iterative (- n k))))))

(defn binomial-coefficient [n]
  (let [nums (map vector (repeat n) (range (+ n 1)))]
    (map (fn [[a b]] (k-combination a b)) nums)))

(defn pascals-triangle [n]
  (map binomial-coefficient (range n)))

;; Excersize 1.15
;; The sine of an angle (specified in radians) can be computed by making use
;; of the approximation of sin(x) ~= x if x is sufficiently small, and the trigonimic
;; identity:
;; sin(x) = 3sin(x/3) - 4sin^3(x/3)

(defn my-cube [x] (* x x x))
(defn my-sine [angle]
  (defn my-p [x] (- (* 3 x) (* 4 (my-cube x))))
  (if (not (> (math/abs angle) 0.1))
    angle
    (my-p (my-sine (/ angle 3.0)))))

;; How many times is the procedure p applied when (sine 12.15) is evaluated?
;; 5 times

(defn my-gcd [a b]
  (if (= b 0)
    a
    (my-gcd b (rem a b))))

;; Higher ordered procedures
(defn my-sum [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (my-sum term (next a) next b))))

;; Utilizing my-sum we can compute the sum of the cubes of the integers from 1 to 10:
(defn sum-cubes [a b]
  (my-sum my-cube a inc b))

;; We can also sum a range
(defn sum-integers [a b]
  (my-sum identity a inc b))

;; Excersize 1.30
;; The sum procedure above generates a linear recursion. The procedure can be rewritten so that the sum
;; is performed iteratively. Show how to do this by filling in the missing expressions in the following
;; definition:
;; (define (sum term a next b)
;;   (define (iter a result)
;;     (if <??>
;;       <??>
;;       (iter <??> <??>)))
;;   (iter <??> <??>))

(defn my-sum-iterative [term a next b]
  (defn iter [x result]
    (if (> x b)
      result
      (iter (next x) (+ result (term x)))))
  (iter a 0))

;; Lecture 1
(defn square [x]
  (* x x))

(defn average [x y]
(/ (+ x y) 2))

(defn mean-square [x y]
(average (square x)
         (square y)))

(defn abs [x]
(if (< x 0)
  (- x)
  x))

(defn my-sqrt [x]
"Approximate algo for calculating square root"
(defn improve [guess]
  (average guess (/ x guess)))
(defn good-enough? [guess]
  (< (abs (- (square guess) x)) 0.001))
(defn try-guess [guess]
  (if (good-enough? guess)
    guess
    (try-guess (improve guess))))
(try-guess 1.0))

(defn sum-square [x y]
(+ (square x) (square y)))

;; Tower of Hanoi
(defn hanoi-move [n from to spare]
(cond (= n 0) "done"
      :else (do (hanoi-move (- n 1) from spare to)
                (println (str "from " from " to " to))
                (hanoi-move (- n 1) spare to from))))



