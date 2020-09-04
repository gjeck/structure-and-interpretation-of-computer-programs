;; Exercise 2.1
;; Create a rational number type
;; `make-rat` should normalize the gcd and the sign so that if the rational number
;; is positive, both the numerator and denominator are positive, and if the rational
;; number is negative, only the numerator is negative.
(defn gcd [a b]
  "returns the greatest common divisor of two integers"
  (if (= b 0)
    a
    (recur b (rem a b))))

(defn make-rat [n d]
  "create a rational number e.g. 1/2"
  (let [g (math/abs (gcd n d))]
    (cond (and (< n 0) (< d 0)) (list (/ (math/abs n) g) (/ (math/abs d) g))
          (and (> n 0) (< d 0)) (list (/ (- n) g) (/ (math/abs d) g))
          :else (list (/ n g) (/ d g)))))

(defn numer [rat]
  (first rat))

(defn denom [rat]
  (last rat))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mult-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; Lecture 2B: Compound Data
(defn make-vector [x y]
  (list x y))

(defn xcoord [vector]
  (first vector))

(defn ycoord [vector]
  (last vector))

(defn make-seg [p q]
  (list p q))

(defn seg-start [seg]
  (first seg))

(defn seg-end [seg]
  (last seg))

(defn line-midpoint [seg]
  "calculate the midpoint of a line segment in 2d"
  (let [a (seg-start seg)
        b (seg-end seg)]
    (make-vector (/ (reduce + [(xcoord a) (xcoord b)]) 2)
                 (/ (reduce + [(ycoord a) (ycoord b)]) 2))))

(defn line-length [seg]
  "calculate the length of a line-segment in 2d"
  (let [dx (- (xcoord (seg-end seg))
              (xcoord (seg-start seg)))
        dy (- (ycoord (seg-end seg))
              (ycoord (seg-start seg)))]
    (math/sqrt (+ (* dx dx)
                  (* dy dy)))))

(defn my-cons [a b]
  "implementation of lisp primitive for pairs"
  (fn pick [p] (cond (= p 1) a
                     (= p 2) b)))

(defn my-car [pair]
  "lisp primitive for 'first' in pair"
  (pair 1))

(defn my-cdr [pair]
  "lisp primitive for 'last' in pair"
  (pair 2))

(defn my-list [item & rest]
  "lisp primitive for a list"
  (cond (nil? item) nil
        (nil? rest) (my-cons item nil)
        :else (my-cons item (apply my-list rest))))

(defn my-list-ref [items n]
  (if (= n 0)
    (my-car items)
    (my-list-ref (my-cdr items) (- n 1))))

(defn my-list-length [items]
  (if (nil? items)
    0
    (+ 1 (my-list-length (my-cdr items)))))

(defn my-list-length-iter [items]
  (defn iter [a count]
    (if (nil? a)
      count
      (iter (my-cdr a) (+ count 1))))
  (iter items 0))

(defn my-list-append [a b]
  (if (nil? a)
    b
    (my-cons (my-car a) (my-list-append (my-cdr a) b))))

(defn print-my-list [items]
  (defn iter [a]
    (if (nil? a)
      (println ")")
      (do
        (if (nil? (my-cdr a))
          (print (my-car a))
          (print (my-car a) " "))
        (iter (my-cdr a)))))
  (print "(")
  (iter items))

;; Exercise 2.17
;; Define a procedure `my-last-pair` that returns the list that contains only the last element of a given
;; (nonempty) my-list. E.g: `(my-car (my-last-pair (my-list 23 72 149 34))) => 34`
(defn my-last-pair [items]
  (cond (nil? items) nil
        (nil? (my-cdr items)) items
        :else (my-last-pair (my-cdr items))))

;; Exercise 2.18
;; Define a procedure `my-list-reverse` that takes a list as an argument and returns a list of the same
;; elements in reverse order. E.g: `(my-list-reverse (my-list 1 2 3 4)) => (4 3 2 1)`
(defn my-list-reverse [items]
  (cond (nil? items) nil
        (nil? (my-cdr items)) items
        :else (my-list-append (my-list-reverse (my-cdr items))
                              (my-cons (my-car items) nil))))

;; Exercise 2.20
;; Define a procedure `same-parity` that takes one or more integers and returns a list of all the
;; arguments that have the same even-odd parity as the first argument. E.g:
;; (same-parity 1 2 3 4 5 6 7) => (1 3 5 7)
;; (same-parity 2 3 4 5 6 7) => (2 4 6)
(defn same-parity [& args]
  (let [filter (if (even? (first args)) even? odd?)]
    (defn make [x & rest]
      (cond (nil? x) nil
            (nil? rest) (my-cons x nil)
            (filter x) (my-cons x (apply make rest))
            :else (apply make rest)))
    (apply make args)))

(defn my-map [proc items]
  "maps a procedure over a list of items returning a list of results"
  (if (nil? items)
    nil
    (my-cons (proc (my-car items))
             (my-map proc (my-cdr items)))))

;; Exercise 2.23
;; The procedure `for-each` is similar to `map`. It takes as arguments a procedure and a list of elements.
;; However, rather than forming a list of the results, `for-each` just applies the procedure to each
;; of the elements in turn, from left to right. Give an implementation of `for-each`
(defn my-for-each [proc items]
  "applies procedure over a list of items for side-effects"
  (if (nil? items)
    nil
    (do
      (proc (my-car items))
      (my-for-each proc (my-cdr items)))))

(defn count-leaves [x]
  "compact length of list, or the leaves of a tree"
  (cond (nil? x) 0
        (not (fn? x)) 1
        :else (+ (count-leaves (my-car x))
                 (count-leaves (my-cdr x)))))

;; Exercise 2.27
;; Write a `deep-reverse` procedure that takes a list as argument and returns as its value the list
;; with its elements reversed and with all sub-lists deep-reversed as well. E.g:
;; (deep-reverse (my-list 1 2) (my-list 3 4)) => ((4 3) (2 1))
(defn deep-reverse [coll]
  (reverse (map (fn [x] (if (coll? x) (deep-reverse x) x)) coll)))

(defn my-deep-reverse [items]
  (cond (nil? items) nil
        (not (fn? items)) items
        :else (my-map my-deep-reverse (my-list-reverse items))))

(defn my-filter [predicate items]
  (cond (nil? items) nil
        (predicate (my-car items)) (do 
                                     (my-cons (my-car items)
                                              (my-filter predicate (my-cdr items))))
        :else (my-filter predicate (my-cdr items))))

(defn my-accumulate [op initial items]
  (if (nil? items)
    initial
    (op (my-car items)
        (my-accumulate op initial (my-cdr items)))))

(defn accumulate [op initial sequence]
  (if (empty? sequence)
    initial
    (op (first sequence)
        (accumulate op initial (rest sequence)))))

(defn my-enumerate-tree [tree]
  (cond (nil? tree) nil
        (not (fn? tree)) (my-list tree)
        :else (my-list-append (my-enumerate-tree (my-car tree))
                              (my-enumerate-tree (my-cdr tree)))))

;; Exercise 2.33
;; Define the list procedures `map`, `append`, and `length` using `accumulate` in the implementation
(defn map-acc [p items]
  (my-accumulate (fn [next acc] (my-cons (p next) acc)) nil items))

(defn append-acc [seq1 seq2]
  (my-accumulate my-cons seq2 seq1))

(defn length-acc [items]
  (my-accumulate (fn [_ acc] (+ acc 1)) 0 items))

;; Exercise 2.35
;; Redifine `count-leaves` from section 2.2.2 as an accumulation
(defn count-leaves-acc [tree]
  (my-accumulate (fn [next acc] (+ (length-acc next) acc)) 0 (my-map my-enumerate-tree tree)))

;; Exercise 2.36
;; The procedure `accumulate-n` is similar to `accumulate` except that it takes as its third argument
;; a sequence of sequences, which are all assumed to have the same number of elements. It applies the
;; designated accumulation procedure to combine all the elements in sequence. E.g:
;; ((1 2 3) (4 5 6) (7 8 9) (10 11 12)) => (accumulate-n + 0 list) => (22 26 30)
(defn my-accumulate-n [op init seqs]
  (if (nil? (my-car seqs))
    nil
    (my-cons (my-accumulate op init (my-map my-car seqs))
             (my-accumulate-n op init (my-map my-cdr seqs)))))

(defn accumulate-n [op init seqs]
  (if (empty? (first seqs))
    nil
    (cons (accumulate op init (map first seqs))
          (accumulate-n op init (map rest seqs)))))

;; Exercise 2.37
;; Matrix and vector operations:
(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(defn matrix*vector [m v]
  (map (fn [x] (dot-product v x)) m))

(defn my-matrix-transpose [m]
  (my-accumulate-n cons nil m))

(defn matrix-transpose [m]
  (accumulate-n cons [] m))

(defn matrix*matrix [m n]
  (let [cols (matrix-transpose n)]
    (map (fn [row] (matrix*vector cols row)) m)))

;; Exercise 2.38
;; The `accumulate` procedure is also known as `fold-right`, because it combines the first element
;; of the sequence with the result of combining all the elements to the right. There is also a
;; `fold-left`, which is similar to `fold-right`, except that it combines elements working in the
;; opposite direction:
(defn fold-left [op initial sequence]
  (defn iter [result rem]
    (if (empty? rem)
      result
      (iter (op result (first rem))
            (rest rem))))
  (iter initial sequence))

(defn fold-right [op initial sequence]
  (accumulate op initial sequence))

;; Exercise 2.39
;; Define `reverse` in terms of `fold-right` and `fold-left`
(defn fold-right-reverse [sequence]
  (fold-right (fn [x y] (conj y x)) [] sequence))

(defn fold-left-reverse [sequence]
  (fold-left (fn [x y] (conj x y)) '() sequence))

;; Section 2.2.3 Sequences as Conventional Interfaces
(defn flat-map [proc sequence]
  (accumulate concat nil (map proc sequence)))

(defn permutations [s]
  "generates all the permutations of the set s"
  (if (empty? s)
    '(())
    (flat-map (fn [y]
                (map (fn [p] (cons y p))
                     (permutations (remove #{y} s))))
              s)))

;; Exercise 2.40
;; Define a procedure `unique-pairs` that, given an integer `n`, generates the sequence of
;; pairs (i, j)` with 1 <= j < i <= n.
(defn unique-pairs [n]
  (if (<= n 0)
    '(())
    (flat-map (fn [a]
                (map (fn [b] (list a b))
                     (range 1 a)))
              (range 1 (+ n 1)))))

;; Exercise 2.41
;; Write a procedure to find all ordered triples of distinct positive integers `i`, `j`, and `k`
;; less than or equal to a given integer `n` that sum to a given integer `s`.
(defn unique-tuples [size n]
  (if (= size 0)
    '(())
    (flat-map (fn [i]
                (map (fn [t] (cons i t))
                     (unique-tuples (- size 1) (- i 1))))
              (range 1 (+ n 1)))))

(defn ordered-triples-sum [n s]
  (let [triples (unique-tuples 3 n)]
    (filter (fn [triple] (= s (reduce + triple)))
            triples)))

;; Exercise 2.42
;; The "eight-queens-puzzle" asks how to place eight queens on a chessboard so that
;; no queen is in check from any other.
;; Implementation should output a solution set. To check the number of solutions, use
;; `count`. E.g: `(count (queens-puzzle 8))` is 92.
(defn queens-puzzle [board-size]
  "Outputs distinct solutions for the queens-puzzle of board size :n"
  (defn make-position [row col]
    (vector row col))
  (defn position-row [position]
    (first position))
  (defn position-col [position]
    (last position))  
  (def empty-board '[])
  (defn adjoin-position [row col positions]
    (conj positions (make-position row col)))
  (defn safe? [col positions]
    (let [kth-queen (nth positions (- col 1))
          other-queens (filter (fn [q] (not (= col (position-col q))))
                               positions)]
      (defn attacks? [q1 q2]
        (or (= (position-row q1) (position-row q2))
            (= (math/abs (- (position-row q1) (position-row q2)))
               (math/abs (- (position-col q1) (position-col q2))))))
      (defn iter [q board]
        (or (empty? board)
            (and (not (attacks? q (first board)))
                 (iter q (rest board)))))
      (iter kth-queen other-queens)))
  (defn queen-cols [k]
    (if (= k 0)
      (vector empty-board)
      (filter (fn [positions] (safe? k positions))
              (mapcat (fn [rest-of-queens] (map (fn [new-row] (adjoin-position new-row k rest-of-queens))
                                                (range 0 board-size)))
                      (queen-cols (- k 1))))))
  (queen-cols board-size))

;; Example 2.3.3
;; Representing Sets using Binary Search Trees.
(defn make-tree-node
  ([item] (make-tree-node item nil nil))
  ([item left] (make-tree-node item left nil))
  ([item left right]
   (vector item left right)))

(defn tree-node-item [node]
  (first node))

(defn tree-node-left [node]
  (nth node 1))

(defn tree-node-right [node]
  (last node))

(defn tree-contains? [item tree]
  (cond (nil? tree) false
        (= item (tree-node-item tree)) true
        (< item (tree-node-item tree)) (recur item (tree-node-left tree))
        (> item (tree-node-item tree)) (recur item (tree-node-right tree))))

(defn tree-append [item tree]
  (cond (nil? tree) (make-tree-node item)
        (= item (tree-node-item tree)) tree
        (< item (tree-node-item tree)) (make-tree-node (tree-node-item tree)
                                                       (tree-append item (tree-node-left tree))
                                                       (tree-node-right tree))
        (> item (tree-node-item tree)) (make-tree-node (tree-node-item tree)
                                                       (tree-node-left tree)
                                                       (tree-append item (tree-node-right tree)))))

;; Example 2.3.4
;; Huffman Encoding Trees
(defn make-leaf [sym weight]
  (vector :leaf sym weight))

(defn leaf? [obj]
  (= :leaf (first obj)))

(defn leaf-symbol [x]
  (nth x 1))

(defn leaf-weight [x]
  (last x))

(defn code-tree-left [tree]
  (first tree))

(defn code-tree-right [tree]
  (nth tree 1))

(defn code-tree-symbols [tree]
  (cond (nil? tree) #{}
        (leaf? tree) #{(leaf-symbol tree)}
        (and (>= (count tree) 2) (not (nil? (nth tree 2)))) (nth tree 2)
        :else (clojure.set/union (code-tree-symbols (code-tree-left tree)) (code-tree-symbols (code-tree-right tree)))))

(defn code-tree-weight [tree]
  (if (leaf? tree)
    (leaf-weight tree)
    (last tree)))

(defn make-code-tree [left right]
  (vector left
          right
          (clojure.set/union (code-tree-symbols left) (code-tree-symbols right))
          (+ (code-tree-weight left) (code-tree-weight right))))

(defn huff-decode-choose-branch [bit branch]
  (cond (= bit 0) (code-tree-left branch)
        (= bit 1) (code-tree-right branch)
        :else (throw (AssertionError. "bad bit"))))

(defn huff-decode [bits tree]
  (defn decode-1 [bits current-branch]
    (if (nil? bits)
      '[]
      (let [next-branch (huff-decode-choose-branch (first bits) current-branch)]
        (if (leaf? next-branch)
          (cons (leaf-symbol next-branch)
                (decode-1 (next bits) tree))
          (decode-1 (next bits) next-branch)))))
  (decode-1 bits tree))

;; Exercise 2.68
;; Define a `huff-encode` procedure that takes as arguments a message and a tree and produces the list of
;; bits that gives the encoded message.
(defn huff-encode-symbol [sym tree]
  (if (not (leaf? tree))
    (cond (contains? (code-tree-symbols (code-tree-left tree)) sym)
          (concat [0] (huff-encode-symbol sym (code-tree-left tree)))
          (contains? (code-tree-symbols (code-tree-right tree)) sym)
          (concat [1] (huff-encode-symbol sym (code-tree-right tree)))
          :else (throw (AssertionError. "symbol not contained in tree")))))

(defn huff-encode [message tree]
  (if (nil? message)
    '[]
    (concat (huff-encode-symbol (first message) tree)
            (huff-encode (next message) tree))))

;; Exercise 2.69
;; Define a `generate-huffman-tree` procedure that takes as its argument a list of symbol-frequency pairs
;; and generates a Huffman encoding tree according to the Huffman algorithm.
(defn adjoin-leaf-set [x s]
  (cond (or (nil? s) (empty? s)) [x]
        (< (leaf-weight x) (leaf-weight (first s))) (cons x s)
        :else (cons (first s) (adjoin-leaf-set x (next s)))))

(defn make-leaf-set [pairs]
  (if (nil? pairs)
    '[]
    (let [pair (first pairs)]
      (adjoin-leaf-set (make-leaf (first pair) (last pair))
                       (make-leaf-set (next pairs))))))

(defn successive-merge-pairs [leaf-set]
  (if (<= (count leaf-set) 2)
    (make-code-tree (first leaf-set) (last leaf-set))
    (make-code-tree (first leaf-set) (successive-merge-pairs (next leaf-set)))))

(defn generate-huffman-tree [pairs]
  "Generates a Huffman encoding tree initialized with a sequence of pairs. E.g [[:a 4] [:b 2]]"
  (successive-merge-pairs (reverse (make-leaf-set pairs))))

;; Section 2.5.1
;; Data-Directed Programming
;; When dealing with a set of generic operations that are common to a set of
;; different types we are, in effect, dealing with a 2-dimensional table of
;; operations to types.
(defn attach-tag [type-tag contents]
  (cons type-tag contents))

(defn datum-type-tag [datum]
  (first datum))

(defn datum-contents [datum]
  (rest datum))

;; Generic tagged dispatch table for rational numbers
(defn tag [x]
  (attach-tag :rational x))
(def math-package-table
  {:rational
   {:add (fn [x y] (tag (add-rat x y)))
    :sub (fn [x y] (tag (sub-rat x y)))}})

;; Lecture 3B: Symbolic Differentiation and Quotation
(defn atomic? [exp]
  (not (sequential? exp)))

(defn sum? [exp]
  (and (not (atomic? exp))
       (= (first exp) '+)))

(defn constant? [exp var]
  (and (atomic? exp)
       (not (= exp var))))

(defn same-var? [exp var]
  (and (atomic? exp)
       (= exp var)))

(defn product? [exp]
  (and (not (atomic? exp))
       (= (first exp) '*)))

(defn make-sum [a1 a2]
  (cond (and (number? a1)
             (number? a2)) (+ a1 a2)
        (and (number? a1)
             (= a1 0)) a2
        (and (number? a2)
             (= a2 0)) a1
        :else (vector '+ a1 a2)))

(defn make-product [m1 m2]
  (cond (and (number? m1)
             (number? m2)) (* m1 m2)
        (and (number? m1)
             (= m1 0)) 0
        (and (number? m2)
             (= m2 0)) 0
        :else (vector '* m1 m2)))

(defn a1 [exp]
  (nth exp 1))

(defn a2 [exp]
  (nth exp 2))

(defn m1 [exp]
  (nth exp 1))

(defn m2 [exp]
  (nth exp 2))

(defn derivative [exp var]
  "handles a couple basic cases for solving derivatives"
  (cond (constant? exp var) 0
        (same-var? exp var) 1
        (sum? exp) (make-sum (derivative (a1 exp) var)
                             (derivative (a2 exp) var))
        (product? exp) (make-sum (make-product (m1 exp)
                                               (derivative (m2 exp) var))
                                 (make-product (derivative (m1 exp) var)
                                               (m2 exp)))))
