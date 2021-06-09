#lang racket/base

; Exercise 3.1:
; An accumulator is a procedure that is called repeatedly with a single numeric argument and accumulates
; its arguments into a sum. Each time it is called, it returns the currently accumulated sum. Write a
; procedure `make-accumulator` that generates accumulators, each maintaining an independent sum.
(define (make-accumulator val)
  (lambda (inc) (set! val (+ val inc)) val))

; Exercise 3.2:
; In software-testing applications, it is useful to be able to count the number of times a given procedure
; is called during the course of computation. Write a procedure that takes as input a procedure `f` and
; returns a procedure that keeps track of the number of times it has been called. If the input
; is 'call-count` then the returned procedure outputs the number of times called. If given 'reset`
; the counter is reset to 0.
(define (make-monitored f)
  (let ([acc (make-accumulator 0)])
    (lambda (input)
      (cond ((eq? input 'call-count) (acc 0))
            ((eq? input 'reset) (acc (- (acc 0))))
            (else (acc 1)
                  (f input))))))

; Exercise 3.19:
; Write a procedure that examines a list and determines whether it contains a cycle, that is, whether
; a program that tried to find the end of the list by taking successive `cdr`s would go into an
; infinite loop.
(require compatibility/mlist) ; Needed for mutable list/cons-cells
(define (mlist-last x)
  (if (null? (mcdr x))
      x
      (mlist-last (mcdr x))))
(define (make-cycle x)
  (set-mcdr! (mlist-last x) x)
  x)
(define (has-cycle? x)
  (define (safe-cdr l)
    (if (mpair? l)
        (mcdr l)
        '()))
  (define (chase tort hare)
    (cond ((null? hare) #f)
          ((equal? tort hare) #t)
          ((null? (mcdr hare)) #f)
          (else (chase (mcdr tort) (mcdr (mcdr hare))))))
  (and (mpair? x) (chase x (mcdr x))))

; Section 3.3.2:
; Representing Queues
(define (queue-front-ptr q)
  (mcar q))
(define (queue-rear-ptr q)
  (mcdr q))
(define (queue-front-ptr-set! q item)
  (set-mcar! q item))
(define (queue-rear-ptr-set! q item)
  (set-mcdr! q item))
(define (queue-empty? q)
  (null? (queue-front-ptr q)))
(define (make-queue)
  (mcons '() '()))
(define (queue-front q)
  (if (queue-empty? q)
      (error "Queue is empty " q)
      (mcar (queue-front-ptr q))))
(define (queue-insert! q item)
  (let ([new-pair (mcons item '())])
    (cond ((queue-empty? q) (queue-front-ptr-set! q new-pair)
                            (queue-rear-ptr-set! q new-pair)
                            q)
          (else (set-mcdr! (queue-rear-ptr q) new-pair)
                (queue-rear-ptr-set! q new-pair)
                q))))
(define (queue-remove! q)
  (cond ((queue-empty? q) (error "Queue is empty " q))
        (else (queue-front-ptr-set! q (mcdr (queue-front-ptr q)))
              q)))

; Exercise 3.22
; Provide an object-oriented implementation of a queue
(define (make-queue-obj)
  (let ([q (make-queue)])
    (define (insert! item)
      (queue-insert! q item))
    (define (dispatch m)
      (cond ((equal? m 'first) (queue-front q))
            ((equal? m 'empty?) (queue-empty? q))
            ((equal? m 'insert!) insert!)
            ((equal? m 'remove!) (queue-remove! q))
            (else (error "Method not found" m))))
    dispatch))

; Exercise 3.27:
; Memoization (or tabulation) is a technique that enables a procedure to record, in a local table,
; values that have previously been computed. This technique can make a vast difference in the
; performance of a program. A memoized procedure maintains a table in which values of the previous
; calls are stored using as keys the arguments that produced the values.
(require racket/dict)
(define (memoize f)
  (let ([table (make-hash)])
    (lambda (x)
      (let ([previously-computed-result (dict-ref table x #f)])
        (or previously-computed-result
            (let ([result (f x)])
              (dict-set! table x result)
              result))))))

(define memo-fib (memoize (lambda (n)
                            (cond ((= n 0) 0)
                                  ((= n 1) 1)
                                  (else (+ (memo-fib (- n 1))
                                           (memo-fib (- n 2))))))))

; Section 3.3.4:
; A Simulator for Digital Circuits
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown Operation" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (wire-get-signal wire)
  (wire 'get-signal))

(define (wire-set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; Section 3.3.5:
; Propagation of Constraints
(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()))
    (define (set-my-value new-value setter)
      (cond ((not (connector-has-value? me))
             (set! value new-value)
             (set! informant setter)
             (for-each-except setter inform-about-value constraints))
            ((not (= value new-value))
             (error "Contradiction" (list value new-value)))
            (else 'ignored)))

    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor inform-about-no-value constraints))
          'ignored))

    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints (cons new-constraint constraints))
          'ignored)
      (if (connector-has-value? me)
          (inform-about-value new-constraint)
          'ignored)
      'done)

    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation " request))))
    me))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (connector-has-value? connector)
  (connector 'has-value?))

(define (connector-get-value connector)
  (connector 'value))

(define (connector-set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (connector-forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connector-connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (for-each-except exception procedure l)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop l))

(define (connector-constant connector constant)
  (define (me request)
    (error "Unknown request" request))
  (connector-connect connector me)
  (connector-set-value! connector constant me)
  me)

;; Section 3.4.2
;; Mechanisms for controlling concurrency. Mutexes and atomic operations.
(define (test-and-set! cell)
  (sync ;; simulated atomic instruction
   (lambda ()
     (if (mcar cell)
         #t
         (begin (set-mcar! cell #t)
                #f)))))

(define (clear! cell)
  (set-mcar! cell #f))

(define (make-mutex)
  (let ([cell (list #f)])
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)
                 #f))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (make-serializer)
  (let ([mutex (make-mutex)])
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ([val (apply p args)])
          (mutex 'release)
          val))
      serialized-p)))

;; Section 3.5.2
;; Infinite Streams, and the Sieve of Eratosthenes
(require racket/stream)

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define (divisible? x y)
  (= (remainder x y) 0))

(define (sieve s)
  (stream-cons (stream-first s)
               (sieve (stream-filter (lambda (x) (not (divisible? x (stream-first s))))
                                     (stream-rest s)))))

(define primes-stream (sieve (integers-starting-from 2)))

;; Exercise 3.50
;; Complete a definition of `stream-map-all` to allow procedures to take multiple arguments
(define (stream-map-all proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map-all (cons proc (map stream-rest argstreams))))))

;; Exercise 3.54
;; Define a procedure `stream-mult-all` analogous to `stream-map-all`, that produces the element-wise
;; product of its tow input streams. Use this together with the stream of integers to create a stream
;; of factorials
(define ones-stream (stream-cons 1 ones-stream))
(define (stream-add-all s1 s2) (stream-map-all + s1 s2))
(define ints-stream (stream-cons 1 (stream-add-all ones-stream ints-stream)))

(define (stream-mult-all s1 s2) (stream-map-all * s1 s2))
(define factorials-stream (stream-cons 1 (stream-mult-all factorials-stream ints-stream)))

(module+ test
  (require rackunit)
  (require rackunit/text-ui)
  (define chapter-03-tests
    (test-suite
     "Chapter 03 Tests"
     (test-case
         "3.1: Accumulator"
       (let ([acc1 (make-accumulator 100)])
         (check = (acc1 0) 100)
         (begin (check = (acc1 10) 110)
                (check = (acc1 -10) 100))))
     (test-case
         "3.2: Monitored"
       (let ([mon1 (make-monitored sqrt)])
         (check = (mon1 'call-count) 0)
         (begin (check = (mon1 64) 8)
                (check = (mon1 'call-count) 1)
                (check = (mon1 'reset) 0)
                (check = (mon1 9) 3)
                (check = (mon1 16) 4)
                (check = (mon1 'call-count) 2))))
     (test-case
         "3.19: Has Cycle"
       (let ([list-with-cycle (make-cycle (mlist 1 2 3 4))])
         (check-true (has-cycle? list-with-cycle))
         (check = (mcar list-with-cycle) 1)
         (check = (mcar (mcdr (mcdr (mcdr list-with-cycle)))) 4)
         (check = (mcar (mcdr (mcdr (mcdr (mcdr list-with-cycle))))) 1)
         (check-false (has-cycle? (mlist 1 2 3 4)))))
     (test-case
         "3.22: Object Oriented Queue"
       (let ([my-queue (make-queue-obj)])
         (begin
           (check-true (my-queue 'empty?))
           ((my-queue 'insert!) 1)
           (check = (my-queue 'first) 1)
           (check-false (my-queue 'empty?))
           ((my-queue 'insert!) 2)
           (check = (my-queue 'first) 1)
           (my-queue 'remove!)
           (check = (my-queue 'first) 2)
           (my-queue 'remove!)
           (check-true (my-queue 'empty?)))))
     (test-case
         "3.27: Memoization"
       (begin (check = (memo-fib 8) 21)
              (check = (memo-fib 16) 987)))
     (test-case
         "3.5.2: Sieve of Eratosthenes"
      (begin (check = (stream-ref primes-stream 0) 2)
             (check = (stream-ref primes-stream 50) 233)))
     (test-case
         "3.54: Factorial Stream"
       (begin (check equal? (stream->list (stream-take factorials-stream 6))
                     '(1 1 2 6 24 120))))))
  (run-tests chapter-03-tests))
