#lang racket

(require racket/future
         racket/flonum
         racket/fixnum)

(printf "=== MEGA HARDCORE BENCHMARKS ===\n")
(printf "Processors: ~a\n\n" (processor-count))

;; ==========================================
;; 31. Flonum vs Generic Speed Test
;; ==========================================

(define (test-flonum-vs-generic)
  (printf "31. FLONUM VS GENERIC NUMBERS\n")
  
  (define n 10000000)
  
  ;; Generic
  (define start-gen (current-milliseconds))
  (for/fold ([sum 0.0]) ([i (in-range n)])
    (+ sum (sin (* i 0.001))))
  (define time-gen (- (current-milliseconds) start-gen))
  
  ;; Flonum
  (define start-fl (current-milliseconds))
  (for/fold ([sum 0.0]) ([i (in-range n)])
    (fl+ sum (flsin (fl* (->fl i) 0.001))))
  (define time-fl (- (current-milliseconds) start-fl))
  
  (printf "    Generic: ~a ms\n" time-gen)
  (printf "    Flonum: ~a ms\n" time-fl)
  (printf "    Speedup: ~ax\n" (~r (/ (exact->inexact time-gen) (max 1 time-fl)) #:precision 2))
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 32. Fixnum vs Generic Speed Test
;; ==========================================

(define (test-fixnum-vs-generic)
  (printf "32. FIXNUM VS GENERIC INTEGERS\n")
  
  (define n 50000000)
  
  ;; Generic
  (define start-gen (current-milliseconds))
  (for/fold ([sum 0]) ([i (in-range n)])
    (+ sum (modulo i 7)))
  (define time-gen (- (current-milliseconds) start-gen))
  
  ;; Fixnum
  (define start-fx (current-milliseconds))
  (for/fold ([sum 0]) ([i (in-range n)])
    (fx+ sum (fxmodulo i 7)))
  (define time-fx (- (current-milliseconds) start-fx))
  
  (printf "    Generic: ~a ms\n" time-gen)
  (printf "    Fixnum: ~a ms\n" time-fx)
  (printf "    Speedup: ~ax\n" (~r (/ (exact->inexact time-gen) (max 1 time-fx)) #:precision 2))
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 33. Vector vs List Speed Test
;; ==========================================

(define (test-vector-vs-list)
  (printf "33. VECTOR VS LIST ACCESS\n")
  
  (define size 100000)
  (define iterations 100)
  
  (define lst (range size))
  (define vec (list->vector lst))
  
  ;; List access (slow)
  (define start-lst (current-milliseconds))
  (for ([_ (in-range iterations)])
    (for ([i (in-range 1000)])
      (list-ref lst i)))
  (define time-lst (- (current-milliseconds) start-lst))
  
  ;; Vector access (fast)
  (define start-vec (current-milliseconds))
  (for ([_ (in-range iterations)])
    (for ([i (in-range 1000)])
      (vector-ref vec i)))
  (define time-vec (- (current-milliseconds) start-vec))
  
  (printf "    List: ~a ms\n" time-lst)
  (printf "    Vector: ~a ms\n" time-vec)
  (printf "    Speedup: ~ax\n" (~r (/ (exact->inexact time-lst) (max 1 time-vec)) #:precision 2))
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 34. Parallel Sum with Futures
;; ==========================================

(define (test-parallel-sum)
  (printf "34. PARALLEL SUM WITH FUTURES\n")
  
  (define n 100000000)
  (define chunks 4)
  (define chunk-size (quotient n chunks))
  
  (define (sum-range start end)
    (for/fold ([sum 0.0]) ([i (in-range start end)])
      (fl+ sum (fl/ 1.0 (fl+ 1.0 (->fl i))))))
  
  ;; Sequential
  (define start-seq (current-milliseconds))
  (sum-range 0 n)
  (define time-seq (- (current-milliseconds) start-seq))
  
  ;; Parallel
  (define start-par (current-milliseconds))
  (define futures
    (for/list ([c (in-range chunks)])
      (future (lambda ()
                (sum-range (* c chunk-size) (* (add1 c) chunk-size))))))
  (apply + (map touch futures))
  (define time-par (- (current-milliseconds) start-par))
  
  (printf "    Sequential: ~a ms\n" time-seq)
  (printf "    Parallel: ~a ms\n" time-par)
  (printf "    Speedup: ~ax\n" (~r (/ (exact->inexact time-seq) (max 1 time-par)) #:precision 2))
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 35. Monte Carlo Pi Calculation
;; ==========================================

(define (test-monte-carlo-pi)
  (printf "35. MONTE CARLO PI (parallel)\n")
  
  (define samples 10000000)
  (define chunks 4)
  (define chunk-size (quotient samples chunks))
  
  (define (count-inside n seed)
    (random-seed seed)
    (for/fold ([inside 0]) ([_ (in-range n)])
      (define x (random))
      (define y (random))
      (if (fl<= (fl+ (fl* x x) (fl* y y)) 1.0)
          (add1 inside)
          inside)))
  
  ;; Sequential
  (define start-seq (current-milliseconds))
  (define inside-seq (count-inside samples 42))
  (define pi-seq (* 4.0 (/ inside-seq samples)))
  (define time-seq (- (current-milliseconds) start-seq))
  
  ;; Parallel
  (define start-par (current-milliseconds))
  (define futures
    (for/list ([c (in-range chunks)])
      (future (lambda () (count-inside chunk-size (+ 42 c))))))
  (define inside-par (apply + (map touch futures)))
  (define pi-par (* 4.0 (/ inside-par samples)))
  (define time-par (- (current-milliseconds) start-par))
  
  (printf "    Sequential: ~a ms (pi=~a)\n" time-seq (~r pi-seq #:precision 5))
  (printf "    Parallel: ~a ms (pi=~a)\n" time-par (~r pi-par #:precision 5))
  (printf "    Speedup: ~ax\n" (~r (/ (exact->inexact time-seq) (max 1 time-par)) #:precision 2))
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 36. Matrix Multiplication
;; ==========================================

(define (test-matrix-mult)
  (printf "36. MATRIX MULTIPLICATION (parallel)\n")
  
  (define size 200)
  
  (define A (for/vector ([i (in-range size)])
              (for/vector ([j (in-range size)])
                (->fl (+ i j)))))
  
  (define B (for/vector ([i (in-range size)])
              (for/vector ([j (in-range size)])
                (->fl (- i j)))))
  
  (define (mult-row row-idx)
    (for/vector ([j (in-range size)])
      (for/fold ([sum 0.0]) ([k (in-range size)])
        (fl+ sum (fl* (vector-ref (vector-ref A row-idx) k)
                       (vector-ref (vector-ref B k) j))))))
  
  ;; Sequential
  (define start-seq (current-milliseconds))
  (for/vector ([i (in-range size)])
    (mult-row i))
  (define time-seq (- (current-milliseconds) start-seq))
  
  ;; Parallel
  (define start-par (current-milliseconds))
  (define futures
    (for/list ([i (in-range size)])
      (future (lambda () (mult-row i)))))
  (list->vector (map touch futures))
  (define time-par (- (current-milliseconds) start-par))
  
  (printf "    Matrix size: ~ax~a\n" size size)
  (printf "    Sequential: ~a ms\n" time-seq)
  (printf "    Parallel: ~a ms\n" time-par)
  (printf "    Speedup: ~ax\n" (~r (/ (exact->inexact time-seq) (max 1 time-par)) #:precision 2))
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 37. Parallel Quick Sort
;; ==========================================

(define (test-parallel-sort)
  (printf "37. PARALLEL QUICKSORT\n")
  
  (define size 100000)
  (define data (for/list ([_ (in-range size)]) (random 1000000)))
  
  (define (quicksort lst)
    (if (or (null? lst) (null? (cdr lst)))
        lst
        (let* ([pivot (car lst)]
               [rest (cdr lst)]
               [less (filter (lambda (x) (< x pivot)) rest)]
               [greater (filter (lambda (x) (>= x pivot)) rest)])
          (append (quicksort less) (list pivot) (quicksort greater)))))
  
  (define (parallel-quicksort lst depth)
    (if (or (null? lst) (null? (cdr lst)) (< depth 0))
        (quicksort lst)
        (let* ([pivot (car lst)]
               [rest (cdr lst)]
               [less (filter (lambda (x) (< x pivot)) rest)]
               [greater (filter (lambda (x) (>= x pivot)) rest)]
               [f-less (future (lambda () (parallel-quicksort less (sub1 depth))))]
               [f-greater (future (lambda () (parallel-quicksort greater (sub1 depth))))])
          (append (touch f-less) (list pivot) (touch f-greater)))))
  
  ;; Sequential
  (define start-seq (current-milliseconds))
  (quicksort data)
  (define time-seq (- (current-milliseconds) start-seq))
  
  ;; Parallel
  (define start-par (current-milliseconds))
  (parallel-quicksort data 3)
  (define time-par (- (current-milliseconds) start-par))
  
  (printf "    Elements: ~a\n" size)
  (printf "    Sequential: ~a ms\n" time-seq)
  (printf "    Parallel: ~a ms\n" time-par)
  (printf "    Speedup: ~ax\n" (~r (/ (exact->inexact time-seq) (max 1 time-par)) #:precision 2))
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 38. N-body Simulation Step
;; ==========================================

(define (test-nbody)
  (printf "38. N-BODY SIMULATION\n")
  
  (define n 1000)
  (define steps 10)
  
  (define positions (for/vector ([_ (in-range n)])
                      (vector (random) (random) (random))))
  
  (define (compute-force i)
    (define pi (vector-ref positions i))
    (define-values (fx fy fz)
      (for/fold ([fx 0.0] [fy 0.0] [fz 0.0]) ([j (in-range n)])
        (if (= i j)
            (values fx fy fz)
            (let* ([pj (vector-ref positions j)]
                   [dx (fl- (vector-ref pj 0) (vector-ref pi 0))]
                   [dy (fl- (vector-ref pj 1) (vector-ref pi 1))]
                   [dz (fl- (vector-ref pj 2) (vector-ref pi 2))]
                   [r2 (fl+ 0.01 (fl+ (fl* dx dx) (fl+ (fl* dy dy) (fl* dz dz))))]
                   [r (flsqrt r2)]
                   [f (fl/ 1.0 (fl* r2 r))])
              (values (fl+ fx (fl* f dx))
                      (fl+ fy (fl* f dy))
                      (fl+ fz (fl* f dz)))))))
    (vector fx fy fz))
  
  (define (compute-all-forces)
    (for ([i (in-range n)])
      (compute-force i)))
  
  (define start (current-milliseconds))
  (for ([_ (in-range steps)])
    (compute-all-forces))
  (define time (- (current-milliseconds) start))
  
  (printf "    Bodies: ~a, Steps: ~a\n" n steps)
  (printf "    Time: ~a ms\n" time)
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 39. Concurrent Hash Map Simulation
;; ==========================================

(define (test-concurrent-hashmap)
  (printf "39. CONCURRENT HASHMAP SIMULATION\n")
  
  (define ht (make-hash))
  (define lock (make-semaphore 1))
  (define ops 100000)
  (define threads-num 10)
  
  (define start (current-milliseconds))
  
  (define threads
    (for/list ([t (in-range threads-num)])
      (thread
       (lambda ()
         (for ([i (in-range (quotient ops threads-num))])
           (define key (modulo (+ (* t 10000) i) 1000))
           (semaphore-wait lock)
           (hash-set! ht key (add1 (hash-ref ht key 0)))
           (semaphore-post lock))))))
  
  (for-each thread-wait threads)
  (define time (- (current-milliseconds) start))
  
  (printf "    Operations: ~a\n" ops)
  (printf "    Threads: ~a\n" threads-num)
  (printf "    Time: ~a ms\n" time)
  (printf "    Ops/sec: ~a\n" (round (/ (* ops 1000.0) time)))
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 40. Tree Traversal (Parallel)
;; ==========================================

(define (test-tree-traversal)
  (printf "40. PARALLEL TREE TRAVERSAL\n")
  
  (struct node (value left right) #:transparent)
  
  (define (make-tree depth)
    (if (zero? depth)
        (node (random 100) #f #f)
        (node (random 100)
              (make-tree (sub1 depth))
              (make-tree (sub1 depth)))))
  
  (define tree (make-tree 15))
  
  (define (sum-tree t)
    (if (not t)
        0
        (+ (node-value t)
           (sum-tree (node-left t))
           (sum-tree (node-right t)))))
  
  (define (parallel-sum-tree t depth)
    (if (or (not t) (< depth 0))
        (sum-tree t)
        (let ([fl (future (lambda () (parallel-sum-tree (node-left t) (sub1 depth))))]
              [fr (future (lambda () (parallel-sum-tree (node-right t) (sub1 depth))))])
          (+ (node-value t) (touch fl) (touch fr)))))
  
  ;; Sequential
  (define start-seq (current-milliseconds))
  (define result-seq (sum-tree tree))
  (define time-seq (- (current-milliseconds) start-seq))
  
  ;; Parallel
  (define start-par (current-milliseconds))
  (define result-par (parallel-sum-tree tree 3))
  (define time-par (- (current-milliseconds) start-par))
  
  (printf "    Tree depth: 15 (~a nodes)\n" (expt 2 16))
  (printf "    Sequential: ~a ms\n" time-seq)
  (printf "    Parallel: ~a ms\n" time-par)
  (printf "    Speedup: ~ax\n" (~r (/ (exact->inexact time-seq) (max 1 time-par)) #:precision 2))
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; Run all
;; ==========================================

(module+ main
  (define start (current-milliseconds))
  
  (test-flonum-vs-generic)
  (test-fixnum-vs-generic)
  (test-vector-vs-list)
  (test-parallel-sum)
  (test-monte-carlo-pi)
  (test-matrix-mult)
  (test-parallel-sort)
  (test-nbody)
  (test-concurrent-hashmap)
  (test-tree-traversal)
  
  (printf "=== MEGA BENCHMARKS COMPLETE ===\n")
  (printf "Total: ~a ms\n" (- (current-milliseconds) start)))
