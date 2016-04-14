#lang racket

(require racket/future
         racket/flonum)

(printf "=== DETAILED PERFORMANCE PROFILING ===\n")
(printf "Processors: ~a\n\n" (processor-count))

;; ==========================================
;; Timing Macro
;; ==========================================

(define-syntax-rule (time-it name body ...)
  (let ()
    (define start (current-inexact-milliseconds))
    (define result (begin body ...))
    (define end (current-inexact-milliseconds))
    (printf "~a: ~a ms\n" name (~r (- end start) #:precision 2))
    result))

;; ==========================================
;; CPU-bound Tests
;; ==========================================

(printf "=== CPU-BOUND TESTS ===\n")

(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

(time-it "Fibonacci(30)" (fib 30))
(time-it "Fibonacci(35)" (fib 35))
(time-it "Fibonacci(38)" (fib 38))

(printf "\n")

;; ==========================================
;; Flonum vs Generic
;; ==========================================

(printf "=== FLONUM VS GENERIC ===\n")

(define (sum-generic n)
  (for/fold ([s 0.0]) ([i (in-range n)])
    (+ s (/ 1.0 (+ 1.0 i)))))

(define (sum-flonum n)
  (for/fold ([s 0.0]) ([i (in-range n)])
    (fl+ s (fl/ 1.0 (fl+ 1.0 (->fl i))))))

(time-it "Sum generic (10M)" (sum-generic 10000000))
(time-it "Sum flonum (10M)" (sum-flonum 10000000))

(printf "\n")

;; ==========================================
;; Thread Creation Overhead
;; ==========================================

(printf "=== THREAD CREATION OVERHEAD ===\n")

(time-it "Create 1K threads"
  (for ([_ (in-range 1000)])
    (thread (lambda () (void)))))

(time-it "Create+wait 1K threads"
  (let ()
    (define ts (for/list ([_ (in-range 1000)])
                 (thread (lambda () (+ 1 1)))))
    (for-each thread-wait ts)))

(printf "\n")

;; ==========================================
;; Future Creation Overhead  
;; ==========================================

(printf "=== FUTURE OVERHEAD ===\n")

(define (heavy-work n)
  (for/fold ([s 0.0]) ([i (in-range n)])
    (fl+ s (flsin (->fl i)))))

(time-it "Sequential 4x1M" 
  (for ([_ (in-range 4)])
    (heavy-work 1000000)))

(time-it "Parallel futures 4x1M"
  (let ()
    (define fs (for/list ([_ (in-range 4)])
                 (future (lambda () (heavy-work 1000000)))))
    (for-each touch fs)))

(printf "\n")

;; ==========================================
;; Channel Throughput
;; ==========================================

(printf "=== CHANNEL THROUGHPUT ===\n")

(define (channel-test n)
  (define ch (make-channel))
  (thread (lambda () 
            (for ([i (in-range n)])
              (channel-put ch i))))
  (for ([_ (in-range n)])
    (channel-get ch)))

(time-it "Channel 100K msgs" (channel-test 100000))
(time-it "Channel 500K msgs" (channel-test 500000))

(printf "\n")

;; ==========================================
;; Semaphore Contention
;; ==========================================

(printf "=== SEMAPHORE CONTENTION ===\n")

(define (sem-test threads-n ops-per-thread)
  (define counter (box 0))
  (define lock (make-semaphore 1))
  (define ts
    (for/list ([_ (in-range threads-n)])
      (thread (lambda ()
                (for ([_ (in-range ops-per-thread)])
                  (semaphore-wait lock)
                  (set-box! counter (add1 (unbox counter)))
                  (semaphore-post lock))))))
  (for-each thread-wait ts)
  (unbox counter))

(time-it "10 threads x 10K ops" (sem-test 10 10000))
(time-it "50 threads x 10K ops" (sem-test 50 10000))
(time-it "100 threads x 1K ops" (sem-test 100 1000))

(printf "\n")

;; ==========================================
;; Memory Allocation
;; ==========================================

(printf "=== MEMORY ALLOCATION ===\n")

(time-it "Allocate 1M cons cells"
  (for/fold ([lst '()]) ([i (in-range 1000000)])
    (cons i lst)))

(time-it "Allocate 1M vectors"
  (for ([_ (in-range 1000000)])
    (make-vector 10)))

(time-it "Allocate 100K strings"
  (for ([i (in-range 100000)])
    (format "string-~a" i)))

(printf "\n")

;; ==========================================
;; Summary
;; ==========================================

(printf "=== PROFILING SUMMARY ===\n\n")

(define (benchmark name seq-thunk par-thunk)
  (collect-garbage)
  (define start-seq (current-inexact-milliseconds))
  (seq-thunk)
  (define time-seq (- (current-inexact-milliseconds) start-seq))
  
  (collect-garbage)
  (define start-par (current-inexact-milliseconds))
  (par-thunk)
  (define time-par (- (current-inexact-milliseconds) start-par))
  
  (printf "~a:\n" name)
  (printf "  Sequential: ~a ms\n" (~r time-seq #:precision 1))
  (printf "  Parallel:   ~a ms\n" (~r time-par #:precision 1))
  (printf "  Speedup:    ~ax\n\n" (~r (/ time-seq (max 0.1 time-par)) #:precision 2)))

(benchmark "Fibonacci x4"
  (lambda () (for ([_ (in-range 4)]) (fib 35)))
  (lambda () 
    (define fs (for/list ([_ (in-range 4)]) (future (lambda () (fib 35)))))
    (for-each touch fs)))

(benchmark "Flonum sum x4"
  (lambda () (for ([_ (in-range 4)]) (sum-flonum 5000000)))
  (lambda ()
    (define fs (for/list ([_ (in-range 4)]) (future (lambda () (sum-flonum 5000000)))))
    (for-each touch fs)))

(printf "=== PROFILING COMPLETE ===\n")
