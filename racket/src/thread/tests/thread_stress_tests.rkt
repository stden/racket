#lang racket

(require racket/future
         racket/place
         rackunit)

(printf "=== ADVANCED MULTITHREADING STRESS TESTS ===\n\n")

;; ==========================================
;; 1. Race Condition Test (Atomic Counter)
;; ==========================================

(define (test-race-condition)
  (printf "1. Testing race conditions with atomic operations...\n")
  (define box-val (box 0))
  (define num-threads 100)
  (define increments-per-thread 10000)
  
  ;; Without synchronization - should have race conditions
  (define threads-unsafe
    (for/list ([_ (in-range num-threads)])
      (thread
       (lambda ()
         (for ([_ (in-range increments-per-thread)])
           (set-box! box-val (add1 (unbox box-val))))))))
  
  (for-each thread-wait threads-unsafe)
  (define unsafe-result (unbox box-val))
  
  ;; With synchronization - should be exact
  (set-box! box-val 0)
  (define lock (make-semaphore 1))
  
  (define threads-safe
    (for/list ([_ (in-range num-threads)])
      (thread
       (lambda ()
         (for ([_ (in-range increments-per-thread)])
           (semaphore-wait lock)
           (set-box! box-val (add1 (unbox box-val)))
           (semaphore-post lock))))))
  
  (for-each thread-wait threads-safe)
  (define safe-result (unbox box-val))
  (define expected (* num-threads increments-per-thread))
  
  (printf "   Unsafe result: ~a (expected ~a, diff: ~a)\n" 
          unsafe-result expected (- expected unsafe-result))
  (printf "   Safe result: ~a (expected ~a)\n" safe-result expected)
  (check-equal? safe-result expected "Safe counter should be exact")
  (printf "   ✓ Test passed\n\n"))

;; ==========================================
;; 2. Producer-Consumer with Bounded Buffer
;; ==========================================

(define (test-producer-consumer)
  (printf "2. Testing producer-consumer pattern...\n")
  (define buffer (make-channel))
  (define results (box '()))
  (define num-items 10000)
  (define num-producers 4)
  (define num-consumers 4)
  (define items-per-producer (quotient num-items num-producers))
  
  (define done-channel (make-channel))
  
  ;; Producers
  (define producers
    (for/list ([p (in-range num-producers)])
      (thread
       (lambda ()
         (for ([i (in-range items-per-producer)])
           (channel-put buffer (list p i)))
         (channel-put done-channel 'producer-done)))))
  
  ;; Consumers
  (define lock (make-semaphore 1))
  (define consumed (box 0))
  
  (define consumers
    (for/list ([c (in-range num-consumers)])
      (thread
       (lambda ()
         (let loop ()
           (define item (sync/timeout 0.1 buffer))
           (when item
             (semaphore-wait lock)
             (set-box! consumed (add1 (unbox consumed)))
             (semaphore-post lock)
             (loop)))))))
  
  ;; Wait for all producers
  (for ([_ (in-range num-producers)])
    (channel-get done-channel))
  
  ;; Give consumers time to finish
  (sleep 0.5)
  (for-each kill-thread consumers)
  
  (printf "   Produced: ~a items, Consumed: ~a items\n" 
          num-items (unbox consumed))
  (check-equal? (unbox consumed) num-items "All items should be consumed")
  (printf "   ✓ Test passed\n\n"))

;; ==========================================
;; 3. Deadlock Avoidance Test
;; ==========================================

(define (test-deadlock-avoidance)
  (printf "3. Testing deadlock avoidance (dining philosophers)...\n")
  (define num-philosophers 5)
  (define forks (for/list ([_ (in-range num-philosophers)])
                  (make-semaphore 1)))
  (define meals-eaten (box 0))
  (define lock (make-semaphore 1))
  (define meals-per-philosopher 100)
  
  (define philosophers
    (for/list ([i (in-range num-philosophers)])
      (thread
       (lambda ()
         (define left-fork (list-ref forks i))
         (define right-fork (list-ref forks (modulo (add1 i) num-philosophers)))
         ;; Ordered lock acquisition to prevent deadlock
         (define first-fork (if (< i (modulo (add1 i) num-philosophers))
                                left-fork right-fork))
         (define second-fork (if (< i (modulo (add1 i) num-philosophers))
                                 right-fork left-fork))
         (for ([_ (in-range meals-per-philosopher)])
           (semaphore-wait first-fork)
           (semaphore-wait second-fork)
           ;; Eating
           (semaphore-wait lock)
           (set-box! meals-eaten (add1 (unbox meals-eaten)))
           (semaphore-post lock)
           ;; Done eating
           (semaphore-post second-fork)
           (semaphore-post first-fork))))))
  
  ;; Use timeout to detect deadlock
  (define completed
    (sync/timeout 10
                  (thread (lambda ()
                            (for-each thread-wait philosophers)
                            'done))))
  
  (check-not-false completed "Should complete without deadlock")
  (check-equal? (unbox meals-eaten) 
                (* num-philosophers meals-per-philosopher)
                "All meals should be eaten")
  (printf "   Total meals eaten: ~a\n" (unbox meals-eaten))
  (printf "   ✓ Test passed (no deadlock)\n\n"))

;; ==========================================
;; 4. Thread Starvation Test
;; ==========================================

(define (test-thread-fairness)
  (printf "4. Testing thread fairness (starvation prevention)...\n")
  (define num-threads 10)
  (define execution-counts (make-vector num-threads 0))
  (define total-iterations 100000)
  (define stop-flag (box #f))
  
  (define threads
    (for/list ([i (in-range num-threads)])
      (thread
       (lambda ()
         (let loop ()
           (unless (unbox stop-flag)
             (vector-set! execution-counts i 
                          (add1 (vector-ref execution-counts i)))
             (sleep 0)
             (loop)))))))
  
  ;; Let threads run
  (sleep 1)
  (set-box! stop-flag #t)
  (for-each thread-wait threads)
  
  (define counts (vector->list execution-counts))
  (define min-count (apply min counts))
  (define max-count (apply max counts))
  (define ratio (/ (exact->inexact max-count) (max 1 min-count)))
  
  (printf "   Min executions: ~a, Max executions: ~a\n" min-count max-count)
  (printf "   Fairness ratio (max/min): ~a\n" ratio)
  ;; Ratio should be somewhat reasonable (not perfect but not terrible)
  (check-true (< ratio 10) "Threads should be reasonably fair")
  (printf "   ✓ Test passed\n\n"))

;; ==========================================
;; 5. Thread Interruption and Kill
;; ==========================================

(define (test-thread-interruption)
  (printf "5. Testing thread interruption and cleanup...\n")
  (define cleanup-done (box #f))
  (define work-done (box 0))
  
  (define worker
    (thread
     (lambda ()
       (with-handlers ([exn:break? 
                        (lambda (e)
                          (set-box! cleanup-done #t)
                          (raise e))])
         (let loop ()
           (set-box! work-done (add1 (unbox work-done)))
           (sleep 0.001)
           (loop))))))
  
  (sleep 0.1)
  (break-thread worker)
  (sleep 0.1)
  
  (printf "   Work done before break: ~a\n" (unbox work-done))
  (printf "   Cleanup executed: ~a\n" (unbox cleanup-done))
  (check-true (unbox cleanup-done) "Cleanup should have run")
  (check-true (> (unbox work-done) 0) "Some work should have been done")
  (printf "   ✓ Test passed\n\n"))

;; ==========================================
;; 6. High Contention Lock Test
;; ==========================================

(define (test-high-contention)
  (printf "6. Testing high contention scenario...\n")
  (define shared-resource (box 0))
  (define lock (make-semaphore 1))
  (define num-threads 50)
  (define operations-per-thread 1000)
  
  (define start-time (current-milliseconds))
  
  (define threads
    (for/list ([_ (in-range num-threads)])
      (thread
       (lambda ()
         (for ([_ (in-range operations-per-thread)])
           (semaphore-wait lock)
           ;; Critical section with some work
           (set-box! shared-resource 
                     (+ (unbox shared-resource) 
                        (random 100)))
           (set-box! shared-resource 
                     (- (unbox shared-resource) 
                        (random 100)))
           (semaphore-post lock))))))
  
  (for-each thread-wait threads)
  (define end-time (current-milliseconds))
  
  (printf "   Operations: ~a, Time: ~a ms\n" 
          (* num-threads operations-per-thread)
          (- end-time start-time))
  (printf "   ✓ Test passed\n\n"))

;; ==========================================
;; 7. Futures with CPU-bound work
;; ==========================================

(define (test-futures-parallel)
  (printf "7. Testing futures parallelism...\n")
  
  (define (cpu-work n)
    (let loop ([i n] [sum 0])
      (if (zero? i)
          sum
          (loop (sub1 i) (+ sum (modulo i 7))))))
  
  (define work-size 10000000)
  (define num-futures 4)
  
  ;; Sequential
  (define start-seq (current-milliseconds))
  (for ([_ (in-range num-futures)])
    (cpu-work work-size))
  (define time-seq (- (current-milliseconds) start-seq))
  
  ;; Parallel with futures
  (define start-par (current-milliseconds))
  (define futures
    (for/list ([_ (in-range num-futures)])
      (future (lambda () (cpu-work work-size)))))
  (for-each touch futures)
  (define time-par (- (current-milliseconds) start-par))
  
  (define speedup (/ (exact->inexact time-seq) (max 1 time-par)))
  
  (printf "   Sequential time: ~a ms\n" time-seq)
  (printf "   Parallel time: ~a ms\n" time-par)
  (printf "   Speedup: ~ax\n" speedup)
  (when ((processor-count) . > . 1)
    (check-true (> speedup 1.2) "Should have some speedup on multicore"))
  (printf "   ✓ Test passed\n\n"))

;; ==========================================
;; 8. Complex Event Synchronization
;; ==========================================

(define (test-complex-sync)
  (printf "8. Testing complex event synchronization...\n")
  (define ch1 (make-channel))
  (define ch2 (make-channel))
  (define ch3 (make-channel))
  (define results (box '()))
  (define lock (make-semaphore 1))
  
  ;; Thread 1: produces to ch1
  (thread
   (lambda ()
     (for ([i (in-range 100)])
       (channel-put ch1 (list 'a i)))))
  
  ;; Thread 2: produces to ch2
  (thread
   (lambda ()
     (for ([i (in-range 100)])
       (channel-put ch2 (list 'b i)))))
  
  ;; Thread 3: multiplexes ch1 and ch2, produces to ch3
  (thread
   (lambda ()
     (for ([_ (in-range 200)])
       (define item (sync ch1 ch2))
       (channel-put ch3 item))))
  
  ;; Consumer: collects from ch3
  (thread
   (lambda ()
     (for ([_ (in-range 200)])
       (define item (channel-get ch3))
       (semaphore-wait lock)
       (set-box! results (cons item (unbox results)))
       (semaphore-post lock))))
  
  (sleep 1)
  
  (define result-list (unbox results))
  (define a-count (count (lambda (x) (eq? (car x) 'a)) result-list))
  (define b-count (count (lambda (x) (eq? (car x) 'b)) result-list))
  
  (printf "   Received: ~a total (~a from A, ~a from B)\n" 
          (length result-list) a-count b-count)
  (check-equal? (length result-list) 200 "Should receive all items")
  (printf "   ✓ Test passed\n\n"))

;; ==========================================
;; 9. Thread-local Storage Isolation
;; ==========================================

(define (test-thread-local)
  (printf "9. Testing thread-local storage isolation...\n")
  (define tls (make-thread-cell 0 #t))
  (define results (make-vector 10 #f))
  
  (define threads
    (for/list ([i (in-range 10)])
      (thread
       (lambda ()
         (thread-cell-set! tls i)
         (sleep (/ (random 100) 1000.0))
         (vector-set! results i (thread-cell-ref tls))))))
  
  (for-each thread-wait threads)
  
  (define all-correct
    (for/and ([i (in-range 10)])
      (equal? (vector-ref results i) i)))
  
  (printf "   Results: ~a\n" (vector->list results))
  (check-true all-correct "Each thread should see its own value")
  (printf "   ✓ Test passed\n\n"))

;; ==========================================
;; 10. Nested Thread Creation Stress
;; ==========================================

(define (test-nested-threads)
  (printf "10. Testing nested thread creation...\n")
  (define depth 4)
  (define branching 3)
  (define counter (box 0))
  (define lock (make-semaphore 1))
  
  (define (spawn-tree d)
    (if (zero? d)
        (begin
          (semaphore-wait lock)
          (set-box! counter (add1 (unbox counter)))
          (semaphore-post lock))
        (let ([children (for/list ([_ (in-range branching)])
                          (thread (lambda () (spawn-tree (sub1 d)))))])
          (for-each thread-wait children)
          (semaphore-wait lock)
          (set-box! counter (add1 (unbox counter)))
          (semaphore-post lock))))
  
  (define start-time (current-milliseconds))
  (spawn-tree depth)
  (define end-time (current-milliseconds))
  
  ;; Expected: sum of geometric series
  (define expected (quotient (- (expt branching (add1 depth)) 1) 
                             (- branching 1)))
  
  (printf "   Created ~a threads in ~a ms\n" 
          (unbox counter) (- end-time start-time))
  (check-equal? (unbox counter) expected "Should create expected number of threads")
  (printf "   ✓ Test passed\n\n"))

;; ==========================================
;; Run all tests
;; ==========================================

(module+ main
  (test-race-condition)
  (test-producer-consumer)
  (test-deadlock-avoidance)
  (test-thread-fairness)
  (test-thread-interruption)
  (test-high-contention)
  (when ((processor-count) . > . 1)
    (test-futures-parallel))
  (test-complex-sync)
  (test-thread-local)
  (test-nested-threads)
  
  (printf "=== ALL STRESS TESTS COMPLETED ===\n"))
