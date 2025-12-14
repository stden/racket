#lang racket

(require racket/future)

(printf "=== ULTRA HARDCORE BENCHMARKS ===\n")
(printf "Processors: ~a\n\n" (processor-count))

;; ==========================================
;; 21. Million Operations Test
;; ==========================================

(define (test-million-ops)
  (printf "21. MILLION OPERATIONS TEST\n")
  (define ops 1000000)
  (define ch (make-channel))
  
  (define start (current-milliseconds))
  
  (thread (lambda ()
            (for ([i (in-range ops)])
              (channel-put ch i))))
  
  (for ([_ (in-range ops)])
    (channel-get ch))
  
  (define time (- (current-milliseconds) start))
  (printf "    Ops: ~a, Time: ~a ms\n" ops time)
  (printf "    Rate: ~a ops/sec\n" (round (/ (* ops 1000.0) time)))
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 22. Cascade Wakeup
;; ==========================================

(define (test-cascade-wakeup)
  (printf "22. CASCADE WAKEUP\n")
  (define n 10000)
  (define semas (for/list ([_ (in-range n)]) (make-semaphore 0)))
  
  (define start (current-milliseconds))
  
  (for ([i (in-range (sub1 n))])
    (thread
     (lambda ()
       (semaphore-wait (list-ref semas i))
       (semaphore-post (list-ref semas (add1 i))))))
  
  (semaphore-post (first semas))
  (semaphore-wait (last semas))
  
  (define time (- (current-milliseconds) start))
  (printf "    Chain length: ~a\n" n)
  (printf "    Time: ~a ms\n" time)
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 23. Thundering Herd
;; ==========================================

(define (test-thundering-herd)
  (printf "23. THUNDERING HERD\n")
  (define n 1000)
  (define gate (make-semaphore 0))
  (define done (make-semaphore 0))
  (define counter (box 0))
  (define lock (make-semaphore 1))
  
  (define start (current-milliseconds))
  
  (for ([_ (in-range n)])
    (thread
     (lambda ()
       (semaphore-wait gate)
       (semaphore-wait lock)
       (set-box! counter (add1 (unbox counter)))
       (semaphore-post lock)
       (semaphore-post done))))
  
  (sleep 0.1)
  (for ([_ (in-range n)])
    (semaphore-post gate))
  
  (for ([_ (in-range n)])
    (semaphore-wait done))
  
  (define time (- (current-milliseconds) start))
  (printf "    Threads: ~a\n" n)
  (printf "    Counter: ~a\n" (unbox counter))
  (printf "    Time: ~a ms\n" time)
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 24. Broadcast Pattern
;; ==========================================

(define (test-broadcast)
  (printf "24. BROADCAST PATTERN\n")
  (define subscribers 100)
  (define messages 1000)
  (define channels (for/list ([_ (in-range subscribers)]) (make-channel)))
  (define received (box 0))
  (define lock (make-semaphore 1))
  
  (define start (current-milliseconds))
  
  (for ([ch (in-list channels)])
    (thread
     (lambda ()
       (for ([_ (in-range messages)])
         (channel-get ch)
         (semaphore-wait lock)
         (set-box! received (add1 (unbox received)))
         (semaphore-post lock)))))
  
  (for ([_ (in-range messages)])
    (for ([ch (in-list channels)])
      (channel-put ch 'msg)))
  
  (sleep 0.5)
  
  (define time (- (current-milliseconds) start))
  (printf "    Subscribers: ~a, Messages: ~a\n" subscribers messages)
  (printf "    Total received: ~a\n" (unbox received))
  (printf "    Time: ~a ms\n" time)
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 25. Recursive Spawn
;; ==========================================

(define (test-recursive-spawn)
  (printf "25. RECURSIVE SPAWN\n")
  (define counter (box 0))
  (define lock (make-semaphore 1))
  
  (define (spawn-recursive depth)
    (when (> depth 0)
      (semaphore-wait lock)
      (set-box! counter (add1 (unbox counter)))
      (semaphore-post lock)
      (define t1 (thread (lambda () (spawn-recursive (sub1 depth)))))
      (define t2 (thread (lambda () (spawn-recursive (sub1 depth)))))
      (thread-wait t1)
      (thread-wait t2)))
  
  (define start (current-milliseconds))
  (spawn-recursive 10)
  (define time (- (current-milliseconds) start))
  
  (printf "    Depth: 10 (binary tree)\n")
  (printf "    Threads spawned: ~a\n" (unbox counter))
  (printf "    Time: ~a ms\n" time)
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 26. Hot Path Contention
;; ==========================================

(define (test-hot-path)
  (printf "26. HOT PATH CONTENTION\n")
  (define hot-counter (box 0))
  (define lock (make-semaphore 1))
  (define threads 50)
  (define ops 50000)
  
  (define start (current-milliseconds))
  
  (define ts
    (for/list ([_ (in-range threads)])
      (thread
       (lambda ()
         (for ([_ (in-range ops)])
           (semaphore-wait lock)
           (set-box! hot-counter (add1 (unbox hot-counter)))
           (semaphore-post lock))))))
  
  (for-each thread-wait ts)
  (define time (- (current-milliseconds) start))
  
  (printf "    Threads: ~a, Ops/thread: ~a\n" threads ops)
  (printf "    Total ops: ~a\n" (unbox hot-counter))
  (printf "    Time: ~a ms\n" time)
  (printf "    Rate: ~a ops/sec\n" (round (/ (* (unbox hot-counter) 1000.0) time)))
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 27. Future Chain
;; ==========================================

(define (test-future-chain)
  (printf "27. FUTURE CHAIN\n")
  (define chain-length 100)
  
  (define start (current-milliseconds))
  
  (define (chain n acc)
    (if (zero? n)
        acc
        (touch (future (lambda () (chain (sub1 n) (add1 acc)))))))
  
  (define result (chain chain-length 0))
  (define time (- (current-milliseconds) start))
  
  (printf "    Chain length: ~a\n" chain-length)
  (printf "    Result: ~a\n" result)
  (printf "    Time: ~a ms\n" time)
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 28. Parallel Map
;; ==========================================

(define (test-parallel-map)
  (printf "28. PARALLEL MAP\n")
  
  (define (parallel-map f lst)
    (map touch (map (lambda (x) (future (lambda () (f x)))) lst)))
  
  (define data (range 1 10001))
  (define (heavy-fn x) (* x x x))
  
  (define start-seq (current-milliseconds))
  (define seq-result (map heavy-fn data))
  (define time-seq (- (current-milliseconds) start-seq))
  
  (define start-par (current-milliseconds))
  (define par-result (parallel-map heavy-fn data))
  (define time-par (- (current-milliseconds) start-par))
  
  (printf "    Items: ~a\n" (length data))
  (printf "    Sequential: ~a ms\n" time-seq)
  (printf "    Parallel: ~a ms\n" time-par)
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 29. Semaphore Pool
;; ==========================================

(define (test-semaphore-pool)
  (printf "29. SEMAPHORE POOL (connection pool sim)\n")
  (define pool-size 10)
  (define pool (make-semaphore pool-size))
  (define requests 1000)
  (define completed (box 0))
  (define lock (make-semaphore 1))
  
  (define start (current-milliseconds))
  
  (define threads
    (for/list ([_ (in-range requests)])
      (thread
       (lambda ()
         (semaphore-wait pool)
         (sleep 0.001)
         (semaphore-post pool)
         (semaphore-wait lock)
         (set-box! completed (add1 (unbox completed)))
         (semaphore-post lock)))))
  
  (for-each thread-wait threads)
  (define time (- (current-milliseconds) start))
  
  (printf "    Pool size: ~a\n" pool-size)
  (printf "    Requests: ~a\n" requests)
  (printf "    Completed: ~a\n" (unbox completed))
  (printf "    Time: ~a ms\n" time)
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; 30. Stress All Features
;; ==========================================

(define (test-stress-all)
  (printf "30. STRESS ALL FEATURES\n")
  
  (define results (make-vector 100 #f))
  (define ch (make-channel))
  (define sema (make-semaphore 1))
  (define box-val (box 0))
  
  (define start (current-milliseconds))
  
  (for ([i (in-range 100)])
    (thread
     (lambda ()
       (semaphore-wait sema)
       (set-box! box-val (add1 (unbox box-val)))
       (vector-set! results i (unbox box-val))
       (semaphore-post sema)
       (channel-put ch i))))
  
  (for ([_ (in-range 100)])
    (channel-get ch))
  
  (define time (- (current-milliseconds) start))
  
  (printf "    Combined: threads + channels + semaphores + boxes + vectors\n")
  (printf "    Time: ~a ms\n" time)
  (printf "    Final box value: ~a\n" (unbox box-val))
  (printf "    ✓ Complete\n\n"))

;; ==========================================
;; Run
;; ==========================================

(module+ main
  (define start (current-milliseconds))
  
  (test-million-ops)
  (test-cascade-wakeup)
  (test-thundering-herd)
  (test-broadcast)
  (test-recursive-spawn)
  (test-hot-path)
  (test-future-chain)
  (test-parallel-map)
  (test-semaphore-pool)
  (test-stress-all)
  
  (printf "=== ULTRA BENCHMARKS COMPLETE ===\n")
  (printf "Total: ~a ms\n" (- (current-milliseconds) start)))
