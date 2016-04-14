#lang racket

(require racket/future
         rackunit)

(printf "=== HARDCORE MULTITHREADING BENCHMARKS (Go Comparison) ===\n")
(printf "Processor count: ~a\n\n" (processor-count))

;; ==========================================
;; 1. Million Threads Spawn Test (Go: 1M goroutines)
;; ==========================================

(define (test-massive-thread-spawn)
  (printf "1. MASSIVE THREAD SPAWN TEST\n")
  (printf "   Goal: Spawn as many threads as possible quickly\n")
  
  (define num-threads 100000)  ;; Start with 100k, can increase
  (define counter (box 0))
  (define done-sema (make-semaphore 0))
  
  (define start-time (current-milliseconds))
  
  (for ([_ (in-range num-threads)])
    (thread
     (lambda ()
       (semaphore-post done-sema))))
  
  ;; Wait for all threads to signal completion
  (for ([_ (in-range num-threads)])
    (semaphore-wait done-sema))
  
  (define end-time (current-milliseconds))
  (define elapsed (- end-time start-time))
  (define threads-per-sec (/ (* num-threads 1000.0) (max 1 elapsed)))
  
  (printf "   Spawned: ~a threads\n" num-threads)
  (printf "   Time: ~a ms\n" elapsed)
  (printf "   Rate: ~a threads/sec\n" (round threads-per-sec))
  (printf "   ✓ Test completed\n\n"))

;; ==========================================
;; 2. Channel Throughput (Go: channel benchmark)
;; ==========================================

(define (test-channel-throughput)
  (printf "2. CHANNEL THROUGHPUT TEST\n")
  (printf "   Goal: Maximum messages per second through channel\n")
  
  (define ch (make-channel))
  (define num-messages 1000000)
  
  (define start-time (current-milliseconds))
  
  ;; Producer
  (define producer
    (thread
     (lambda ()
       (for ([i (in-range num-messages)])
         (channel-put ch i)))))
  
  ;; Consumer
  (define consumer
    (thread
     (lambda ()
       (for ([_ (in-range num-messages)])
         (channel-get ch)))))
  
  (thread-wait producer)
  (thread-wait consumer)
  
  (define end-time (current-milliseconds))
  (define elapsed (- end-time start-time))
  (define msgs-per-sec (/ (* num-messages 1000.0) (max 1 elapsed)))
  
  (printf "   Messages: ~a\n" num-messages)
  (printf "   Time: ~a ms\n" elapsed)
  (printf "   Throughput: ~a msgs/sec\n" (round msgs-per-sec))
  (printf "   ✓ Test completed\n\n"))

;; ==========================================
;; 3. Context Switch Benchmark
;; ==========================================

(define (test-context-switch)
  (printf "3. CONTEXT SWITCH BENCHMARK\n")
  (printf "   Goal: Measure context switch overhead\n")
  
  (define num-switches 100000)
  (define ch1 (make-channel))
  (define ch2 (make-channel))
  
  (define start-time (current-milliseconds))
  
  ;; Ping thread
  (define ping
    (thread
     (lambda ()
       (for ([i (in-range num-switches)])
         (channel-put ch1 i)
         (channel-get ch2)))))
  
  ;; Pong thread
  (define pong
    (thread
     (lambda ()
       (for ([_ (in-range num-switches)])
         (define v (channel-get ch1))
         (channel-put ch2 v)))))
  
  (thread-wait ping)
  (thread-wait pong)
  
  (define end-time (current-milliseconds))
  (define elapsed (- end-time start-time))
  (define switches-per-sec (/ (* num-switches 2 1000.0) (max 1 elapsed)))
  (define ns-per-switch (/ (* elapsed 1000000.0) (* num-switches 2)))
  
  (printf "   Switches: ~a (round-trips)\n" num-switches)
  (printf "   Time: ~a ms\n" elapsed)
  (printf "   Rate: ~a switches/sec\n" (round switches-per-sec))
  (printf "   Latency: ~a ns/switch\n" (round ns-per-switch))
  (printf "   ✓ Test completed\n\n"))

;; ==========================================
;; 4. Ring Benchmark (Classic concurrency test)
;; ==========================================

(define (test-ring-benchmark)
  (printf "4. RING BENCHMARK\n")
  (printf "   Goal: Pass token around ring of threads\n")
  
  (define ring-size 1000)
  (define num-passes 10000)
  
  (define channels
    (for/vector ([_ (in-range ring-size)])
      (make-channel)))
  
  (define start-time (current-milliseconds))
  
  ;; Create ring of threads
  (for ([i (in-range ring-size)])
    (define my-ch (vector-ref channels i))
    (define next-ch (vector-ref channels (modulo (add1 i) ring-size)))
    (thread
     (lambda ()
       (let loop ()
         (define token (channel-get my-ch))
         (when (> token 0)
           (channel-put next-ch (sub1 token))
           (loop))))))
  
  ;; Start the token
  (channel-put (vector-ref channels 0) num-passes)
  
  ;; Wait for completion (token reaches 0)
  ;; The last thread to receive 0 will not forward
  (sleep 2)  ;; Give time for completion
  
  (define end-time (current-milliseconds))
  (define elapsed (- end-time start-time))
  (define hops-per-sec (/ (* num-passes ring-size 1000.0) (max 1 elapsed)))
  
  (printf "   Ring size: ~a threads\n" ring-size)
  (printf "   Token passes: ~a\n" num-passes)
  (printf "   Total hops: ~a\n" (* num-passes ring-size))
  (printf "   Time: ~a ms\n" elapsed)
  (printf "   Rate: ~a hops/sec\n" (round hops-per-sec))
  (printf "   ✓ Test completed\n\n"))

;; ==========================================
;; 5. Fan-Out/Fan-In Pattern
;; ==========================================

(define (test-fan-out-fan-in)
  (printf "5. FAN-OUT/FAN-IN PATTERN\n")
  (printf "   Goal: Distribute work and collect results\n")
  
  (define num-workers 100)
  (define jobs-per-worker 10000)
  (define total-jobs (* num-workers jobs-per-worker))
  
  (define job-channel (make-channel))
  (define result-channel (make-channel))
  
  (define start-time (current-milliseconds))
  
  ;; Workers (fan-out)
  (for ([w (in-range num-workers)])
    (thread
     (lambda ()
       (let loop ()
         (define job (sync/timeout 0.01 job-channel))
         (when job
           ;; Do some work
           (define result (+ job job))
           (channel-put result-channel result)
           (loop))))))
  
  ;; Job producer
  (thread
   (lambda ()
     (for ([j (in-range total-jobs)])
       (channel-put job-channel j))))
  
  ;; Result collector (fan-in)
  (define results (box 0))
  (for ([_ (in-range total-jobs)])
    (channel-get result-channel)
    (set-box! results (add1 (unbox results))))
  
  (define end-time (current-milliseconds))
  (define elapsed (- end-time start-time))
  (define jobs-per-sec (/ (* total-jobs 1000.0) (max 1 elapsed)))
  
  (printf "   Workers: ~a\n" num-workers)
  (printf "   Total jobs: ~a\n" total-jobs)
  (printf "   Time: ~a ms\n" elapsed)
  (printf "   Throughput: ~a jobs/sec\n" (round jobs-per-sec))
  (printf "   ✓ Test completed\n\n"))

;; ==========================================
;; 6. Parallel Computation (CPU-bound)
;; ==========================================

(define (test-parallel-compute)
  (printf "6. PARALLEL COMPUTATION BENCHMARK\n")
  (printf "   Goal: Compare sequential vs parallel CPU work\n")
  
  (define (fibonacci n)
    (if (< n 2)
        n
        (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
  
  (define work-items 8)
  (define fib-n 30)
  
  ;; Sequential
  (define start-seq (current-milliseconds))
  (for ([_ (in-range work-items)])
    (fibonacci fib-n))
  (define time-seq (- (current-milliseconds) start-seq))
  
  ;; Parallel with futures
  (define start-par (current-milliseconds))
  (define futures
    (for/list ([_ (in-range work-items)])
      (future (lambda () (fibonacci fib-n)))))
  (for-each touch futures)
  (define time-par (- (current-milliseconds) start-par))
  
  (define speedup (/ (exact->inexact time-seq) (max 1 time-par)))
  (define efficiency (/ speedup (processor-count)))
  
  (printf "   Work items: ~a x fib(~a)\n" work-items fib-n)
  (printf "   Sequential: ~a ms\n" time-seq)
  (printf "   Parallel: ~a ms\n" time-par)
  (printf "   Speedup: ~ax\n" (~r speedup #:precision 2))
  (printf "   Efficiency: ~a%\n" (round (* efficiency 100)))
  (printf "   ✓ Test completed\n\n"))

;; ==========================================
;; 7. Thread Creation/Destruction Churn
;; ==========================================

(define (test-thread-churn)
  (printf "7. THREAD CHURN TEST\n")
  (printf "   Goal: Rapid thread creation and destruction\n")
  
  (define num-iterations 10000)
  (define threads-per-iteration 10)
  
  (define start-time (current-milliseconds))
  
  (for ([_ (in-range num-iterations)])
    (define threads
      (for/list ([_ (in-range threads-per-iteration)])
        (thread (lambda () (+ 1 1)))))
    (for-each thread-wait threads))
  
  (define end-time (current-milliseconds))
  (define elapsed (- end-time start-time))
  (define total-threads (* num-iterations threads-per-iteration))
  (define threads-per-sec (/ (* total-threads 1000.0) (max 1 elapsed)))
  
  (printf "   Total threads created: ~a\n" total-threads)
  (printf "   Time: ~a ms\n" elapsed)
  (printf "   Rate: ~a threads/sec (create+destroy)\n" (round threads-per-sec))
  (printf "   ✓ Test completed\n\n"))

;; ==========================================
;; 8. Semaphore Contention Benchmark
;; ==========================================

(define (test-semaphore-contention)
  (printf "8. SEMAPHORE CONTENTION BENCHMARK\n")
  (printf "   Goal: Maximum lock/unlock operations under contention\n")
  
  (define num-threads 100)
  (define ops-per-thread 10000)
  (define lock (make-semaphore 1))
  (define counter (box 0))
  (define done-sema (make-semaphore 0))
  
  (define start-time (current-milliseconds))
  
  (for ([_ (in-range num-threads)])
    (thread
     (lambda ()
       (for ([_ (in-range ops-per-thread)])
         (semaphore-wait lock)
         (set-box! counter (add1 (unbox counter)))
         (semaphore-post lock))
       (semaphore-post done-sema))))
  
  (for ([_ (in-range num-threads)])
    (semaphore-wait done-sema))
  
  (define end-time (current-milliseconds))
  (define elapsed (- end-time start-time))
  (define total-ops (* num-threads ops-per-thread))
  (define ops-per-sec (/ (* total-ops 1000.0) (max 1 elapsed)))
  
  (printf "   Threads: ~a\n" num-threads)
  (printf "   Total lock/unlock ops: ~a\n" total-ops)
  (printf "   Final counter: ~a\n" (unbox counter))
  (printf "   Time: ~a ms\n" elapsed)
  (printf "   Rate: ~a ops/sec\n" (round ops-per-sec))
  (check-equal? (unbox counter) total-ops "Counter should match operations")
  (printf "   ✓ Test completed\n\n"))

;; ==========================================
;; 9. Select/Sync on Multiple Channels
;; ==========================================

(define (test-multi-channel-select)
  (printf "9. MULTI-CHANNEL SELECT BENCHMARK\n")
  (printf "   Goal: Sync across many channels efficiently\n")
  
  (define num-channels 100)
  (define num-messages 10000)
  
  (define channels
    (for/list ([_ (in-range num-channels)])
      (make-channel)))
  
  (define start-time (current-milliseconds))
  
  ;; Producers - each sends to its channel
  (for ([ch (in-list channels)])
    (thread
     (lambda ()
       (for ([i (in-range (quotient num-messages num-channels))])
         (channel-put ch i)))))
  
  ;; Consumer - selects from all channels
  (define received (box 0))
  (for ([_ (in-range num-messages)])
    (apply sync channels)
    (set-box! received (add1 (unbox received))))
  
  (define end-time (current-milliseconds))
  (define elapsed (- end-time start-time))
  (define selects-per-sec (/ (* num-messages 1000.0) (max 1 elapsed)))
  
  (printf "   Channels: ~a\n" num-channels)
  (printf "   Messages: ~a\n" num-messages)
  (printf "   Time: ~a ms\n" elapsed)
  (printf "   Rate: ~a selects/sec\n" (round selects-per-sec))
  (printf "   ✓ Test completed\n\n"))

;; ==========================================
;; 10. Worker Pool Benchmark
;; ==========================================

(define (test-worker-pool)
  (printf "10. WORKER POOL BENCHMARK\n")
  (printf "    Goal: Fixed pool processing many tasks\n")
  
  (define num-workers 8)
  (define num-tasks 100000)
  (define task-queue (make-channel))
  (define result-queue (make-channel))
  
  (define start-time (current-milliseconds))
  
  ;; Start workers
  (for ([_ (in-range num-workers)])
    (thread
     (lambda ()
       (let loop ()
         (define task (sync/timeout 0.1 task-queue))
         (when task
           ;; Process task
           (channel-put result-queue (* task task))
           (loop))))))
  
  ;; Submit tasks
  (thread
   (lambda ()
     (for ([i (in-range num-tasks)])
       (channel-put task-queue i))))
  
  ;; Collect results
  (for ([_ (in-range num-tasks)])
    (channel-get result-queue))
  
  (define end-time (current-milliseconds))
  (define elapsed (- end-time start-time))
  (define tasks-per-sec (/ (* num-tasks 1000.0) (max 1 elapsed)))
  
  (printf "    Workers: ~a\n" num-workers)
  (printf "    Tasks: ~a\n" num-tasks)
  (printf "    Time: ~a ms\n" elapsed)
  (printf "    Throughput: ~a tasks/sec\n" (round tasks-per-sec))
  (printf "    ✓ Test completed\n\n"))

;; ==========================================
;; Summary
;; ==========================================

(define (print-summary start-time)
  (define total-time (- (current-milliseconds) start-time))
  (printf "=== BENCHMARK SUMMARY ===\n")
  (printf "Total time: ~a ms (~a seconds)\n" total-time (/ total-time 1000.0))
  (printf "\nNOTE: These tests stress-test Racket's green threads.\n")
  (printf "For true parallelism comparison with Go, use futures/places.\n")
  (printf "Racket threads are cooperative (like goroutines) but single-OS-thread by default.\n"))

;; ==========================================
;; Run all benchmarks
;; ==========================================

(module+ main
  (define start-time (current-milliseconds))
  
  (test-massive-thread-spawn)
  (test-channel-throughput)
  (test-context-switch)
  (test-ring-benchmark)
  (test-fan-out-fan-in)
  (test-parallel-compute)
  (test-thread-churn)
  (test-semaphore-contention)
  (test-multi-channel-select)
  (test-worker-pool)
  
  (print-summary start-time))
