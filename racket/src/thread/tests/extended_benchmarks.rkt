#lang racket

(require racket/future
         rackunit)

(printf "=== EXTENDED HARDCORE BENCHMARKS ===\n")
(printf "Processor count: ~a\n\n" (processor-count))

;; ==========================================
;; 11. Barrier Synchronization
;; ==========================================

(define (test-barrier)
  (printf "11. BARRIER SYNCHRONIZATION\n")
  (printf "    Goal: All threads wait at barrier before continuing\n")
  
  (define num-threads 100)
  (define num-rounds 100)
  (define barrier-count (box 0))
  (define barrier-lock (make-semaphore 1))
  (define barrier-wait (make-semaphore 0))
  (define results (make-vector num-threads 0))
  
  (define start-time (current-milliseconds))
  
  (define threads
    (for/list ([i (in-range num-threads)])
      (thread
       (lambda ()
         (for ([round (in-range num-rounds)])
           ;; Arrive at barrier
           (semaphore-wait barrier-lock)
           (set-box! barrier-count (add1 (unbox barrier-count)))
           (if (= (unbox barrier-count) num-threads)
               (begin
                 (set-box! barrier-count 0)
                 (semaphore-post barrier-lock)
                 ;; Release all waiting threads
                 (for ([_ (in-range (sub1 num-threads))])
                   (semaphore-post barrier-wait)))
               (begin
                 (semaphore-post barrier-lock)
                 (semaphore-wait barrier-wait)))
           ;; Do some work after barrier
           (vector-set! results i (add1 (vector-ref results i))))))))
  
  (for-each thread-wait threads)
  (define end-time (current-milliseconds))
  
  (define all-equal (for/and ([v (in-vector results)])
                      (= v num-rounds)))
  
  (printf "    Threads: ~a, Rounds: ~a\n" num-threads num-rounds)
  (printf "    Time: ~a ms\n" (- end-time start-time))
  (printf "    All synchronized: ~a\n" all-equal)
  (check-true all-equal)
  (printf "    ✓ Test completed\n\n"))

;; ==========================================
;; 12. Pipeline Pattern
;; ==========================================

(define (test-pipeline)
  (printf "12. PIPELINE PATTERN\n")
  (printf "    Goal: Chain of processing stages\n")
  
  (define num-stages 5)
  (define num-items 10000)
  
  (define channels
    (for/list ([_ (in-range (add1 num-stages))])
      (make-channel)))
  
  (define start-time (current-milliseconds))
  
  ;; Create pipeline stages
  (for ([stage (in-range num-stages)])
    (define in-ch (list-ref channels stage))
    (define out-ch (list-ref channels (add1 stage)))
    (thread
     (lambda ()
       (let loop ()
         (define item (channel-get in-ch))
         (unless (eq? item 'done)
           (channel-put out-ch (* item 2))  ;; Process: double the value
           (loop)))
       (channel-put out-ch 'done))))
  
  ;; Feed items
  (thread
   (lambda ()
     (for ([i (in-range num-items)])
       (channel-put (first channels) i))
     (channel-put (first channels) 'done)))
  
  ;; Collect results
  (define final-ch (last channels))
  (define results
    (let loop ([acc '()])
      (define item (channel-get final-ch))
      (if (eq? item 'done)
          acc
          (loop (cons item acc)))))
  
  (define end-time (current-milliseconds))
  (define expected-multiplier (expt 2 num-stages))
  
  (printf "    Stages: ~a, Items: ~a\n" num-stages num-items)
  (printf "    Time: ~a ms\n" (- end-time start-time))
  (printf "    Items processed: ~a\n" (length results))
  (printf "    ✓ Test completed\n\n"))

;; ==========================================
;; 13. Scatter-Gather Pattern
;; ==========================================

(define (test-scatter-gather)
  (printf "13. SCATTER-GATHER PATTERN\n")
  (printf "    Goal: Scatter work, gather results with timeout\n")
  
  (define num-workers 10)
  (define work-items 1000)
  
  (define result-channel (make-channel))
  
  (define start-time (current-milliseconds))
  
  ;; Scatter work to workers
  (for ([w (in-range num-workers)])
    (thread
     (lambda ()
       (for ([i (in-range (quotient work-items num-workers))])
         (define result (* (+ (* w 1000) i) 2))
         (channel-put result-channel result)))))
  
  ;; Gather results with timeout
  (define gathered
    (let loop ([count 0] [results '()])
      (if (>= count work-items)
          results
          (let ([r (sync/timeout 1 result-channel)])
            (if r
                (loop (add1 count) (cons r results))
                results)))))
  
  (define end-time (current-milliseconds))
  
  (printf "    Workers: ~a, Work items: ~a\n" num-workers work-items)
  (printf "    Gathered: ~a results\n" (length gathered))
  (printf "    Time: ~a ms\n" (- end-time start-time))
  (printf "    ✓ Test completed\n\n"))

;; ==========================================
;; 14. Read-Write Lock Simulation
;; ==========================================

(define (test-rw-lock)
  (printf "14. READ-WRITE LOCK SIMULATION\n")
  (printf "    Goal: Multiple readers OR single writer\n")
  
  (define readers-count (box 0))
  (define readers-lock (make-semaphore 1))
  (define write-lock (make-semaphore 1))
  (define shared-data (box 0))
  
  (define read-ops (box 0))
  (define write-ops (box 0))
  
  (define num-readers 10)
  (define num-writers 3)
  (define ops-per-thread 1000)
  
  (define start-time (current-milliseconds))
  
  ;; Readers
  (define readers
    (for/list ([_ (in-range num-readers)])
      (thread
       (lambda ()
         (for ([_ (in-range ops-per-thread)])
           ;; Acquire read lock
           (semaphore-wait readers-lock)
           (set-box! readers-count (add1 (unbox readers-count)))
           (when (= (unbox readers-count) 1)
             (semaphore-wait write-lock))
           (semaphore-post readers-lock)
           
           ;; Read
           (void (unbox shared-data))
           (set-box! read-ops (add1 (unbox read-ops)))
           
           ;; Release read lock
           (semaphore-wait readers-lock)
           (set-box! readers-count (sub1 (unbox readers-count)))
           (when (= (unbox readers-count) 0)
             (semaphore-post write-lock))
           (semaphore-post readers-lock))))))
  
  ;; Writers
  (define writers
    (for/list ([_ (in-range num-writers)])
      (thread
       (lambda ()
         (for ([_ (in-range ops-per-thread)])
           (semaphore-wait write-lock)
           (set-box! shared-data (add1 (unbox shared-data)))
           (set-box! write-ops (add1 (unbox write-ops)))
           (semaphore-post write-lock))))))
  
  (for-each thread-wait readers)
  (for-each thread-wait writers)
  
  (define end-time (current-milliseconds))
  
  (printf "    Readers: ~a, Writers: ~a\n" num-readers num-writers)
  (printf "    Read ops: ~a, Write ops: ~a\n" (unbox read-ops) (unbox write-ops))
  (printf "    Final value: ~a\n" (unbox shared-data))
  (printf "    Time: ~a ms\n" (- end-time start-time))
  (check-equal? (unbox write-ops) (* num-writers ops-per-thread))
  (printf "    ✓ Test completed\n\n"))

;; ==========================================
;; 15. Event-Driven State Machine
;; ==========================================

(define (test-state-machine)
  (printf "15. EVENT-DRIVEN STATE MACHINE\n")
  (printf "    Goal: Thread-based state machine\n")
  
  (define event-ch (make-channel))
  (define result-ch (make-channel))
  
  (define state-machine
    (thread
     (lambda ()
       (let loop ([state 'idle] [counter 0])
         (define event (sync/timeout 0.5 event-ch))
         (cond
           [(not event)
            (channel-put result-ch (list 'final state counter))]
           [else
            (case state
              [(idle)
               (case event
                 [(start) (loop 'running counter)]
                 [else (loop 'idle counter)])]
              [(running)
               (case event
                 [(tick) (loop 'running (add1 counter))]
                 [(pause) (loop 'paused counter)]
                 [(stop) (loop 'idle 0)]
                 [else (loop 'running counter)])]
              [(paused)
               (case event
                 [(resume) (loop 'running counter)]
                 [(stop) (loop 'idle 0)]
                 [else (loop 'paused counter)])])])))))
  
  (define start-time (current-milliseconds))
  
  ;; Send events
  (channel-put event-ch 'start)
  (for ([_ (in-range 10000)])
    (channel-put event-ch 'tick))
  (channel-put event-ch 'pause)
  (channel-put event-ch 'resume)
  (for ([_ (in-range 5000)])
    (channel-put event-ch 'tick))
  
  ;; Get result
  (define result (channel-get result-ch))
  
  (define end-time (current-milliseconds))
  
  (printf "    Events processed, Final state: ~a\n" result)
  (printf "    Time: ~a ms\n" (- end-time start-time))
  (printf "    ✓ Test completed\n\n"))

;; ==========================================
;; 16. Concurrent Counter Comparison
;; ==========================================

(define (test-counter-methods)
  (printf "16. CONCURRENT COUNTER COMPARISON\n")
  (printf "    Goal: Compare different sync methods\n")
  
  (define num-threads 50)
  (define increments 10000)
  
  ;; Method 1: Semaphore
  (define counter1 (box 0))
  (define lock1 (make-semaphore 1))
  
  (define start1 (current-milliseconds))
  (define threads1
    (for/list ([_ (in-range num-threads)])
      (thread
       (lambda ()
         (for ([_ (in-range increments)])
           (semaphore-wait lock1)
           (set-box! counter1 (add1 (unbox counter1)))
           (semaphore-post lock1))))))
  (for-each thread-wait threads1)
  (define time1 (- (current-milliseconds) start1))
  
  ;; Method 2: Channel-based
  (define counter-ch (make-channel))
  (define result-ch (make-channel))
  
  (define counter-server
    (thread
     (lambda ()
       (let loop ([count 0])
         (define msg (channel-get counter-ch))
         (case msg
           [(inc) (loop (add1 count))]
           [(get) (channel-put result-ch count)])))))
  
  (define start2 (current-milliseconds))
  (define threads2
    (for/list ([_ (in-range num-threads)])
      (thread
       (lambda ()
         (for ([_ (in-range increments)])
           (channel-put counter-ch 'inc))))))
  (for-each thread-wait threads2)
  (channel-put counter-ch 'get)
  (define counter2 (channel-get result-ch))
  (define time2 (- (current-milliseconds) start2))
  
  (printf "    Semaphore method: ~a ms (result: ~a)\n" time1 (unbox counter1))
  (printf "    Channel method: ~a ms (result: ~a)\n" time2 counter2)
  (check-equal? (unbox counter1) (* num-threads increments))
  (check-equal? counter2 (* num-threads increments))
  (printf "    ✓ Test completed\n\n"))

;; ==========================================
;; 17. Timeout and Cancellation
;; ==========================================

(define (test-timeout-cancellation)
  (printf "17. TIMEOUT AND CANCELLATION\n")
  (printf "    Goal: Handle timeouts and cancel operations\n")
  
  (define num-operations 100)
  (define successful (box 0))
  (define timed-out (box 0))
  (define cancelled (box 0))
  
  (define start-time (current-milliseconds))
  
  ;; Create operations with varying delays
  (define threads
    (for/list ([i (in-range num-operations)])
      (define op-ch (make-channel))
      (define worker
        (thread
         (lambda ()
           (sleep (/ (random 50) 1000.0))  ;; 0-50ms
           (channel-put op-ch 'done))))
      
      (thread
       (lambda ()
         (define result (sync/timeout 0.02 op-ch))  ;; 20ms timeout
         (if result
             (set-box! successful (add1 (unbox successful)))
             (begin
               (kill-thread worker)
               (set-box! timed-out (add1 (unbox timed-out)))))))))
  
  (for-each thread-wait threads)
  (define end-time (current-milliseconds))
  
  (printf "    Operations: ~a\n" num-operations)
  (printf "    Successful: ~a\n" (unbox successful))
  (printf "    Timed out: ~a\n" (unbox timed-out))
  (printf "    Time: ~a ms\n" (- end-time start-time))
  (check-equal? (+ (unbox successful) (unbox timed-out)) num-operations)
  (printf "    ✓ Test completed\n\n"))

;; ==========================================
;; 18. Memory Pressure Test
;; ==========================================

(define (test-memory-pressure)
  (printf "18. MEMORY PRESSURE TEST\n")
  (printf "    Goal: Threads under GC pressure\n")
  
  (define num-threads 20)
  (define allocations-per-thread 10000)
  (define completed (box 0))
  (define lock (make-semaphore 1))
  
  (define start-time (current-milliseconds))
  
  (define threads
    (for/list ([_ (in-range num-threads)])
      (thread
       (lambda ()
         (for ([_ (in-range allocations-per-thread)])
           ;; Allocate and immediately discard
           (make-vector 100 0))
         (semaphore-wait lock)
         (set-box! completed (add1 (unbox completed)))
         (semaphore-post lock)))))
  
  (for-each thread-wait threads)
  (define end-time (current-milliseconds))
  
  (define total-allocs (* num-threads allocations-per-thread))
  (printf "    Threads: ~a\n" num-threads)
  (printf "    Total allocations: ~a\n" total-allocs)
  (printf "    Completed: ~a\n" (unbox completed))
  (printf "    Time: ~a ms\n" (- end-time start-time))
  (printf "    Rate: ~a allocs/sec\n" 
          (round (/ (* total-allocs 1000.0) (- end-time start-time))))
  (printf "    ✓ Test completed\n\n"))

;; ==========================================
;; 19. Priority Simulation
;; ==========================================

(define (test-priority-simulation)
  (printf "19. PRIORITY SIMULATION\n")
  (printf "    Goal: Simulate priority scheduling\n")
  
  (define high-priority-work (box 0))
  (define low-priority-work (box 0))
  (define lock (make-semaphore 1))
  
  (define iterations 100000)
  
  (define start-time (current-milliseconds))
  
  ;; High priority (runs more often)
  (define high-threads
    (for/list ([_ (in-range 2)])
      (thread
       (lambda ()
         (for ([_ (in-range iterations)])
           (semaphore-wait lock)
           (set-box! high-priority-work (add1 (unbox high-priority-work)))
           (semaphore-post lock))))))
  
  ;; Low priority (yields more)
  (define low-threads
    (for/list ([_ (in-range 2)])
      (thread
       (lambda ()
         (for ([_ (in-range iterations)])
           (sleep 0)  ;; Yield
           (semaphore-wait lock)
           (set-box! low-priority-work (add1 (unbox low-priority-work)))
           (semaphore-post lock))))))
  
  (for-each thread-wait high-threads)
  (for-each thread-wait low-threads)
  
  (define end-time (current-milliseconds))
  
  (printf "    High priority work: ~a\n" (unbox high-priority-work))
  (printf "    Low priority work: ~a\n" (unbox low-priority-work))
  (printf "    Time: ~a ms\n" (- end-time start-time))
  (printf "    ✓ Test completed\n\n"))

;; ==========================================
;; 20. Futures vs Threads Comparison
;; ==========================================

(define (test-futures-vs-threads)
  (printf "20. FUTURES VS THREADS COMPARISON\n")
  (printf "    Goal: Compare performance for parallel work\n")
  
  (define (cpu-work n)
    (for/fold ([sum 0]) ([i (in-range n)])
      (+ sum (modulo (* i i) 997))))
  
  (define work-size 5000000)
  (define num-workers 4)
  
  ;; Threads
  (define start-threads (current-milliseconds))
  (define results-ch (make-channel))
  (for ([_ (in-range num-workers)])
    (thread (lambda () 
              (channel-put results-ch (cpu-work work-size)))))
  (for ([_ (in-range num-workers)])
    (channel-get results-ch))
  (define time-threads (- (current-milliseconds) start-threads))
  
  ;; Futures
  (define start-futures (current-milliseconds))
  (define futures
    (for/list ([_ (in-range num-workers)])
      (future (lambda () (cpu-work work-size)))))
  (for-each touch futures)
  (define time-futures (- (current-milliseconds) start-futures))
  
  (printf "    Green Threads: ~a ms\n" time-threads)
  (printf "    Futures: ~a ms\n" time-futures)
  (printf "    Futures speedup: ~ax\n" 
          (~r (/ (exact->inexact time-threads) (max 1 time-futures)) #:precision 2))
  (printf "    ✓ Test completed\n\n"))

;; ==========================================
;; Run all extended tests
;; ==========================================

(module+ main
  (define start-time (current-milliseconds))
  
  (test-barrier)
  (test-pipeline)
  (test-scatter-gather)
  (test-rw-lock)
  (test-state-machine)
  (test-counter-methods)
  (test-timeout-cancellation)
  (test-memory-pressure)
  (test-priority-simulation)
  (test-futures-vs-threads)
  
  (define total-time (- (current-milliseconds) start-time))
  (printf "=== EXTENDED BENCHMARKS COMPLETE ===\n")
  (printf "Total time: ~a ms (~a seconds)\n" total-time (/ total-time 1000.0)))
