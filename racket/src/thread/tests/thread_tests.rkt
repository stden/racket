#lang racket

(require racket/future
         racket/place
         rackunit)

;; ==========================================
;; 1. Тесты базовой функциональности (Green Threads)
;; ==========================================

(define (test-basic-threads)
  (printf "Testing basic threads...\n")
  
  (define counter 0)
  (define lock (make-semaphore 1))
  
  (define threads
    (for/list ([i (in-range 100)])
      (thread
       (lambda ()
         (for ([j (in-range 1000)])
           (semaphore-wait lock)
           (set! counter (add1 counter))
           (semaphore-post lock))))))
  
  (for-each thread-wait threads)
  (check-equal? counter 100000 "Counter should be 100 * 1000")
  (printf "Basic threads test passed: Counter = ~a\n" counter))

;; ==========================================
;; 2. Тесты каналов (Channels)
;; ==========================================

(define (test-channels)
  (printf "Testing channels...\n")
  (define ch (make-channel))
  
  (thread
   (lambda ()
     (channel-put ch "hello")
     (channel-put ch "world")))
  
  (check-equal? (channel-get ch) "hello")
  (check-equal? (channel-get ch) "world")
  (printf "Channels test passed.\n"))

;; ==========================================
;; 3. Тесты Futures (Parallelism)
;; ==========================================

(define (test-futures)
  (printf "Testing futures...\n")
  (if ((processor-count) . > . 1)
      (let ()
        (define f
          (future
           (lambda ()
             (let loop ([n 10000000])
               (if (zero? n)
                   'done
                   (loop (sub1 n)))))))
        
        (check-equal? (touch f) 'done)
        (printf "Futures test passed.\n"))
      (printf "Skipping futures test (single core).\n")))

;; ==========================================
;; 4. Бенчмарки
;; ==========================================

(define (benchmark-threads n-threads n-iterations)
  (printf "Benchmarking ~a threads with ~a iterations...\n" n-threads n-iterations)
  (define start (current-milliseconds))
  
  (define threads
    (for/list ([i (in-range n-threads)])
      (thread (lambda ()
                (let loop ([n n-iterations])
                  (unless (zero? n)
                    (loop (sub1 n))))))))
  
  (for-each thread-wait threads)
  (define end (current-milliseconds))
  (printf "Time: ~a ms\n" (- end start)))

(define (benchmark-futures n-futures n-iterations)
  (printf "Benchmarking ~a futures with ~a iterations...\n" n-futures n-iterations)
  (define start (current-milliseconds))
  
  (define futures
    (for/list ([i (in-range n-futures)])
      (future (lambda ()
                (let loop ([n n-iterations])
                  (if (zero? n)
                      i
                      (loop (sub1 n))))))))
  
  (for-each touch futures)
  (define end (current-milliseconds))
  (printf "Time: ~a ms\n" (- end start)))

;; ==========================================
;; Запуск
;; ==========================================

(module+ main
  (test-basic-threads)
  (test-channels)
  (test-futures)
  
  (newline)
  (printf "=== Benchmarks ===\n")
  (benchmark-threads 1000 100000)
  (benchmark-threads 10 10000000)
  (when ((processor-count) . > . 1)
    (benchmark-futures 4 100000000)))
