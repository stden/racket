#lang racket

(require profile
         racket/future
         racket/flonum)

(printf "=== PROFILING RACKET THREAD TESTS ===\n\n")

;; ==========================================
;; Test Functions to Profile
;; ==========================================

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (mandelbrot-flonum size)
  (define max-iter 100)
  (define count 0)
  (for* ([y (in-range size)]
         [x (in-range size)])
    (define cx (fl- (fl/ (fl* 3.5 (->fl x)) (->fl size)) 2.5))
    (define cy (fl- (fl/ (fl* 2.0 (->fl y)) (->fl size)) 1.0))
    (define iter
      (let loop ([zx 0.0] [zy 0.0] [i 0])
        (if (or (>= i max-iter) (fl> (fl+ (fl* zx zx) (fl* zy zy)) 4.0))
            i
            (loop (fl+ (fl- (fl* zx zx) (fl* zy zy)) cx)
                  (fl+ (fl* 2.0 (fl* zx zy)) cy)
                  (add1 i)))))
    (set! count (+ count iter)))
  count)

(define (channel-stress n)
  (define ch (make-channel))
  (thread (lambda ()
            (for ([i (in-range n)])
              (channel-put ch i))))
  (for ([_ (in-range n)])
    (channel-get ch)))

(define (semaphore-stress n)
  (define counter (box 0))
  (define lock (make-semaphore 1))
  (define threads
    (for/list ([_ (in-range 10)])
      (thread
       (lambda ()
         (for ([_ (in-range (quotient n 10))])
           (semaphore-wait lock)
           (set-box! counter (add1 (unbox counter)))
           (semaphore-post lock))))))
  (for-each thread-wait threads)
  (unbox counter))

(define (parallel-compute n)
  (define futures
    (for/list ([_ (in-range 4)])
      (future (lambda ()
                (for/fold ([sum 0.0]) ([i (in-range n)])
                  (fl+ sum (flsin (->fl i))))))))
  (apply + (map touch futures)))

;; ==========================================
;; Profile Each Test
;; ==========================================

(printf "1. PROFILING FIBONACCI(35)\n")
(printf "─────────────────────────────────────\n")
(profile-thunk
 (lambda ()
   (fibonacci 35))
 #:order 'total)
(printf "\n")

(printf "2. PROFILING MANDELBROT (200x200)\n")
(printf "─────────────────────────────────────\n")
(profile-thunk
 (lambda ()
   (mandelbrot-flonum 200))
 #:order 'total)
(printf "\n")

(printf "3. PROFILING CHANNEL STRESS (100K ops)\n")
(printf "─────────────────────────────────────\n")
(profile-thunk
 (lambda ()
   (channel-stress 100000))
 #:order 'total)
(printf "\n")

(printf "4. PROFILING SEMAPHORE STRESS (100K ops)\n")
(printf "─────────────────────────────────────\n")
(profile-thunk
 (lambda ()
   (semaphore-stress 100000))
 #:order 'total)
(printf "\n")

(printf "5. PROFILING PARALLEL COMPUTE (10M ops)\n")
(printf "─────────────────────────────────────\n")
(profile-thunk
 (lambda ()
   (parallel-compute 10000000))
 #:order 'total)
(printf "\n")

(printf "=== PROFILING COMPLETE ===\n")
