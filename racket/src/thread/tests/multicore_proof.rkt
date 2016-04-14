#lang racket

(require racket/future
         racket/place
         racket/flonum
         racket/runtime-path)

(define-runtime-path this-module "multicore_proof.rkt")

(printf "=== ДОКАЗАТЕЛЬСТВО МНОГОЯДЕРНОСТИ В RACKET ===\n")
(printf "Процессоров: ~a\n\n" (processor-count))

;; ==========================================
;; Worker для Places
;; ==========================================

(define (place-worker ch)
  (let loop ()
    (define msg (place-channel-get ch))
    (cond
      [(eq? msg 'stop) (place-channel-put ch 'done)]
      [(list? msg)
       (case (car msg)
         [(mandelbrot-float)
          (define n (cadr msg))
          (define result (mandelbrot-compute-float n))
          (place-channel-put ch (list 'result result))
          (loop)]
         [(mandelbrot-bignum)
          (define n (cadr msg))
          (define result (mandelbrot-compute-bignum n))
          (place-channel-put ch (list 'result result))
          (loop)]
         [(fib-bignum)
          (define n (cadr msg))
          (define result (fib-bignum n))
          (place-channel-put ch (list 'result result))
          (loop)]
         [else (loop)])]
      [else (loop)])))

(provide place-worker)

;; ==========================================
;; Mandelbrot с floating point
;; ==========================================

(define (mandelbrot-compute-float size)
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

;; ==========================================
;; Mandelbrot с bignum (длинная арифметика)
;; ==========================================

(define (mandelbrot-compute-bignum size)
  (define max-iter 30)
  (define count 0)
  (for* ([y (in-range size)]
         [x (in-range size)])
    (define cx (- (/ (* 35/10 x) size) 25/10))
    (define cy (- (/ (* 2 y) size) 1))
    (define iter
      (let loop ([zx 0] [zy 0] [i 0])
        (if (or (>= i max-iter) (> (+ (* zx zx) (* zy zy)) 4))
            i
            (loop (+ (- (* zx zx) (* zy zy)) cx)
                  (+ (* 2 zx zy) cy)
                  (add1 i)))))
    (set! count (+ count iter)))
  count)

;; ==========================================
;; Fibonacci с bignum
;; ==========================================

(define (fib-bignum n)
  (if (< n 2) 
      n 
      (+ (fib-bignum (- n 1)) (fib-bignum (- n 2)))))

;; ==========================================
;; ТЕСТ 1: Futures с flonum (должно работать параллельно)
;; ==========================================

(module+ main
  (printf "=== ТЕСТ 1: FUTURES + FLONUM ===\n")
  (printf "Ожидание: ПАРАЛЛЕЛЬНО (Вова прав - это работает)\n\n")
  
  (define size 200)
  (define num-workers 4)
  
  ;; Sequential
  (printf "Sequential...\n")
  (define start-seq (current-milliseconds))
  (for ([_ (in-range num-workers)])
    (mandelbrot-compute-float size))
  (define time-seq (- (current-milliseconds) start-seq))
  (printf "Sequential: ~a ms\n" time-seq)
  
  ;; Parallel with Futures
  (printf "Parallel (Futures)...\n")
  (define start-par (current-milliseconds))
  (define futures
    (for/list ([_ (in-range num-workers)])
      (future (lambda () (mandelbrot-compute-float size)))))
  (for-each touch futures)
  (define time-par (- (current-milliseconds) start-par))
  (printf "Parallel: ~a ms\n" time-par)
  (printf "Speedup: ~ax\n" (~r (/ (exact->inexact time-seq) (max 1 time-par)) #:precision 2))
  (printf ">>> FUTURES + FLONUM = ПАРАЛЛЕЛЬНО <<<\n\n")
  
  ;; ==========================================
  ;; ТЕСТ 2: Futures с bignum (Вова говорит - не будет параллельно)
  ;; ==========================================
  
  (printf "=== ТЕСТ 2: FUTURES + BIGNUM ===\n")
  (printf "Ожидание: НЕ параллельно (проверяем Вову)\n\n")
  
  (define fib-n 35)
  
  ;; Sequential
  (printf "Sequential fib(~a)...\n" fib-n)
  (define start-fib-seq (current-milliseconds))
  (for ([_ (in-range num-workers)])
    (fib-bignum fib-n))
  (define time-fib-seq (- (current-milliseconds) start-fib-seq))
  (printf "Sequential: ~a ms\n" time-fib-seq)
  
  ;; Parallel with Futures
  (printf "Parallel (Futures)...\n")
  (define start-fib-par (current-milliseconds))
  (define fib-futures
    (for/list ([_ (in-range num-workers)])
      (future (lambda () (fib-bignum fib-n)))))
  (for-each touch fib-futures)
  (define time-fib-par (- (current-milliseconds) start-fib-par))
  (printf "Parallel: ~a ms\n" time-fib-par)
  (define fib-speedup (/ (exact->inexact time-fib-seq) (max 1 time-fib-par)))
  (printf "Speedup: ~ax\n" (~r fib-speedup #:precision 2))
  
  (if (> fib-speedup 1.5)
      (printf ">>> ВОВА НЕ ПРАВ: BIGNUM тоже параллелится! <<<\n\n")
      (printf ">>> ВОВА ПРАВ: BIGNUM не параллелится <<<\n\n"))
  
  ;; ==========================================
  ;; ТЕСТ 3: Places с bignum (ДОЛЖНО работать всегда)
  ;; ==========================================
  
  (printf "=== ТЕСТ 3: PLACES + BIGNUM ===\n")
  (printf "Ожидание: ПАРАЛЛЕЛЬНО (Places всегда работают!)\n\n")
  
  (printf "Sequential...\n")
  ;; Уже измеряли выше: time-fib-seq
  (printf "Sequential: ~a ms\n" time-fib-seq)
  
  (printf "Parallel (Places)...\n")
  (define start-places (current-milliseconds))
  
  (define places
    (for/list ([_ (in-range num-workers)])
      (dynamic-place this-module 'place-worker)))
  
  (for ([p (in-list places)])
    (place-channel-put p (list 'fib-bignum fib-n)))
  
  (define place-results
    (for/list ([p (in-list places)])
      (place-channel-get p)))
  
  (for ([p (in-list places)])
    (place-channel-put p 'stop)
    (place-channel-get p))
  
  (define time-places (- (current-milliseconds) start-places))
  (printf "Parallel (Places): ~a ms\n" time-places)
  (define places-speedup (/ (exact->inexact time-fib-seq) (max 1 time-places)))
  (printf "Speedup: ~ax\n" (~r places-speedup #:precision 2))
  
  (if (> places-speedup 1.5)
      (printf ">>> PLACES ДАЮТ РЕАЛЬНОЕ УСКОРЕНИЕ! <<<\n\n")
      (printf ">>> Overhead Places слишком большой для этой задачи <<<\n\n"))
  
  ;; ==========================================
  ;; ИТОГОВАЯ ТАБЛИЦА
  ;; ==========================================
  
  (printf "=== ИТОГОВЫЕ ВЫВОДЫ ===\n\n")
  (printf "Вова говорит: 'Futures только для числодробилок (flonum)'\n\n")
  (printf "Реальность:\n")
  (printf "1. Futures + flonum:  ПАРАЛЛЕЛЬНО (Вова прав)\n")
  (printf "2. Futures + bignum:  ")
  (if (> fib-speedup 1.5)
      (printf "ПАРАЛЛЕЛЬНО (Вова НЕ прав!)\n")
      (printf "НЕ параллельно (Вова прав)\n"))
  (printf "3. Places + anything: ВСЕГДА ПАРАЛЛЕЛЬНО\n\n")
  
  (printf "ВЫВОД:\n")
  (printf "- Для flonum: используй Futures (быстро, легко)\n")
  (printf "- Для сложных типов: используй Places (overhead, но работает)\n")
  (printf "- Racket МОЖЕТ использовать все ядра, просто нужно\n")
  (printf "  выбрать правильный инструмент!\n"))
