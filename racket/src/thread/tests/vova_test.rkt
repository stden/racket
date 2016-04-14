#lang racket

(require racket/future
         racket/flonum)

(printf "=== ПРОВЕРКА УТВЕРЖДЕНИЯ ВОВЫ ===\n")
(printf "Вова: 'Mandelbrot с flonum параллелится, с bignum - нет'\n")
(printf "Процессоров: ~a\n\n" (processor-count))

;; ==========================================
;; Mandelbrot с flonum (double precision)
;; ==========================================

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

;; ==========================================
;; Mandelbrot с рациональными числами (длинная арифметика)
;; ==========================================

(define (mandelbrot-rational size)
  (define max-iter 100)
  (define count 0)
  (for* ([y (in-range size)]
         [x (in-range size)])
    ;; Используем рациональные числа вместо flonum
    (define cx (- (* 35/10 (/ x size)) 25/10))
    (define cy (- (* 2 (/ y size)) 1))
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
;; Mandelbrot с очень большими числами (explicit bignums)
;; ==========================================

(define (mandelbrot-bignum size)
  (define max-iter 50)
  (define scale (expt 10 20))  ;; Очень большой масштаб
  (define count 0)
  (for* ([y (in-range size)]
         [x (in-range size)])
    (define cx (- (quotient (* 35 x scale) (* size 10)) (quotient (* 25 scale) 10)))
    (define cy (- (quotient (* 2 y scale) size) scale))
    (define bound (* 4 scale scale))
    (define iter
      (let loop ([zx 0] [zy 0] [i 0])
        (if (or (>= i max-iter) (> (+ (* zx zx) (* zy zy)) bound))
            i
            (let ([zx2 (quotient (* zx zx) scale)]
                  [zy2 (quotient (* zy zy) scale)]
                  [zxy (quotient (* 2 zx zy) scale)])
              (loop (+ (- zx2 zy2) cx)
                    (+ zxy cy)
                    (add1 i))))))
    (set! count (+ count iter)))
  count)

;; ==========================================
;; ТЕСТ 1: Mandelbrot FLONUM
;; ==========================================

(printf "=== ТЕСТ 1: MANDELBROT FLONUM ===\n")
(define size 150)
(define num-workers 4)

(printf "Sequential...\n")
(define start-fl-seq (current-milliseconds))
(for ([_ (in-range num-workers)])
  (mandelbrot-flonum size))
(define time-fl-seq (- (current-milliseconds) start-fl-seq))
(printf "Sequential: ~a ms\n" time-fl-seq)

(printf "Parallel (Futures)...\n")
(define start-fl-par (current-milliseconds))
(define futures-fl
  (for/list ([_ (in-range num-workers)])
    (future (lambda () (mandelbrot-flonum size)))))
(for-each touch futures-fl)
(define time-fl-par (- (current-milliseconds) start-fl-par))
(printf "Parallel: ~a ms\n" time-fl-par)
(define speedup-fl (/ (exact->inexact time-fl-seq) (max 1 time-fl-par)))
(printf "Speedup: ~ax\n\n" (~r speedup-fl #:precision 2))

;; ==========================================
;; ТЕСТ 2: Mandelbrot RATIONAL (длинная арифметика)
;; ==========================================

(printf "=== ТЕСТ 2: MANDELBROT RATIONAL ===\n")
(define size-rat 50)  ;; Меньше из-за медленности

(printf "Sequential...\n")
(define start-rat-seq (current-milliseconds))
(for ([_ (in-range num-workers)])
  (mandelbrot-rational size-rat))
(define time-rat-seq (- (current-milliseconds) start-rat-seq))
(printf "Sequential: ~a ms\n" time-rat-seq)

(printf "Parallel (Futures)...\n")
(define start-rat-par (current-milliseconds))
(define futures-rat
  (for/list ([_ (in-range num-workers)])
    (future (lambda () (mandelbrot-rational size-rat)))))
(for-each touch futures-rat)
(define time-rat-par (- (current-milliseconds) start-rat-par))
(printf "Parallel: ~a ms\n" time-rat-par)
(define speedup-rat (/ (exact->inexact time-rat-seq) (max 1 time-rat-par)))
(printf "Speedup: ~ax\n\n" (~r speedup-rat #:precision 2))

;; ==========================================
;; ТЕСТ 3: Mandelbrot BIGNUM (очень длинная арифметика)
;; ==========================================

(printf "=== ТЕСТ 3: MANDELBROT BIGNUM (scale=10^20) ===\n")
(define size-big 30)  ;; Ещё меньше

(printf "Sequential...\n")
(define start-big-seq (current-milliseconds))
(for ([_ (in-range num-workers)])
  (mandelbrot-bignum size-big))
(define time-big-seq (- (current-milliseconds) start-big-seq))
(printf "Sequential: ~a ms\n" time-big-seq)

(printf "Parallel (Futures)...\n")
(define start-big-par (current-milliseconds))
(define futures-big
  (for/list ([_ (in-range num-workers)])
    (future (lambda () (mandelbrot-bignum size-big)))))
(for-each touch futures-big)
(define time-big-par (- (current-milliseconds) start-big-par))
(printf "Parallel: ~a ms\n" time-big-par)
(define speedup-big (/ (exact->inexact time-big-seq) (max 1 time-big-par)))
(printf "Speedup: ~ax\n\n" (~r speedup-big #:precision 2))

;; ==========================================
;; ИТОГОВАЯ ТАБЛИЦА
;; ==========================================

(printf "=== ИТОГОВАЯ ТАБЛИЦА ===\n\n")
(printf "| Тип чисел     | Sequential | Parallel | Speedup |\n")
(printf "|---------------|------------|----------|--------|\n")
(printf "| flonum        | ~a ms     | ~a ms   | ~ax   |\n" time-fl-seq time-fl-par (~r speedup-fl #:precision 2))
(printf "| rational      | ~a ms     | ~a ms   | ~ax   |\n" time-rat-seq time-rat-par (~r speedup-rat #:precision 2))
(printf "| bignum 10^20  | ~a ms     | ~a ms   | ~ax   |\n\n" time-big-seq time-big-par (~r speedup-big #:precision 2))

(printf "=== ВЕРДИКТ ===\n")
(cond
  [(and (> speedup-fl 1.5) (> speedup-rat 1.5) (> speedup-big 1.5))
   (printf "ВОВА НЕ ПРАВ: Все типы параллелятся!\n")]
  [(and (> speedup-fl 1.5) (< speedup-rat 1.5))
   (printf "ВОВА ПРАВ: flonum параллелится, rational/bignum - нет\n")]
  [(and (> speedup-fl 1.5) (> speedup-rat 1.5) (< speedup-big 1.5))
   (printf "ЧАСТИЧНО: flonum и rational ОК, bignum блокирует\n")]
  [else
   (printf "Неожиданный результат\n")])
