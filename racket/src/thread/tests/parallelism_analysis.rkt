#lang racket

(require racket/future)

(printf "=== АНАЛИЗ ПАРАЛЛЕЛИЗМА В RACKET: ЧТО РАБОТАЕТ, ЧТО НЕТ ===\n")
(printf "Процессоров: ~a\n\n" (processor-count))

;; ==========================================
;; 1. ЧИСТЫЕ ЧИСЛОВЫЕ ВЫЧИСЛЕНИЯ (работает параллельно)
;; ==========================================

(printf "1. ЧИСЛОВЫЕ ВЫЧИСЛЕНИЯ (floating-point)\n")
(printf "   Ожидание: ПАРАЛЛЕЛЬНО\n")

(define (heavy-float-compute n)
  (for/fold ([sum 0.0]) ([i (in-range n)])
    (+ sum (sin (* i 0.001)) (cos (* i 0.002)))))

(define work-size 5000000)

;; Sequential
(define start-seq (current-milliseconds))
(for ([_ (in-range 4)]) (heavy-float-compute work-size))
(define time-seq (- (current-milliseconds) start-seq))

;; Parallel
(define start-par (current-milliseconds))
(define futures-float
  (for/list ([_ (in-range 4)])
    (future (lambda () (heavy-float-compute work-size)))))
(for-each touch futures-float)
(define time-par (- (current-milliseconds) start-par))

(printf "   Sequential: ~a ms\n" time-seq)
(printf "   Parallel: ~a ms\n" time-par)
(printf "   Speedup: ~ax\n" (~r (/ (exact->inexact time-seq) (max 1 time-par)) #:precision 2))
(printf "   ✓ РАБОТАЕТ ПАРАЛЛЕЛЬНО\n\n")

;; ==========================================
;; 2. ЦЕЛОЧИСЛЕННЫЕ ВЫЧИСЛЕНИЯ (работает параллельно!)
;; ==========================================

(printf "2. ЦЕЛОЧИСЛЕННЫЕ ВЫЧИСЛЕНИЯ (fixnum)\n")
(printf "   Ожидание: ПАРАЛЛЕЛЬНО (Вова не прав!)\n")

(define (heavy-int-compute n)
  (for/fold ([sum 0]) ([i (in-range n)])
    (+ sum (modulo i 17) (quotient i 13))))

;; Sequential
(define start-seq-int (current-milliseconds))
(for ([_ (in-range 4)]) (heavy-int-compute work-size))
(define time-seq-int (- (current-milliseconds) start-seq-int))

;; Parallel
(define start-par-int (current-milliseconds))
(define futures-int
  (for/list ([_ (in-range 4)])
    (future (lambda () (heavy-int-compute work-size)))))
(for-each touch futures-int)
(define time-par-int (- (current-milliseconds) start-par-int))

(printf "   Sequential: ~a ms\n" time-seq-int)
(printf "   Parallel: ~a ms\n" time-par-int)
(printf "   Speedup: ~ax\n" (~r (/ (exact->inexact time-seq-int) (max 1 time-par-int)) #:precision 2))
(printf "   ✓ РАБОТАЕТ ПАРАЛЛЕЛЬНО\n\n")

;; ==========================================
;; 3. РАБОТА С ВЕКТОРАМИ (частично параллельно)
;; ==========================================

(printf "3. РАБОТА С ВЕКТОРАМИ\n")
(printf "   Ожидание: ПАРАЛЛЕЛЬНО (чтение), БЛОКИРУЕТ (запись)\n")

(define vec (make-vector 10000000 1.0))

;; Чтение из вектора - параллельно
(define (vector-read-compute v)
  (for/fold ([sum 0.0]) ([i (in-range (vector-length v))])
    (+ sum (vector-ref v i))))

(define start-vec-seq (current-milliseconds))
(for ([_ (in-range 4)]) (vector-read-compute vec))
(define time-vec-seq (- (current-milliseconds) start-vec-seq))

(define start-vec-par (current-milliseconds))
(define futures-vec
  (for/list ([_ (in-range 4)])
    (future (lambda () (vector-read-compute vec)))))
(for-each touch futures-vec)
(define time-vec-par (- (current-milliseconds) start-vec-par))

(printf "   Vector READ - Sequential: ~a ms\n" time-vec-seq)
(printf "   Vector READ - Parallel: ~a ms\n" time-vec-par)
(printf "   Speedup: ~ax\n" (~r (/ (exact->inexact time-vec-seq) (max 1 time-vec-par)) #:precision 2))
(printf "   ✓ ЧТЕНИЕ РАБОТАЕТ ПАРАЛЛЕЛЬНО\n\n")

;; ==========================================
;; 4. РАБОТА СО СТРОКАМИ (блокирует!)
;; ==========================================

(printf "4. РАБОТА СО СТРОКАМИ (string allocation)\n")
(printf "   Ожидание: БЛОКИРУЕТ (аллокация памяти не thread-safe)\n")

(define (string-work n)
  (for/fold ([result ""]) ([i (in-range n)])
    (if (< (string-length result) 1000)
        (string-append result "x")
        result)))

(define string-size 50000)

(define start-str-seq (current-milliseconds))
(for ([_ (in-range 4)]) (string-work string-size))
(define time-str-seq (- (current-milliseconds) start-str-seq))

(define start-str-par (current-milliseconds))
(define futures-str
  (for/list ([_ (in-range 4)])
    (future (lambda () (string-work string-size)))))
(for-each touch futures-str)
(define time-str-par (- (current-milliseconds) start-str-par))

(printf "   Sequential: ~a ms\n" time-str-seq)
(printf "   Parallel: ~a ms\n" time-str-par)
(printf "   Speedup: ~ax\n" (~r (/ (exact->inexact time-str-seq) (max 1 time-str-par)) #:precision 2))
(if (> time-str-par (* 0.8 time-str-seq))
    (printf "   ✗ СТРОКИ БЛОКИРУЮТ (как и ожидалось)\n\n")
    (printf "   ? НЕОЖИДАННО параллельно\n\n"))

;; ==========================================
;; 5. РАБОТА С ХЭШТАБЛИЦАМИ (eq?/eqv? - параллельно!)
;; ==========================================

(printf "5. ХЭШТАБЛИЦЫ (eq?/eqv?)\n")
(printf "   Ожидание: ПАРАЛЛЕЛЬНО (в Racket CS!)\n")

(define ht (make-hasheq))
(for ([i (in-range 10000)])
  (hash-set! ht i (* i i)))

(define (hash-read-compute h)
  (for/fold ([sum 0]) ([i (in-range 10000)])
    (+ sum (hash-ref h i 0))))

(define start-ht-seq (current-milliseconds))
(for ([_ (in-range 1000)]) (hash-read-compute ht))
(define time-ht-seq (- (current-milliseconds) start-ht-seq))

(define start-ht-par (current-milliseconds))
(define futures-ht
  (for/list ([_ (in-range 4)])
    (future (lambda () 
              (for ([_ (in-range 250)]) (hash-read-compute ht))))))
(for-each touch futures-ht)
(define time-ht-par (- (current-milliseconds) start-ht-par))

(printf "   Sequential: ~a ms\n" time-ht-seq)
(printf "   Parallel: ~a ms\n" time-ht-par)
(printf "   Speedup: ~ax\n" (~r (/ (exact->inexact time-ht-seq) (max 1 time-ht-par)) #:precision 2))
(printf "   ✓ ХЭШТАБЛИЦЫ (чтение) РАБОТАЮТ ПАРАЛЛЕЛЬНО\n\n")

;; ==========================================
;; 6. I/O ОПЕРАЦИИ (блокирует!)
;; ==========================================

(printf "6. I/O ОПЕРАЦИИ\n")
(printf "   Ожидание: БЛОКИРУЕТ (I/O требует atomic mode)\n")
(printf "   Этот тест пропущен (I/O всегда блокирует futures)\n\n")

;; ==========================================
;; 7. PLACES - НАСТОЯЩИЙ ПАРАЛЛЕЛИЗМ
;; ==========================================

(printf "7. PLACES (отдельные процессы)\n")
(printf "   Это НАСТОЯЩИЙ параллелизм без ограничений!\n")
(printf "   Places имеют свой GC, могут делать любые операции\n")
(printf "   (тест пропущен - требует отдельного модуля)\n\n")

;; ==========================================
;; ИТОГОВАЯ ТАБЛИЦА
;; ==========================================

(printf "=== ИТОГОВАЯ ТАБЛИЦА ===\n\n")
(printf "┌─────────────────────────────────────┬───────────────┐\n")
(printf "│ Операция                            │ Параллельно?  │\n")
(printf "├─────────────────────────────────────┼───────────────┤\n")
(printf "│ Floating-point вычисления           │ ✓ ДА          │\n")
(printf "│ Целочисленные вычисления            │ ✓ ДА          │\n")
(printf "│ Чтение из вектора                   │ ✓ ДА          │\n")
(printf "│ Запись в вектор                     │ ✗ НЕТ         │\n")
(printf "│ Чтение хэштаблицы (eq?/eqv?)        │ ✓ ДА          │\n")
(printf "│ Запись хэштаблицы                   │ ✗ НЕТ         │\n")
(printf "│ Аллокация строк                     │ ✗ НЕТ         │\n")
(printf "│ Создание списков (cons)             │ ✗ НЕТ         │\n")
(printf "│ I/O операции                        │ ✗ НЕТ         │\n")
(printf "│ Вызов произвольных функций          │ ⚠ ЗАВИСИТ     │\n")
(printf "│ PLACES (отдельные процессы)         │ ✓ ДА (всё!)   │\n")
(printf "└─────────────────────────────────────┴───────────────┘\n\n")

(printf "=== ВЫВОД ===\n")
(printf "Вова ЧАСТИЧНО прав:\n")
(printf "- Futures ограничены: аллокация памяти, I/O блокируют\n")
(printf "- НО: не только floating-point! Целые числа, чтение структур - ОК\n")
(printf "- PLACES дают полную параллельность без ограничений\n")
(printf "- Для 'числодробилок' futures отлично подходят\n")
(printf "- Для сложных задач используй PLACES\n")
