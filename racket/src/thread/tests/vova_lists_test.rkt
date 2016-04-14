#lang racket

(require racket/future
         racket/flonum)

(printf "=== ПРОВЕРКА УТВЕРЖДЕНИЯ ВОВЫ О СПИСКАХ ===\n")
(printf "Вова: 'Достаточно чуть-чуть списками пользоваться чтобы всё сошло с рельс'\n")
(printf "Процессоров: ~a\n\n" (processor-count))

;; ==========================================
;; ТЕСТ 1: Чистый flonum (без списков)
;; ==========================================

(define (pure-flonum-work n)
  (for/fold ([sum 0.0]) ([i (in-range n)])
    (fl+ sum (flsin (fl* (->fl i) 0.0001)))))

(printf "=== ТЕСТ 1: ЧИСТЫЙ FLONUM (без списков) ===\n")

(define pure-n 5000000)

(define start-seq-1 (current-milliseconds))
(for ([_ (in-range 4)])
  (pure-flonum-work pure-n))
(define time-seq-1 (- (current-milliseconds) start-seq-1))
(printf "Sequential: ~a ms\n" time-seq-1)

(define start-par-1 (current-milliseconds))
(define fs-1 (for/list ([_ (in-range 4)])
               (future (lambda () (pure-flonum-work pure-n)))))
(for-each touch fs-1)
(define time-par-1 (- (current-milliseconds) start-par-1))
(printf "Parallel: ~a ms\n" time-par-1)
(printf "Speedup: ~ax\n\n" (~r (/ (exact->inexact time-seq-1) (max 1 time-par-1)) #:precision 2))

;; ==========================================
;; ТЕСТ 2: Flonum + периодическое создание списка
;; ==========================================

(define (flonum-with-list-creation n)
  (for/fold ([sum 0.0]) ([i (in-range n)])
    (when (= (modulo i 10000) 0)
      (list i (* i 2) (* i 3)))  ;; Создаём список периодически
    (fl+ sum (flsin (fl* (->fl i) 0.0001)))))

(printf "=== ТЕСТ 2: FLONUM + СОЗДАНИЕ СПИСКОВ ===\n")

(define start-seq-2 (current-milliseconds))
(for ([_ (in-range 4)])
  (flonum-with-list-creation pure-n))
(define time-seq-2 (- (current-milliseconds) start-seq-2))
(printf "Sequential: ~a ms\n" time-seq-2)

(define start-par-2 (current-milliseconds))
(define fs-2 (for/list ([_ (in-range 4)])
               (future (lambda () (flonum-with-list-creation pure-n)))))
(for-each touch fs-2)
(define time-par-2 (- (current-milliseconds) start-par-2))
(printf "Parallel: ~a ms\n" time-par-2)
(printf "Speedup: ~ax\n\n" (~r (/ (exact->inexact time-seq-2) (max 1 time-par-2)) #:precision 2))

;; ==========================================
;; ТЕСТ 3: Flonum + накопление в список
;; ==========================================

(define (flonum-accumulate-list n)
  (for/fold ([sum 0.0] [lst '()]) ([i (in-range n)])
    (values (fl+ sum (flsin (fl* (->fl i) 0.0001)))
            (if (= (modulo i 100000) 0)
                (cons i lst)
                lst))))

(printf "=== ТЕСТ 3: FLONUM + НАКОПЛЕНИЕ В СПИСОК ===\n")

(define acc-n 1000000)

(define start-seq-3 (current-milliseconds))
(for ([_ (in-range 4)])
  (flonum-accumulate-list acc-n))
(define time-seq-3 (- (current-milliseconds) start-seq-3))
(printf "Sequential: ~a ms\n" time-seq-3)

(define start-par-3 (current-milliseconds))
(define fs-3 (for/list ([_ (in-range 4)])
               (future (lambda () (flonum-accumulate-list acc-n)))))
(for-each touch fs-3)
(define time-par-3 (- (current-milliseconds) start-par-3))
(printf "Parallel: ~a ms\n" time-par-3)
(printf "Speedup: ~ax\n\n" (~r (/ (exact->inexact time-seq-3) (max 1 time-par-3)) #:precision 2))

;; ==========================================
;; ТЕСТ 4: Flonum + интенсивное использование списков
;; ==========================================

(define (flonum-heavy-list n)
  (for/fold ([sum 0.0] [lst '()]) ([i (in-range n)])
    (define new-lst (cons (flsin (fl* (->fl i) 0.0001)) lst))
    (values (fl+ sum (if (null? new-lst) 0.0 (car new-lst)))
            (if (> (length new-lst) 100)
                (cdr new-lst)
                new-lst))))

(printf "=== ТЕСТ 4: FLONUM + ИНТЕНСИВНЫЕ СПИСКИ ===\n")

(define heavy-n 500000)

(define start-seq-4 (current-milliseconds))
(for ([_ (in-range 4)])
  (flonum-heavy-list heavy-n))
(define time-seq-4 (- (current-milliseconds) start-seq-4))
(printf "Sequential: ~a ms\n" time-seq-4)

(define start-par-4 (current-milliseconds))
(define fs-4 (for/list ([_ (in-range 4)])
               (future (lambda () (flonum-heavy-list heavy-n)))))
(for-each touch fs-4)
(define time-par-4 (- (current-milliseconds) start-par-4))
(printf "Parallel: ~a ms\n" time-par-4)
(printf "Speedup: ~ax\n\n" (~r (/ (exact->inexact time-seq-4) (max 1 time-par-4)) #:precision 2))

;; ==========================================
;; ТЕСТ 5: Flonum + использование векторов (вместо списков)
;; ==========================================

(define (flonum-with-vector n)
  (define vec (make-flvector 100 0.0))
  (for/fold ([sum 0.0]) ([i (in-range n)])
    (flvector-set! vec (modulo i 100) (flsin (fl* (->fl i) 0.0001)))
    (fl+ sum (flvector-ref vec (modulo i 100)))))

(printf "=== ТЕСТ 5: FLONUM + ВЕКТОР (альтернатива спискам) ===\n")

(define start-seq-5 (current-milliseconds))
(for ([_ (in-range 4)])
  (flonum-with-vector pure-n))
(define time-seq-5 (- (current-milliseconds) start-seq-5))
(printf "Sequential: ~a ms\n" time-seq-5)

(define start-par-5 (current-milliseconds))
(define fs-5 (for/list ([_ (in-range 4)])
               (future (lambda () (flonum-with-vector pure-n)))))
(for-each touch fs-5)
(define time-par-5 (- (current-milliseconds) start-par-5))
(printf "Parallel: ~a ms\n" time-par-5)
(printf "Speedup: ~ax\n\n" (~r (/ (exact->inexact time-seq-5) (max 1 time-par-5)) #:precision 2))

;; ==========================================
;; ИТОГИ
;; ==========================================

(printf "=== ИТОГОВАЯ ТАБЛИЦА ===\n\n")
(printf "| Тест                       | Seq    | Par    | Speedup |\n")
(printf "|----------------------------|--------|--------|--------|\n")
(printf "| Чистый flonum              | ~a ms | ~a ms | ~ax    |\n" 
        time-seq-1 time-par-1 (~r (/ (exact->inexact time-seq-1) (max 1 time-par-1)) #:precision 2))
(printf "| Flonum + создание списков  | ~a ms | ~a ms | ~ax    |\n"
        time-seq-2 time-par-2 (~r (/ (exact->inexact time-seq-2) (max 1 time-par-2)) #:precision 2))
(printf "| Flonum + накопление списка | ~a ms | ~a ms | ~ax    |\n"
        time-seq-3 time-par-3 (~r (/ (exact->inexact time-seq-3) (max 1 time-par-3)) #:precision 2))
(printf "| Flonum + интенсивные списки| ~a ms | ~a ms | ~ax    |\n"
        time-seq-4 time-par-4 (~r (/ (exact->inexact time-seq-4) (max 1 time-par-4)) #:precision 2))
(printf "| Flonum + вектор            | ~a ms | ~a ms | ~ax    |\n\n"
        time-seq-5 time-par-5 (~r (/ (exact->inexact time-seq-5) (max 1 time-par-5)) #:precision 2))

(define speedup-1 (/ (exact->inexact time-seq-1) (max 1 time-par-1)))
(define speedup-4 (/ (exact->inexact time-seq-4) (max 1 time-par-4)))

(printf "=== ВЕРДИКТ ===\n")
(cond
  [(and (> speedup-1 1.5) (< speedup-4 1.2))
   (printf "ВОВА ПРАВ: Списки ломают параллелизм futures!\n")
   (printf "Решение: используй flvector вместо списков.\n")]
  [(and (> speedup-1 1.5) (> speedup-4 1.5))
   (printf "ВОВА НЕ ПРАВ: Даже со списками futures параллелятся.\n")]
  [else
   (printf "Неоднозначный результат - нужно больше тестов.\n")])
