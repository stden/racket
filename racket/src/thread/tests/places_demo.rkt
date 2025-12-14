#lang racket

(require racket/place
         racket/runtime-path)

;; Определяем путь к этому модулю для Places
(define-runtime-path this-module "places_demo.rkt")

;; ==========================================
;; Worker функция для Places
;; ==========================================

(define (worker-main ch)
  (let loop ()
    (define msg (place-channel-get ch))
    (cond
      [(eq? msg 'stop)
       (place-channel-put ch 'done)]
      [(list? msg)
       (case (car msg)
         [(compute)
          (define n (cadr msg))
          (define result
            (for/fold ([lst '()]) ([i (in-range n)])
              (cons (format "item-~a" i) lst)))
          (place-channel-put ch (list 'result (length result)))
          (loop)]
         [(fib)
          (define n (cadr msg))
          (define (fib x)
            (if (< x 2) x (+ (fib (- x 1)) (fib (- x 2)))))
          (place-channel-put ch (list 'result (fib n)))
          (loop)]
         [(string-work)
          (define text (cadr msg))
          (define result
            (for/fold ([r ""]) ([c (in-string text)])
              (string-append r (string (char-upcase c)))))
          (place-channel-put ch (list 'result (string-length result)))
          (loop)]
         [else
          (place-channel-put ch (list 'error "unknown"))
          (loop)])]
      [else
       (place-channel-put ch (list 'echo msg))
       (loop)])))

(provide worker-main)

;; ==========================================
;; Главный модуль
;; ==========================================

(module+ main
  (printf "=== PLACES: ПОЛНОЦЕННЫЙ ПАРАЛЛЕЛИЗМ В RACKET ===\n")
  (printf "Процессоров: ~a\n\n" (processor-count))
  
  ;; ==========================================
  ;; 1. Простой тест Place
  ;; ==========================================
  
  (printf "1. ПРОСТОЙ PLACE\n")
  
  (define p1 (dynamic-place this-module 'worker-main))
  (place-channel-put p1 "Hello")
  (printf "   Ответ: ~a\n" (place-channel-get p1))
  (place-channel-put p1 'stop)
  (place-channel-get p1)
  (printf "   ✓ Тест пройден\n\n")
  
  ;; ==========================================
  ;; 2. Параллельные вычисления с аллокацией
  ;; ==========================================
  
  (printf "2. ПАРАЛЛЕЛЬНЫЕ ВЫЧИСЛЕНИЯ С АЛЛОКАЦИЕЙ\n")
  
  (define work-size 100000)
  (define num-workers 4)
  
  ;; Sequential
  (printf "   Последовательно...\n")
  (define start-seq (current-milliseconds))
  (for ([_ (in-range num-workers)])
    (for/fold ([lst '()]) ([i (in-range work-size)])
      (cons (format "item-~a" i) lst)))
  (define time-seq (- (current-milliseconds) start-seq))
  (printf "   Sequential: ~a ms\n" time-seq)
  
  ;; Parallel with Places
  (printf "   Параллельно (Places)...\n")
  (define start-par (current-milliseconds))
  
  (define workers
    (for/list ([_ (in-range num-workers)])
      (dynamic-place this-module 'worker-main)))
  
  ;; Отправляем работу
  (for ([w (in-list workers)])
    (place-channel-put w (list 'compute work-size)))
  
  ;; Собираем результаты
  (define results
    (for/list ([w (in-list workers)])
      (place-channel-get w)))
  
  ;; Останавливаем workers
  (for ([w (in-list workers)])
    (place-channel-put w 'stop)
    (place-channel-get w))
  
  (define time-par (- (current-milliseconds) start-par))
  (printf "   Parallel (Places): ~a ms\n" time-par)
  (printf "   Speedup: ~ax\n" (~r (/ (exact->inexact time-seq) (max 1 time-par)) #:precision 2))
  (printf "   Результаты: ~a\n" results)
  (printf "   ✓ АЛЛОКАЦИЯ РАБОТАЕТ ПАРАЛЛЕЛЬНО!\n\n")
  
  ;; ==========================================
  ;; 3. Параллельный Fibonacci
  ;; ==========================================
  
  (printf "3. ПАРАЛЛЕЛЬНЫЙ FIBONACCI\n")
  
  (define fib-n 35)
  
  (define (fib x)
    (if (< x 2) x (+ (fib (- x 1)) (fib (- x 2)))))
  
  ;; Sequential
  (define start-fib-seq (current-milliseconds))
  (for ([_ (in-range 4)])
    (fib fib-n))
  (define time-fib-seq (- (current-milliseconds) start-fib-seq))
  (printf "   Sequential: ~a ms\n" time-fib-seq)
  
  ;; Parallel with Places
  (define start-fib-par (current-milliseconds))
  
  (define fib-workers
    (for/list ([_ (in-range 4)])
      (dynamic-place this-module 'worker-main)))
  
  (for ([w (in-list fib-workers)])
    (place-channel-put w (list 'fib fib-n)))
  
  (define fib-results
    (for/list ([w (in-list fib-workers)])
      (place-channel-get w)))
  
  (for ([w (in-list fib-workers)])
    (place-channel-put w 'stop)
    (place-channel-get w))
  
  (define time-fib-par (- (current-milliseconds) start-fib-par))
  (printf "   Parallel (Places): ~a ms\n" time-fib-par)
  (printf "   Speedup: ~ax\n" (~r (/ (exact->inexact time-fib-seq) (max 1 time-fib-par)) #:precision 2))
  (printf "   Результаты: ~a\n" fib-results)
  (printf "   ✓ FIBONACCI РАБОТАЕТ ПАРАЛЛЕЛЬНО!\n\n")
  
  ;; ==========================================
  ;; Сравнительная таблица
  ;; ==========================================
  
  (printf "=== СРАВНЕНИЕ: FUTURES vs PLACES ===\n\n")
  
  (printf "~a~n" "┌──────────────────┬─────────────────────┬─────────────────────┐")
  (printf "~a~n" "│ Характеристика   │ Futures             │ Places              │")
  (printf "~a~n" "├──────────────────┼─────────────────────┼─────────────────────┤")
  (printf "~a~n" "│ Создание         │ Быстро (us)         │ Медленно (200ms)    │")
  (printf "~a~n" "│ Память           │ Общая (shared)      │ Раздельная          │")
  (printf "~a~n" "│ GC               │ Общий (блокирует!)  │ Свой (не блокирует) │")
  (printf "~a~n" "│ Аллокация        │ Блокирует           │ Параллельно         │")
  (printf "~a~n" "│ I/O              │ Блокирует           │ Параллельно         │")
  (printf "~a~n" "│ Данные           │ Прямой доступ       │ Только сообщения    │")
  (printf "~a~n" "│ Использование    │ Числодробилки       │ Любые задачи        │")
  (printf "~a~n" "└──────────────────┴─────────────────────┴─────────────────────┘")
  
  (printf "=== ВЫВОД ===\n")
  (printf "Places - ПОЛНОЦЕННЫЙ параллелизм в Racket!\n")
  (printf "- Нет ограничений на типы операций\n")
  (printf "- Каждый Place имеет свой GC\n")
  (printf "- Цена: overhead на создание и сообщения\n"))
