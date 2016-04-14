#lang racket

(require racket/place
         racket/runtime-path)

(define-runtime-path this-module "places_heavy_demo.rkt")

;; Worker для тяжелых задач
(define (heavy-worker ch)
  (let loop ()
    (define msg (place-channel-get ch))
    (cond
      [(eq? msg 'stop)
       (place-channel-put ch 'done)]
      [(list? msg)
       (case (car msg)
         [(fib)
          (define n (cadr msg))
          (define (fib x)
            (if (< x 2) x (+ (fib (- x 1)) (fib (- x 2)))))
          (place-channel-put ch (list 'result (fib n)))
          (loop)]
         [(heavy-alloc)
          ;; Очень тяжелая аллокация
          (define n (cadr msg))
          (define result
            (for/fold ([sum 0]) ([i (in-range n)])
              (define s (format "item-~a-~a-~a" i (* i i) (+ i 1000)))
              (+ sum (string-length s))))
          (place-channel-put ch (list 'result result))
          (loop)]
         [else
          (place-channel-put ch 'error)
          (loop)])]
      [else (loop)])))

(provide heavy-worker)

(module+ main
  (printf "=== PLACES: ТЕСТ С ТЯЖЁЛЫМИ ЗАДАЧАМИ ===\n")
  (printf "Процессоров: ~a\n\n" (processor-count))
  
  ;; ==========================================
  ;; 1. ОЧЕНЬ ТЯЖЕЛЫЙ Fibonacci
  ;; ==========================================
  
  (printf "1. ТЯЖЕЛЫЙ FIBONACCI (n=40)\n")
  (printf "   Это займет несколько секунд...\n")
  
  (define fib-n 40)  ;; Очень тяжело!
  
  (define (fib x)
    (if (< x 2) x (+ (fib (- x 1)) (fib (- x 2)))))
  
  ;; Sequential
  (printf "   Sequential...\n")
  (define start-seq (current-milliseconds))
  (for ([_ (in-range 4)])
    (fib fib-n))
  (define time-seq (- (current-milliseconds) start-seq))
  (printf "   Sequential: ~a ms\n" time-seq)
  
  ;; Parallel with Places
  (printf "   Parallel (Places)...\n")
  (define start-par (current-milliseconds))
  
  (define workers
    (for/list ([_ (in-range 4)])
      (dynamic-place this-module 'heavy-worker)))
  
  (for ([w (in-list workers)])
    (place-channel-put w (list 'fib fib-n)))
  
  (define results
    (for/list ([w (in-list workers)])
      (place-channel-get w)))
  
  (for ([w (in-list workers)])
    (place-channel-put w 'stop)
    (place-channel-get w))
  
  (define time-par (- (current-milliseconds) start-par))
  (printf "   Parallel: ~a ms\n" time-par)
  (printf "   Speedup: ~ax\n" (~r (/ (exact->inexact time-seq) (max 1 time-par)) #:precision 2))
  (printf "   Результаты: ~a\n" (map cadr results))
  
  (if (> time-seq time-par)
      (printf "   >>> PLACES БЫСТРЕЕ! <<<\n\n")
      (printf "   (Places все еще имеют overhead)\n\n"))
  
  ;; ==========================================
  ;; 2. ТЯЖЕЛАЯ АЛЛОКАЦИЯ
  ;; ==========================================
  
  (printf "2. ТЯЖЕЛАЯ АЛЛОКАЦИЯ (1M итераций)\n")
  
  (define alloc-size 1000000)
  
  ;; Sequential
  (printf "   Sequential...\n")
  (define start-alloc-seq (current-milliseconds))
  (for ([_ (in-range 4)])
    (for/fold ([sum 0]) ([i (in-range alloc-size)])
      (define s (format "item-~a-~a-~a" i (* i i) (+ i 1000)))
      (+ sum (string-length s))))
  (define time-alloc-seq (- (current-milliseconds) start-alloc-seq))
  (printf "   Sequential: ~a ms\n" time-alloc-seq)
  
  ;; Parallel
  (printf "   Parallel (Places)...\n")
  (define start-alloc-par (current-milliseconds))
  
  (define alloc-workers
    (for/list ([_ (in-range 4)])
      (dynamic-place this-module 'heavy-worker)))
  
  (for ([w (in-list alloc-workers)])
    (place-channel-put w (list 'heavy-alloc alloc-size)))
  
  (define alloc-results
    (for/list ([w (in-list alloc-workers)])
      (place-channel-get w)))
  
  (for ([w (in-list alloc-workers)])
    (place-channel-put w 'stop)
    (place-channel-get w))
  
  (define time-alloc-par (- (current-milliseconds) start-alloc-par))
  (printf "   Parallel: ~a ms\n" time-alloc-par)
  (printf "   Speedup: ~ax\n" (~r (/ (exact->inexact time-alloc-seq) (max 1 time-alloc-par)) #:precision 2))
  
  (if (> time-alloc-seq time-alloc-par)
      (printf "   >>> PLACES БЫСТРЕЕ! <<<\n\n")
      (printf "   (Overhead все еще больше выигрыша)\n\n"))
  
  ;; ==========================================
  ;; ВЫВОД
  ;; ==========================================
  
  (printf "=== ВЫВОДЫ ===\n")
  (printf "1. Places имеют overhead ~200-300ms на создание\n")
  (printf "2. Для задач < 1 сек - используй Futures\n")
  (printf "3. Для задач > 1 сек - Places дают реальное ускорение\n")
  (printf "4. Для долгоживущих worker pools - Places идеальны\n"))
