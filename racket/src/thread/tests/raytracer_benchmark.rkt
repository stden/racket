#lang racket

;; ============================================================
;; COMPLEX 3D RENDERING BENCHMARK
;; Ray tracer with parallel rendering
;; ============================================================

(require racket/flonum
         racket/future)

(printf "=== COMPLEX 3D RENDERING BENCHMARK ===\n")
(printf "Processors: ~a\n\n" (processor-count))

;; ============================================================
;; Vector Operations (inline for speed)
;; ============================================================

(define-syntax-rule (vec3 x y z) (flvector x y z))
(define-syntax-rule (vec3-x v) (flvector-ref v 0))
(define-syntax-rule (vec3-y v) (flvector-ref v 1))
(define-syntax-rule (vec3-z v) (flvector-ref v 2))

(define (vec3-add a b)
  (flvector (fl+ (vec3-x a) (vec3-x b))
            (fl+ (vec3-y a) (vec3-y b))
            (fl+ (vec3-z a) (vec3-z b))))

(define (vec3-sub a b)
  (flvector (fl- (vec3-x a) (vec3-x b))
            (fl- (vec3-y a) (vec3-y b))
            (fl- (vec3-z a) (vec3-z b))))

(define (vec3-mul v s)
  (flvector (fl* (vec3-x v) s)
            (fl* (vec3-y v) s)
            (fl* (vec3-z v) s)))

(define (vec3-dot a b)
  (fl+ (fl+ (fl* (vec3-x a) (vec3-x b))
            (fl* (vec3-y a) (vec3-y b)))
       (fl* (vec3-z a) (vec3-z b))))

(define (vec3-length v) (flsqrt (vec3-dot v v)))

(define (vec3-normalize v)
  (define len (vec3-length v))
  (if (fl< len 0.000001)
      (vec3 0.0 0.0 0.0)
      (flvector (fl/ (vec3-x v) len)
                (fl/ (vec3-y v) len)
                (fl/ (vec3-z v) len))))

;; ============================================================
;; Scene Definition
;; ============================================================

(struct sphere (center radius color) #:transparent)

(define scene
  (list (sphere (vec3 0.0 0.0 5.0) 1.0 (vec3 1.0 0.3 0.3))
        (sphere (vec3 2.0 0.0 6.0) 1.0 (vec3 0.3 1.0 0.3))
        (sphere (vec3 -2.0 0.0 6.0) 1.0 (vec3 0.3 0.3 1.0))
        (sphere (vec3 0.0 2.0 5.0) 0.5 (vec3 1.0 1.0 0.3))
        (sphere (vec3 0.0 -101.0 5.0) 100.0 (vec3 0.5 0.5 0.5))))

(define light-pos (vec3 -5.0 5.0 0.0))

;; ============================================================
;; Ray Tracing
;; ============================================================

(define (ray-sphere-hit origin dir sphere-obj)
  (define center (sphere-center sphere-obj))
  (define radius (sphere-radius sphere-obj))
  (define oc (vec3-sub origin center))
  (define a (vec3-dot dir dir))
  (define b (fl* 2.0 (vec3-dot oc dir)))
  (define c (fl- (vec3-dot oc oc) (fl* radius radius)))
  (define disc (fl- (fl* b b) (fl* 4.0 (fl* a c))))
  
  (if (fl< disc 0.0)
      #f
      (let ([t (fl/ (fl- (fl- 0.0 b) (flsqrt disc)) (fl* 2.0 a))])
        (if (fl> t 0.001) t #f))))

(define (trace-ray origin dir)
  (define closest-t +inf.0)
  (define closest-sphere #f)
  
  ;; Find closest intersection
  (for ([s (in-list scene)])
    (define t (ray-sphere-hit origin dir s))
    (when (and t (fl< t closest-t))
      (set! closest-t t)
      (set! closest-sphere s)))
  
  (if (not closest-sphere)
      (vec3 0.1 0.1 0.2)  ;; Background color
      (let* ([hit-point (vec3-add origin (vec3-mul dir closest-t))]
             [normal (vec3-normalize (vec3-sub hit-point (sphere-center closest-sphere)))]
             [to-light (vec3-normalize (vec3-sub light-pos hit-point))]
             [diff (flmax 0.0 (vec3-dot normal to-light))]
             [ambient 0.2]
             [color (sphere-color closest-sphere)])
        (vec3-mul color (fl+ ambient (fl* 0.8 diff))))))

;; ============================================================
;; Rendering
;; ============================================================

(define (render-pixel width height x y)
  (define aspect (fl/ (->fl width) (->fl height)))
  (define fov 1.0)
  (define px (fl* fov (fl* aspect (fl- (fl/ (fl* 2.0 (->fl x)) (->fl width)) 1.0))))
  (define py (fl* fov (fl- 1.0 (fl/ (fl* 2.0 (->fl y)) (->fl height)))))
  (define dir (vec3-normalize (vec3 px py 1.0)))
  (trace-ray (vec3 0.0 0.0 0.0) dir))

(define (render-sequential width height)
  (define fb (make-flvector (* width height 3) 0.0))
  (for* ([y (in-range height)]
         [x (in-range width)])
    (define color (render-pixel width height x y))
    (define idx (* 3 (+ (* y width) x)))
    (flvector-set! fb idx (vec3-x color))
    (flvector-set! fb (+ idx 1) (vec3-y color))
    (flvector-set! fb (+ idx 2) (vec3-z color)))
  fb)

(define (render-parallel width height)
  (define fb (make-flvector (* width height 3) 0.0))
  (define num-threads (processor-count))
  (define rows-per-thread (quotient height num-threads))
  
  (define futures
    (for/list ([t (in-range num-threads)])
      (define start-row (* t rows-per-thread))
      (define end-row (if (= t (sub1 num-threads)) height (+ start-row rows-per-thread)))
      (future
       (lambda ()
         (for* ([y (in-range start-row end-row)]
                [x (in-range width)])
           (define color (render-pixel width height x y))
           (define idx (* 3 (+ (* y width) x)))
           (flvector-set! fb idx (vec3-x color))
           (flvector-set! fb (+ idx 1) (vec3-y color))
           (flvector-set! fb (+ idx 2) (vec3-z color)))))))
  
  (for-each touch futures)
  fb)

;; ============================================================
;; Benchmarks
;; ============================================================

(printf "1. RAY TRACER 100x100\n")
(define start-1 (current-milliseconds))
(render-sequential 100 100)
(define time-seq-1 (- (current-milliseconds) start-1))
(printf "   Sequential: ~a ms\n" time-seq-1)

(define start-1p (current-milliseconds))
(render-parallel 100 100)
(define time-par-1 (- (current-milliseconds) start-1p))
(printf "   Parallel: ~a ms\n" time-par-1)
(printf "   Speedup: ~ax\n\n" (~r (/ (exact->inexact time-seq-1) (max 1 time-par-1)) #:precision 2))

(printf "2. RAY TRACER 200x200\n")
(define start-2 (current-milliseconds))
(render-sequential 200 200)
(define time-seq-2 (- (current-milliseconds) start-2))
(printf "   Sequential: ~a ms\n" time-seq-2)

(define start-2p (current-milliseconds))
(render-parallel 200 200)
(define time-par-2 (- (current-milliseconds) start-2p))
(printf "   Parallel: ~a ms\n" time-par-2)
(printf "   Speedup: ~ax\n\n" (~r (/ (exact->inexact time-seq-2) (max 1 time-par-2)) #:precision 2))

(printf "3. RAY TRACER 400x400\n")
(define start-3 (current-milliseconds))
(render-sequential 400 400)
(define time-seq-3 (- (current-milliseconds) start-3))
(printf "   Sequential: ~a ms\n" time-seq-3)

(define start-3p (current-milliseconds))
(render-parallel 400 400)
(define time-par-3 (- (current-milliseconds) start-3p))
(printf "   Parallel: ~a ms\n" time-par-3)
(printf "   Speedup: ~ax\n\n" (~r (/ (exact->inexact time-seq-3) (max 1 time-par-3)) #:precision 2))

(printf "=== SUMMARY ===\n")
(printf "| Resolution | Sequential | Parallel | Speedup |\n")
(printf "|------------|------------|----------|--------|\n")
(printf "| 100x100    | ~a ms      | ~a ms    | ~ax    |\n" 
        time-seq-1 time-par-1 (~r (/ (exact->inexact time-seq-1) (max 1 time-par-1)) #:precision 2))
(printf "| 200x200    | ~a ms      | ~a ms    | ~ax    |\n"
        time-seq-2 time-par-2 (~r (/ (exact->inexact time-seq-2) (max 1 time-par-2)) #:precision 2))
(printf "| 400x400    | ~a ms      | ~a ms    | ~ax    |\n\n"
        time-seq-3 time-par-3 (~r (/ (exact->inexact time-seq-3) (max 1 time-par-3)) #:precision 2))

(printf "=== BENCHMARK COMPLETE ===\n")
