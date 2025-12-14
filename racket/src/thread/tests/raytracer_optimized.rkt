#lang racket

;; ============================================================
;; OPTIMIZED RAY TRACER - Zero allocation in hot path
;; ============================================================

(require racket/flonum
         racket/future
         racket/unsafe/ops)

(printf "=== OPTIMIZED RAY TRACER (zero alloc) ===\n")
(printf "Processors: ~a\n\n" (processor-count))

;; ============================================================
;; Scene as flat arrays (no allocation during tracing)
;; ============================================================

;; 5 spheres: [cx, cy, cz, radius, r, g, b] x 5 = 35 floats
(define sphere-data
  (flvector 
   ;; Sphere 0: center, radius, color
   0.0 0.0 5.0 1.0 1.0 0.3 0.3
   ;; Sphere 1
   2.0 0.0 6.0 1.0 0.3 1.0 0.3
   ;; Sphere 2
   -2.0 0.0 6.0 1.0 0.3 0.3 1.0
   ;; Sphere 3
   0.0 2.0 5.0 0.5 1.0 1.0 0.3
   ;; Sphere 4 (ground)
   0.0 -101.0 5.0 100.0 0.5 0.5 0.5))

(define num-spheres 5)
(define light-x -5.0)
(define light-y 5.0)
(define light-z 0.0)

;; ============================================================
;; Core ray tracing (no allocation)
;; ============================================================

(define (trace-pixel! fb idx width height x y)
  (define aspect (fl/ (->fl width) (->fl height)))
  (define fov 1.0)
  (define px (fl* fov (fl* aspect (fl- (fl/ (fl* 2.0 (->fl x)) (->fl width)) 1.0))))
  (define py (fl* fov (fl- 1.0 (fl/ (fl* 2.0 (->fl y)) (->fl height)))))
  
  ;; Normalize direction
  (define len (flsqrt (fl+ (fl+ (fl* px px) (fl* py py)) 1.0)))
  (define dx (fl/ px len))
  (define dy (fl/ py len))
  (define dz (fl/ 1.0 len))
  
  ;; Origin at 0,0,0
  (define ox 0.0)
  (define oy 0.0)
  (define oz 0.0)
  
  ;; Find closest hit
  (define closest-t +inf.0)
  (define hit-sphere -1)
  
  (for ([s (in-range num-spheres)])
    (define base (* s 7))
    (define cx (flvector-ref sphere-data base))
    (define cy (flvector-ref sphere-data (+ base 1)))
    (define cz (flvector-ref sphere-data (+ base 2)))
    (define radius (flvector-ref sphere-data (+ base 3)))
    
    ;; Ray-sphere intersection
    (define ocx (fl- ox cx))
    (define ocy (fl- oy cy))
    (define ocz (fl- oz cz))
    
    (define a (fl+ (fl+ (fl* dx dx) (fl* dy dy)) (fl* dz dz)))
    (define b (fl* 2.0 (fl+ (fl+ (fl* ocx dx) (fl* ocy dy)) (fl* ocz dz))))
    (define c (fl- (fl+ (fl+ (fl* ocx ocx) (fl* ocy ocy)) (fl* ocz ocz)) (fl* radius radius)))
    (define disc (fl- (fl* b b) (fl* 4.0 (fl* a c))))
    
    (when (fl>= disc 0.0)
      (define t (fl/ (fl- (fl- 0.0 b) (flsqrt disc)) (fl* 2.0 a)))
      (when (and (fl> t 0.001) (fl< t closest-t))
        (set! closest-t t)
        (set! hit-sphere s))))
  
  ;; Shade pixel
  (cond
    [(= hit-sphere -1)
     ;; Background
     (flvector-set! fb idx 0.1)
     (flvector-set! fb (+ idx 1) 0.1)
     (flvector-set! fb (+ idx 2) 0.2)]
    [else
     ;; Hit point
     (define hx (fl+ ox (fl* dx closest-t)))
     (define hy (fl+ oy (fl* dy closest-t)))
     (define hz (fl+ oz (fl* dz closest-t)))
     
     ;; Sphere data
     (define base (* hit-sphere 7))
     (define cx (flvector-ref sphere-data base))
     (define cy (flvector-ref sphere-data (+ base 1)))
     (define cz (flvector-ref sphere-data (+ base 2)))
     (define radius (flvector-ref sphere-data (+ base 3)))
     (define cr (flvector-ref sphere-data (+ base 4)))
     (define cg (flvector-ref sphere-data (+ base 5)))
     (define cb (flvector-ref sphere-data (+ base 6)))
     
     ;; Normal
     (define nx (fl/ (fl- hx cx) radius))
     (define ny (fl/ (fl- hy cy) radius))
     (define nz (fl/ (fl- hz cz) radius))
     
     ;; To light
     (define lx (fl- light-x hx))
     (define ly (fl- light-y hy))
     (define lz (fl- light-z hz))
     (define ll (flsqrt (fl+ (fl+ (fl* lx lx) (fl* ly ly)) (fl* lz lz))))
     (define ldx (fl/ lx ll))
     (define ldy (fl/ ly ll))
     (define ldz (fl/ lz ll))
     
     ;; Diffuse
     (define diff (flmax 0.0 (fl+ (fl+ (fl* nx ldx) (fl* ny ldy)) (fl* nz ldz))))
     (define shade (fl+ 0.2 (fl* 0.8 diff)))
     
     (flvector-set! fb idx (fl* cr shade))
     (flvector-set! fb (+ idx 1) (fl* cg shade))
     (flvector-set! fb (+ idx 2) (fl* cb shade))]))

;; ============================================================
;; Rendering
;; ============================================================

(define (render-sequential width height)
  (define fb (make-flvector (* width height 3) 0.0))
  (for* ([y (in-range height)]
         [x (in-range width)])
    (define idx (* 3 (+ (* y width) x)))
    (trace-pixel! fb idx width height x y))
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
           (define idx (* 3 (+ (* y width) x)))
           (trace-pixel! fb idx width height x y))))))
  
  (for-each touch futures)
  fb)

;; ============================================================
;; Benchmarks
;; ============================================================

(printf "1. OPTIMIZED RAY TRACER 200x200\n")
(define start-1 (current-milliseconds))
(render-sequential 200 200)
(define time-seq-1 (- (current-milliseconds) start-1))
(printf "   Sequential: ~a ms\n" time-seq-1)

(define start-1p (current-milliseconds))
(render-parallel 200 200)
(define time-par-1 (- (current-milliseconds) start-1p))
(printf "   Parallel: ~a ms\n" time-par-1)
(printf "   Speedup: ~ax\n\n" (~r (/ (exact->inexact time-seq-1) (max 1 time-par-1)) #:precision 2))

(printf "2. OPTIMIZED RAY TRACER 400x400\n")
(define start-2 (current-milliseconds))
(render-sequential 400 400)
(define time-seq-2 (- (current-milliseconds) start-2))
(printf "   Sequential: ~a ms\n" time-seq-2)

(define start-2p (current-milliseconds))
(render-parallel 400 400)
(define time-par-2 (- (current-milliseconds) start-2p))
(printf "   Parallel: ~a ms\n" time-par-2)
(printf "   Speedup: ~ax\n\n" (~r (/ (exact->inexact time-seq-2) (max 1 time-par-2)) #:precision 2))

(printf "3. OPTIMIZED RAY TRACER 800x800\n")
(define start-3 (current-milliseconds))
(render-sequential 800 800)
(define time-seq-3 (- (current-milliseconds) start-3))
(printf "   Sequential: ~a ms\n" time-seq-3)

(define start-3p (current-milliseconds))
(render-parallel 800 800)
(define time-par-3 (- (current-milliseconds) start-3p))
(printf "   Parallel: ~a ms\n" time-par-3)
(printf "   Speedup: ~ax\n\n" (~r (/ (exact->inexact time-seq-3) (max 1 time-par-3)) #:precision 2))

(printf "=== SUMMARY ===\n")
(printf "| Resolution | Sequential | Parallel | Speedup |\n")
(printf "|------------|------------|----------|--------|\n")
(printf "| 200x200    | ~a ms      | ~a ms    | ~ax    |\n" 
        time-seq-1 time-par-1 (~r (/ (exact->inexact time-seq-1) (max 1 time-par-1)) #:precision 2))
(printf "| 400x400    | ~a ms      | ~a ms    | ~ax    |\n"
        time-seq-2 time-par-2 (~r (/ (exact->inexact time-seq-2) (max 1 time-par-2)) #:precision 2))
(printf "| 800x800    | ~a ms      | ~a ms    | ~ax    |\n\n"
        time-seq-3 time-par-3 (~r (/ (exact->inexact time-seq-3) (max 1 time-par-3)) #:precision 2))

(printf "Key optimization: NO ALLOCATION in trace loop!\n")
(printf "- Scene stored as flat flvector\n")
(printf "- All intermediate values as local flonum variables\n")
(printf "- Only flvector-set! on pre-allocated framebuffer\n")
