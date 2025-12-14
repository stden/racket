#lang racket

;; ============================================================
;; DOOM-STYLE RAYCASTER ENGINE
;; Classic 2.5D rendering like Wolfenstein 3D / DOOM
;; ============================================================

(require racket/flonum
         racket/future
         racket/fixnum)

(printf "=== DOOM-STYLE RAYCASTER ENGINE ===\n")
(printf "Processors: ~a\n\n" (processor-count))

;; ============================================================
;; Map Definition
;; ============================================================

(define map-width 16)
(define map-height 16)

;; 1 = wall, 0 = empty
(define game-map
  (bytes
   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
   1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
   1 0 1 1 1 0 0 0 0 0 1 1 1 0 0 1
   1 0 1 0 0 0 0 0 0 0 0 0 1 0 0 1
   1 0 1 0 0 0 0 0 0 0 0 0 1 0 0 1
   1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
   1 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1
   1 0 0 0 0 0 1 0 1 0 0 0 0 0 0 1
   1 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1
   1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
   1 0 1 0 0 0 0 0 0 0 0 0 1 0 0 1
   1 0 1 0 0 0 0 0 0 0 0 0 1 0 0 1
   1 0 1 1 1 0 0 0 0 0 1 1 1 0 0 1
   1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
   1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))

(define (get-map x y)
  (if (and (>= x 0) (< x map-width) (>= y 0) (< y map-height))
      (bytes-ref game-map (+ (* y map-width) x))
      1))

;; ============================================================
;; Player State
;; ============================================================

(define player-x 2.5)
(define player-y 2.5)
(define player-angle 0.0)
(define fov (fl/ 3.14159 3.0))  ;; 60 degrees

;; ============================================================
;; Raycasting
;; ============================================================

(define (cast-ray angle)
  ;; DDA algorithm for raycasting
  (define ray-dir-x (flcos angle))
  (define ray-dir-y (flsin angle))
  
  (define map-x (exact-floor player-x))
  (define map-y (exact-floor player-y))
  
  (define delta-dist-x (if (fl= ray-dir-x 0.0) 1e30 (flabs (fl/ 1.0 ray-dir-x))))
  (define delta-dist-y (if (fl= ray-dir-y 0.0) 1e30 (flabs (fl/ 1.0 ray-dir-y))))
  
  (define-values (step-x side-dist-x)
    (if (fl< ray-dir-x 0.0)
        (values -1 (fl* (fl- player-x (->fl map-x)) delta-dist-x))
        (values 1 (fl* (fl- (fl+ (->fl map-x) 1.0) player-x) delta-dist-x))))
  
  (define-values (step-y side-dist-y)
    (if (fl< ray-dir-y 0.0)
        (values -1 (fl* (fl- player-y (->fl map-y)) delta-dist-y))
        (values 1 (fl* (fl- (fl+ (->fl map-y) 1.0) player-y) delta-dist-y))))
  
  ;; DDA loop
  (let loop ([mx map-x] [my map-y] [sdx side-dist-x] [sdy side-dist-y] [side 0])
    (if (= (get-map mx my) 1)
        ;; Hit wall
        (let ([perp-wall-dist
               (if (= side 0)
                   (fl- sdx delta-dist-x)
                   (fl- sdy delta-dist-y))])
          (values perp-wall-dist side))
        ;; Step forward
        (if (fl< sdx sdy)
            (loop (+ mx step-x) my (fl+ sdx delta-dist-x) sdy 0)
            (loop mx (+ my step-y) sdx (fl+ sdy delta-dist-y) 1)))))

;; ============================================================
;; Rendering
;; ============================================================

(define (render-column! fb screen-width screen-height column)
  (define angle (fl+ player-angle 
                     (fl- (fl* (fl/ (->fl column) (->fl screen-width)) fov) 
                          (fl/ fov 2.0))))
  
  (define-values (dist side) (cast-ray angle))
  
  ;; Fish-eye correction
  (define corrected-dist (fl* dist (flcos (fl- angle player-angle))))
  
  ;; Wall height
  (define line-height (exact-floor (fl/ (->fl screen-height) (flmax 0.1 corrected-dist))))
  (define draw-start (max 0 (- (quotient screen-height 2) (quotient line-height 2))))
  (define draw-end (min (sub1 screen-height) (+ (quotient screen-height 2) (quotient line-height 2))))
  
  ;; Color based on distance and side
  (define base-color (fl/ 1.0 (fl+ 1.0 (fl* 0.1 corrected-dist))))
  (define wall-color (if (= side 1) (fl* base-color 0.7) base-color))
  
  ;; Draw column
  (for ([y (in-range screen-height)])
    (define idx (* 3 (+ (* y screen-width) column)))
    (cond
      [(< y draw-start)
       ;; Ceiling (dark blue)
       (flvector-set! fb idx 0.1)
       (flvector-set! fb (+ idx 1) 0.1)
       (flvector-set! fb (+ idx 2) 0.3)]
      [(> y draw-end)
       ;; Floor (dark green)
       (flvector-set! fb idx 0.1)
       (flvector-set! fb (+ idx 1) 0.2)
       (flvector-set! fb (+ idx 2) 0.1)]
      [else
       ;; Wall
       (flvector-set! fb idx wall-color)
       (flvector-set! fb (+ idx 1) (fl* wall-color 0.5))
       (flvector-set! fb (+ idx 2) 0.1)])))

(define (render-frame-sequential width height)
  (define fb (make-flvector (* width height 3) 0.0))
  (for ([x (in-range width)])
    (render-column! fb width height x))
  fb)

(define (render-frame-parallel width height)
  (define fb (make-flvector (* width height 3) 0.0))
  (define num-threads (processor-count))
  (define cols-per-thread (quotient width num-threads))
  
  (define futures
    (for/list ([t (in-range num-threads)])
      (define start-col (* t cols-per-thread))
      (define end-col (if (= t (sub1 num-threads)) width (+ start-col cols-per-thread)))
      (future
       (lambda ()
         (for ([x (in-range start-col end-col)])
           (render-column! fb width height x))))))
  
  (for-each touch futures)
  fb)

;; ============================================================
;; Frame Rate Simulation
;; ============================================================

(define (benchmark-fps render-fn width height frames)
  (define start (current-milliseconds))
  (for ([_ (in-range frames)])
    (render-fn width height))
  (define elapsed (- (current-milliseconds) start))
  (define fps (fl/ (fl* (->fl frames) 1000.0) (->fl (max 1 elapsed))))
  (values elapsed fps))

;; ============================================================
;; Tests
;; ============================================================

(printf "1. DOOM RAYCASTER 320x200 (classic resolution)\n")
(define-values (time-seq-1 fps-seq-1) (benchmark-fps render-frame-sequential 320 200 10))
(printf "   Sequential: ~a ms total, ~a FPS\n" time-seq-1 (~r fps-seq-1 #:precision 1))
(define-values (time-par-1 fps-par-1) (benchmark-fps render-frame-parallel 320 200 10))
(printf "   Parallel: ~a ms total, ~a FPS\n" time-par-1 (~r fps-par-1 #:precision 1))
(printf "   Speedup: ~ax\n\n" (~r (/ fps-par-1 (max 0.1 fps-seq-1)) #:precision 2))

(printf "2. DOOM RAYCASTER 640x400\n")
(define-values (time-seq-2 fps-seq-2) (benchmark-fps render-frame-sequential 640 400 5))
(printf "   Sequential: ~a ms total, ~a FPS\n" time-seq-2 (~r fps-seq-2 #:precision 1))
(define-values (time-par-2 fps-par-2) (benchmark-fps render-frame-parallel 640 400 5))
(printf "   Parallel: ~a ms total, ~a FPS\n" time-par-2 (~r fps-par-2 #:precision 1))
(printf "   Speedup: ~ax\n\n" (~r (/ fps-par-2 (max 0.1 fps-seq-2)) #:precision 2))

(printf "3. DOOM RAYCASTER 1280x800 (HD)\n")
(define-values (time-seq-3 fps-seq-3) (benchmark-fps render-frame-sequential 1280 800 2))
(printf "   Sequential: ~a ms total, ~a FPS\n" time-seq-3 (~r fps-seq-3 #:precision 1))
(define-values (time-par-3 fps-par-3) (benchmark-fps render-frame-parallel 1280 800 2))
(printf "   Parallel: ~a ms total, ~a FPS\n" time-par-3 (~r fps-par-3 #:precision 1))
(printf "   Speedup: ~ax\n\n" (~r (/ fps-par-3 (max 0.1 fps-seq-3)) #:precision 2))

(printf "=== SUMMARY ===\n")
(printf "| Resolution | Seq FPS | Par FPS | Speedup |\n")
(printf "|------------|---------|---------|--------|\n")
(printf "| 320x200    | ~a     | ~a     | ~ax    |\n" 
        (~r fps-seq-1 #:precision 1) (~r fps-par-1 #:precision 1) 
        (~r (/ fps-par-1 (max 0.1 fps-seq-1)) #:precision 2))
(printf "| 640x400    | ~a     | ~a     | ~ax    |\n"
        (~r fps-seq-2 #:precision 1) (~r fps-par-2 #:precision 1)
        (~r (/ fps-par-2 (max 0.1 fps-seq-2)) #:precision 2))
(printf "| 1280x800   | ~a     | ~a     | ~ax    |\n\n"
        (~r fps-seq-3 #:precision 1) (~r fps-par-3 #:precision 1)
        (~r (/ fps-par-3 (max 0.1 fps-seq-3)) #:precision 2))

(printf "=== DOOM ENGINE COMPLETE ===\n")
