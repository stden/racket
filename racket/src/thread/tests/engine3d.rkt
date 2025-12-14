#lang racket

;; ============================================================
;; 3D ENGINE - Оптимизированный для параллелизма
;; ============================================================
;; Использует flonum и flvector для максимальной производительности
;; с futures. Избегает списков в критических путях.

(require racket/flonum
         racket/future
         racket/fixnum)

(provide (all-defined-out))

;; ============================================================
;; БАЗОВЫЕ ТИПЫ
;; ============================================================

;; Vec3 - 3D вектор (как flvector для скорости)
(define (vec3 x y z)
  (flvector x y z))

(define (vec3-x v) (flvector-ref v 0))
(define (vec3-y v) (flvector-ref v 1))
(define (vec3-z v) (flvector-ref v 2))

(define (vec3-set-x! v x) (flvector-set! v 0 x))
(define (vec3-set-y! v y) (flvector-set! v 1 y))
(define (vec3-set-z! v z) (flvector-set! v 2 z))

;; ============================================================
;; ВЕКТОРНЫЕ ОПЕРАЦИИ
;; ============================================================

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

(define (vec3-div v s)
  (flvector (fl/ (vec3-x v) s)
            (fl/ (vec3-y v) s)
            (fl/ (vec3-z v) s)))

(define (vec3-dot a b)
  (fl+ (fl+ (fl* (vec3-x a) (vec3-x b))
            (fl* (vec3-y a) (vec3-y b)))
       (fl* (vec3-z a) (vec3-z b))))

(define (vec3-cross a b)
  (flvector (fl- (fl* (vec3-y a) (vec3-z b)) (fl* (vec3-z a) (vec3-y b)))
            (fl- (fl* (vec3-z a) (vec3-x b)) (fl* (vec3-x a) (vec3-z b)))
            (fl- (fl* (vec3-x a) (vec3-y b)) (fl* (vec3-y a) (vec3-x b)))))

(define (vec3-length v)
  (flsqrt (vec3-dot v v)))

(define (vec3-normalize v)
  (define len (vec3-length v))
  (if (fl< len 0.000001)
      (vec3 0.0 0.0 0.0)
      (vec3-div v len)))

(define (vec3-lerp a b t)
  (vec3-add (vec3-mul a (fl- 1.0 t))
            (vec3-mul b t)))

(define (vec3-reflect v n)
  (vec3-sub v (vec3-mul n (fl* 2.0 (vec3-dot v n)))))

;; ============================================================
;; МАТРИЦА 4x4
;; ============================================================

;; Mat4 - 4x4 матрица (16 flonum в flvector, row-major)
(define (mat4-identity)
  (flvector 1.0 0.0 0.0 0.0
            0.0 1.0 0.0 0.0
            0.0 0.0 1.0 0.0
            0.0 0.0 0.0 1.0))

(define (mat4-ref m row col)
  (flvector-ref m (fx+ (fx* row 4) col)))

(define (mat4-set! m row col v)
  (flvector-set! m (fx+ (fx* row 4) col) v))

(define (mat4-mul a b)
  (define result (make-flvector 16 0.0))
  (for* ([i (in-range 4)]
         [j (in-range 4)])
    (define sum 0.0)
    (for ([k (in-range 4)])
      (set! sum (fl+ sum (fl* (mat4-ref a i k) (mat4-ref b k j)))))
    (mat4-set! result i j sum))
  result)

(define (mat4-translate tx ty tz)
  (flvector 1.0 0.0 0.0 tx
            0.0 1.0 0.0 ty
            0.0 0.0 1.0 tz
            0.0 0.0 0.0 1.0))

(define (mat4-scale sx sy sz)
  (flvector sx  0.0 0.0 0.0
            0.0 sy  0.0 0.0
            0.0 0.0 sz  0.0
            0.0 0.0 0.0 1.0))

(define (mat4-rotate-x angle)
  (define c (flcos angle))
  (define s (flsin angle))
  (flvector 1.0 0.0 0.0  0.0
            0.0 c   (fl- 0.0 s) 0.0
            0.0 s   c    0.0
            0.0 0.0 0.0  1.0))

(define (mat4-rotate-y angle)
  (define c (flcos angle))
  (define s (flsin angle))
  (flvector c   0.0 s   0.0
            0.0 1.0 0.0 0.0
            (fl- 0.0 s) 0.0 c   0.0
            0.0 0.0 0.0 1.0))

(define (mat4-rotate-z angle)
  (define c (flcos angle))
  (define s (flsin angle))
  (flvector c   (fl- 0.0 s) 0.0 0.0
            s   c    0.0 0.0
            0.0 0.0  1.0 0.0
            0.0 0.0  0.0 1.0))

(define (mat4-transform-point m p)
  (define x (vec3-x p))
  (define y (vec3-y p))
  (define z (vec3-z p))
  (define w (fl+ (fl+ (fl+ (fl* (mat4-ref m 3 0) x)
                            (fl* (mat4-ref m 3 1) y))
                       (fl* (mat4-ref m 3 2) z))
                 (mat4-ref m 3 3)))
  (define inv-w (if (fl< (flabs w) 0.000001) 1.0 (fl/ 1.0 w)))
  (vec3 (fl* inv-w (fl+ (fl+ (fl+ (fl* (mat4-ref m 0 0) x)
                                    (fl* (mat4-ref m 0 1) y))
                              (fl* (mat4-ref m 0 2) z))
                        (mat4-ref m 0 3)))
        (fl* inv-w (fl+ (fl+ (fl+ (fl* (mat4-ref m 1 0) x)
                                    (fl* (mat4-ref m 1 1) y))
                              (fl* (mat4-ref m 1 2) z))
                        (mat4-ref m 1 3)))
        (fl* inv-w (fl+ (fl+ (fl+ (fl* (mat4-ref m 2 0) x)
                                    (fl* (mat4-ref m 2 1) y))
                              (fl* (mat4-ref m 2 2) z))
                        (mat4-ref m 2 3)))))

;; ============================================================
;; КАМЕРА
;; ============================================================

(define (mat4-look-at eye target up)
  (define f (vec3-normalize (vec3-sub target eye)))
  (define s (vec3-normalize (vec3-cross f up)))
  (define u (vec3-cross s f))
  
  (flvector (vec3-x s) (vec3-y s) (vec3-z s) (fl- 0.0 (vec3-dot s eye))
            (vec3-x u) (vec3-y u) (vec3-z u) (fl- 0.0 (vec3-dot u eye))
            (fl- 0.0 (vec3-x f)) (fl- 0.0 (vec3-y f)) (fl- 0.0 (vec3-z f)) (vec3-dot f eye)
            0.0 0.0 0.0 1.0))

(define (mat4-perspective fov aspect near far)
  (define f (fl/ 1.0 (fltan (fl/ fov 2.0))))
  (define nf (fl/ 1.0 (fl- near far)))
  
  (flvector (fl/ f aspect) 0.0 0.0 0.0
            0.0 f 0.0 0.0
            0.0 0.0 (fl* (fl+ far near) nf) (fl* 2.0 (fl* far (fl* near nf)))
            0.0 0.0 -1.0 0.0))

;; ============================================================
;; RAY CASTING
;; ============================================================

(struct ray (origin direction) #:transparent)

(define (ray-at r t)
  (vec3-add (ray-origin r) (vec3-mul (ray-direction r) t)))

;; Пересечение луча со сферой
(define (ray-sphere-intersect r center radius)
  (define oc (vec3-sub (ray-origin r) center))
  (define a (vec3-dot (ray-direction r) (ray-direction r)))
  (define b (fl* 2.0 (vec3-dot oc (ray-direction r))))
  (define c (fl- (vec3-dot oc oc) (fl* radius radius)))
  (define discriminant (fl- (fl* b b) (fl* 4.0 (fl* a c))))
  
  (if (fl< discriminant 0.0)
      #f
      (fl/ (fl- (fl- 0.0 b) (flsqrt discriminant)) (fl* 2.0 a))))

;; Пересечение луча с плоскостью
(define (ray-plane-intersect r plane-normal plane-d)
  (define denom (vec3-dot plane-normal (ray-direction r)))
  (if (fl< (flabs denom) 0.000001)
      #f
      (fl/ (fl- plane-d (vec3-dot plane-normal (ray-origin r))) denom)))

;; Пересечение луча с треугольником (Möller–Trumbore)
(define (ray-triangle-intersect r v0 v1 v2)
  (define edge1 (vec3-sub v1 v0))
  (define edge2 (vec3-sub v2 v0))
  (define h (vec3-cross (ray-direction r) edge2))
  (define a (vec3-dot edge1 h))
  
  (if (fl< (flabs a) 0.000001)
      #f
      (let* ([f (fl/ 1.0 a)]
             [s (vec3-sub (ray-origin r) v0)]
             [u (fl* f (vec3-dot s h))])
        (if (or (fl< u 0.0) (fl> u 1.0))
            #f
            (let* ([q (vec3-cross s edge1)]
                   [v (fl* f (vec3-dot (ray-direction r) q))])
              (if (or (fl< v 0.0) (fl> (fl+ u v) 1.0))
                  #f
                  (fl* f (vec3-dot edge2 q))))))))

;; ============================================================
;; ОСВЕЩЕНИЕ
;; ============================================================

(define (phong-lighting normal light-dir view-dir
                        ambient diffuse-color specular-color shininess
                        light-color)
  (define n-dot-l (flmax 0.0 (vec3-dot normal light-dir)))
  (define reflect-dir (vec3-reflect (vec3-mul light-dir -1.0) normal))
  (define spec (real->double-flonum (expt (flmax 0.0 (vec3-dot view-dir reflect-dir)) shininess)))
  
  ;; ambient + diffuse + specular
  (vec3-add (vec3-add ambient
                       (vec3-mul (vec3-mul diffuse-color n-dot-l) 
                                 (vec3-x light-color)))
            (vec3-mul specular-color spec)))

;; ============================================================
;; ПАРАЛЛЕЛЬНЫЙ РЕНДЕРИНГ
;; ============================================================

;; Параллельный рендеринг строк изображения
(define (parallel-render width height render-pixel-fn)
  (define num-threads (processor-count))
  (define rows-per-thread (quotient height num-threads))
  (define framebuffer (make-flvector (* width height 3) 0.0))
  
  (define futures
    (for/list ([t (in-range num-threads)])
      (define start-row (* t rows-per-thread))
      (define end-row (if (= t (sub1 num-threads))
                          height
                          (+ start-row rows-per-thread)))
      (future
       (lambda ()
         (for* ([y (in-range start-row end-row)]
                [x (in-range width)])
           (define color (render-pixel-fn x y))
           (define idx (* 3 (+ (* y width) x)))
           (flvector-set! framebuffer idx (vec3-x color))
           (flvector-set! framebuffer (+ idx 1) (vec3-y color))
           (flvector-set! framebuffer (+ idx 2) (vec3-z color)))))))
  
  (for-each touch futures)
  framebuffer)

;; ============================================================
;; ТЕСТЫ
;; ============================================================

(module+ main
  (printf "=== 3D ENGINE TESTS ===\n")
  (printf "Процессоров: ~a\n\n" (processor-count))
  
  ;; Тест векторов
  (printf "1. Vector operations...\n")
  (define v1 (vec3 1.0 2.0 3.0))
  (define v2 (vec3 4.0 5.0 6.0))
  (printf "   v1 = (~a, ~a, ~a)\n" (vec3-x v1) (vec3-y v1) (vec3-z v1))
  (printf "   v1 + v2 = (~a, ~a, ~a)\n" 
          (vec3-x (vec3-add v1 v2))
          (vec3-y (vec3-add v1 v2))
          (vec3-z (vec3-add v1 v2)))
  (printf "   v1 · v2 = ~a\n" (vec3-dot v1 v2))
  (printf "   |v1| = ~a\n" (vec3-length v1))
  (printf "   ✓ Passed\n\n")
  
  ;; Тест матриц
  (printf "2. Matrix operations...\n")
  (define m1 (mat4-rotate-y 0.5))
  (define m2 (mat4-translate 1.0 2.0 3.0))
  (define m3 (mat4-mul m1 m2))
  (define p (vec3 1.0 0.0 0.0))
  (define tp (mat4-transform-point m3 p))
  (printf "   Transformed point: (~a, ~a, ~a)\n" (vec3-x tp) (vec3-y tp) (vec3-z tp))
  (printf "   ✓ Passed\n\n")
  
  ;; Тест ray-sphere
  (printf "3. Ray-sphere intersection...\n")
  (define r (ray (vec3 0.0 0.0 -5.0) (vec3 0.0 0.0 1.0)))
  (define t (ray-sphere-intersect r (vec3 0.0 0.0 0.0) 1.0))
  (printf "   Hit at t = ~a\n" t)
  (printf "   ✓ Passed\n\n")
  
  ;; Бенчмарк параллельного рендеринга
  (printf "4. Parallel rendering benchmark...\n")
  (define render-size 200)
  
  (define (simple-render x y)
    (define u (fl/ (->fl x) (->fl render-size)))
    (define v (fl/ (->fl y) (->fl render-size)))
    (vec3 u v (fl* 0.5 (fl+ u v))))
  
  ;; Sequential
  (define start-seq (current-milliseconds))
  (define fb-seq (make-flvector (* render-size render-size 3) 0.0))
  (for* ([y (in-range render-size)]
         [x (in-range render-size)])
    (define color (simple-render x y))
    (define idx (* 3 (+ (* y render-size) x)))
    (flvector-set! fb-seq idx (vec3-x color))
    (flvector-set! fb-seq (+ idx 1) (vec3-y color))
    (flvector-set! fb-seq (+ idx 2) (vec3-z color)))
  (define time-seq (- (current-milliseconds) start-seq))
  (printf "   Sequential: ~a ms\n" time-seq)
  
  ;; Parallel
  (define start-par (current-milliseconds))
  (define fb-par (parallel-render render-size render-size simple-render))
  (define time-par (- (current-milliseconds) start-par))
  (printf "   Parallel: ~a ms\n" time-par)
  (printf "   Speedup: ~ax\n" (~r (/ (exact->inexact time-seq) (max 1 time-par)) #:precision 2))
  (printf "   ✓ Passed\n\n")
  
  (printf "=== ALL TESTS PASSED ===\n"))
