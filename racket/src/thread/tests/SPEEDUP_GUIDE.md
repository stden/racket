# Как ускорить Racket: Полное руководство

## 1. Используй правильные типы данных

### Числа
```racket
;; МЕДЛЕННО: generic numbers
(+ x y)

;; БЫСТРО: flonum (машинные float64)
(require racket/flonum)
(fl+ x y)

;; БЫСТРО: fixnum (машинные целые)
(require racket/fixnum)
(fx+ x y)
```

### Векторы
```racket
;; МЕДЛЕННО: обычный вектор
(vector-ref v i)

;; БЫСТРО: типизированный вектор
(require racket/flonum)
(flvector-ref fv i)
```

## 2. Используй Typed Racket

```racket
#lang typed/racket

(: fibonacci (-> Integer Integer))
(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
```

Typed Racket генерирует более эффективный код благодаря информации о типах.

## 3. Параллелизм

### Futures (для числовых вычислений)
```racket
(require racket/future)

;; Параллельная обработка
(define futures
  (for/list ([i (in-range 4)])
    (future (lambda () (heavy-computation i)))))

(for-each touch futures)
```

**Ограничения Futures:**
- ✅ flonum операции
- ✅ fixnum операции  
- ✅ чтение из структур
- ❌ аллокация памяти (cons, string-append)
- ❌ I/O операции

### Places (для любых задач)
```racket
(require racket/place)

;; Настоящий параллелизм без ограничений
(define p (dynamic-place "worker.rkt" 'main))
(place-channel-put p data)
(place-channel-get p)
```

## 4. Компиляция

```bash
# Компиляция в байткод
raco make program.rkt

# Создание исполняемого файла
raco exe program.rkt

# Создание дистрибутива
raco distribute dist-folder program
```

## 5. Профилирование

```racket
(require profile)

;; Профилирование функции
(profile-thunk
 (lambda ()
   (my-slow-function)))
```

## 6. Unsafe операции (осторожно!)

```racket
(require racket/unsafe/ops)

;; Без проверки границ (опасно, но быстро)
(unsafe-vector-ref v i)
(unsafe-fx+ x y)
```

## 7. Оптимизации компилятора

```racket
;; Инлайнинг
(define-inline (fast-add x y)
  (+ x y))

;; Декларации
(begin-encourage-inline
  (define (my-func x) ...))
```

## 8. Сводная таблица скоростей

| Техника | Ускорение |
|---------|-----------|
| flonum вместо generic | 2-10x |
| Typed Racket | 2-5x |
| Futures (4 ядра) | 2-4x |
| Places (4 ядра) | 2-4x |
| unsafe ops | 1.5-3x |
| Компиляция (raco exe) | 1.2-2x |

## 9. Пример оптимизации Mandelbrot

```racket
#lang racket
(require racket/flonum racket/future)

(define (mandelbrot-fast size)
  (define max-iter 100)
  (for*/fold ([count 0]) 
             ([y (in-range size)]
              [x (in-range size)])
    (define cx (fl- (fl/ (fl* 3.5 (->fl x)) (->fl size)) 2.5))
    (define cy (fl- (fl/ (fl* 2.0 (->fl y)) (->fl size)) 1.0))
    (+ count
       (let loop ([zx 0.0] [zy 0.0] [i 0])
         (if (or (>= i max-iter) 
                 (fl> (fl+ (fl* zx zx) (fl* zy zy)) 4.0))
             i
             (loop (fl+ (fl- (fl* zx zx) (fl* zy zy)) cx)
                   (fl+ (fl* 2.0 (fl* zx zy)) cy)
                   (add1 i)))))))

;; Параллельная версия
(define (mandelbrot-parallel size)
  (define results
    (for/list ([chunk (in-range 4)])
      (future (lambda () (mandelbrot-chunk size chunk)))))
  (apply + (map touch results)))
```

## 10. Вывод

Racket может быть быстрым если:
1. Используешь правильные типы (flonum, fixnum, flvector)
2. Используешь Typed Racket для критичных частей
3. Используешь Futures для числовых вычислений
4. Используешь Places для сложных параллельных задач
5. Профилируешь и оптимизируешь узкие места
