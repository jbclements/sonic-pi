#lang racket

(provide (all-defined-out))

;; choose from a variable amount of arguments
(define (choose . args)
  (if (andmap list? args)
      (choose-list (flatten args))
      (list-ref args (random (length args)))))
;; choose from a list of arguments
(define (choose-list l)
  (list-ref l (random (length l))))

;; random in a range.
(define (rrand min max)
  (+ min (* (- max min) (random))))

;; random integer in any range, inclusive
(define (rrand_i min max)
  (if (> min 0)
      (random min max)
      (let ([diff (- 1 min)])
        (- (random (+ diff min)
                   (add1 (+ diff max)))
           diff))))