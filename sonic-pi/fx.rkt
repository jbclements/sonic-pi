#lang racket

(require 
         "util.rkt")

(provide fx?
         fx
         fx-name
         fx-params
         fx-block
         set-fx-busses
         set-block
         control-fx)

;; an fx structure contains a name, arguments,
;; and a list of (sample or note or sleep)
(define-struct Fx (name params block) #:transparent)

;; rename 
(define fx? Fx?)
(define fx-name Fx-name)
(define fx-params Fx-params)
(define fx-block Fx-block)



;; default values for a sound effect
;; for now, the only options are reps, out_but and in_bus
;(: default-vals ParamAssoc)
(define default-vals
  (map
   (λ (x)
     (list (string->bytes/utf-8 (symbol->string (car x)))
           (cadr x)))
   '((reps 1)
     ;; in_bus and out_bus will always be set later
     ;; keeping it at 0 by default in case that
     ;; doesn't happen to make debugging easier
     (in_bus 0)
     (out_bus 0)
   )))

;; create sound fx given name, args, and block
;; since this is a variatic function with the variable length
;; args coming before the list, I'm going to include the list
;; of notes/samples/pisleep in the args and it will always be
;; the last thing in the list
;(: fx (String (U String Real (Listof (U sample note pisleep Fx))) * -> Fx))
(define (fx name . param-parts)
  (define other-params (group-params (drop-last param-parts)))
  (Fx (string->bytes/utf-8 (string-append "sonic-pi-fx_" name))
      (merge-field-list other-params default-vals)
      (last param-parts)))

;; set the an fx to contain a block
(define (set-block f  block)
  (Fx (fx-name f)
      (fx-params f)
       block))

;; set an fx's in_bus and out_bus
(define (set-fx-busses f in_bus out_bus)
  (Fx (fx-name f)
      (complete-field-list (group-params (list "in_bus" in_bus "out_bus" out_bus)) (Fx-params f))
      (fx-block f)))

;; control a sound fx's arguments
(define (control-fx f . param-parts)
  (Fx (Fx-name f)
      (complete-field-list (group-params param-parts) (Fx-params f))
      (Fx-block f)))


;; drop the last element in a list
(define (drop-last lst)
  (cond [(empty? lst) (error 'drop-last "empty list")]
        [(empty? (rest lst)) empty]
        [else (cons (first lst) (drop-last (rest lst)))]))


(module+ test
  (require rackunit)

  (check-equal? (fx "bitcrusher"
                    ;; this is allowed to be bogus
                    (list 1 2 3))
                (Fx #"sonic-pi-fx_bitcrusher"
                    '((#"reps" 1)
                      (#"in_bus" 0)
                      (#"out_bus" 0))
                    (list 1 2 3)))
  (check-equal? (set-fx-busses (fx "bitcrusher"
                                   (list 1 2 3))
                               12 10)
                (Fx #"sonic-pi-fx_bitcrusher"
                    '((#"reps" 1)
                      (#"in_bus" 12)
                      (#"out_bus" 10))
                    (list 1 2 3)))
  (check-equal? (control-fx (fx "bitcrusher" (list 1 2 3))
                            "in_bus" 12 "out_bus" 10)
                (Fx #"sonic-pi-fx_bitcrusher"
                    '((#"reps" 1)
                      (#"in_bus" 12)
                      (#"out_bus" 10))
                    (list 1 2 3))))
 