#lang racket

(require 
         "util.rkt"
         "sample.rkt"
         "allocator.rkt"
         "note.rkt")

(provide fx?
         fx
         fx-name
         fx-params
         fx-score
         set-fx-busses
         set-score
         control-fx)

;; an fx structure contains a name, arguments,
;; and a list of (sample or note or sleep)
;; need to define pisleep so fx can use it
(struct pisleep (duration) #:prefab)
(define-struct Fx (name params score) #:transparent)

;; rename 
(define fx? Fx?)
(define fx-name Fx-name)
(define fx-params Fx-params)
(define fx-score Fx-score)



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

;; set the list of notes/samples/pisleep
;; to a list of scores
(define (set-score f score)
  (Fx (fx-name f)
      (fx-params f)
      score))

;; set an fx's in_bus and out_bus
(define (set-fx-busses f in_bus out_bus)
  (Fx (fx-name f)
      (complete-field-list (group-params (list "in_bus" in_bus "out_bus" out_bus)) (Fx-params f))
      (fx-score f)))

;; control a sound fx's arguments
(define (control-fx f . param-parts)
  (Fx (Fx-name f)
      (complete-field-list (group-params param-parts) (Fx-params f))
      (Fx-score f)))


;; drop the last element in a list
(define (drop-last lst)
  (cond [(empty? lst) (error 'drop-last "empty list")]
        [(empty? (rest lst)) empty]
        [else (cons (first lst) (drop-last (rest lst)))]))
 