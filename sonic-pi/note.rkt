#lang typed/racket/base

(require racket/match
         "util.rkt")

(provide note
         note?
         note-name
         note-params
         control-note)


;; a note has a distinguished synth name, then other params
;(define-type Note (Pairof Bytes ParamAssoc))
(define-struct Note ([name : Bytes] [params : ParamAssoc]) #:transparent)

;; redefine everything with lowercase
(define note? Note?)
(define note-name Note-name)
(define note-params Note-params)

(: default-vals ParamAssoc)
(define default-vals
  (map
   (λ ([x : (List Symbol ParamVal)])
     (list (string->bytes/utf-8 (symbol->string (car x)))
           (cadr x)))
   '((note_slide 0)
     (note_slide_shape 5)
     (node_slide_curve 0)
     (amp 1)
     (amp_slide 0)
     (amp_slide_shape 5)
     (pan 0)
     (pan_slide 0)
     (pan_slide_shape 5)
     (pan_slide_curve 0)
     (attack 0)
     (decay 0)
     (sustain 0)
     (release 1)
     (attack_level 1)
     (sustain_level 1)
     (env_curve 2)
     (out_bus 12.0))))

;; create a note given a synth name, pitch number, and optional parameters
(: note (String Real (U String Real) * -> Note))
(define (note synth midi-pitch . param-parts)
  (define other-params (group-params param-parts))
  (Note (bytes-append #"sonic-pi-" (string->bytes/utf-8 synth))
        (cons (list #"note" (ann midi-pitch ParamVal))
              (complete-field-list other-params default-vals))))

;; control a note's parameters
(: control-note (Note (U String Real) * -> Note))
(define (control-note n . param-parts)
  (Note (Note-name n)
        (complete-field-list (group-params param-parts) (Note-params n))))

(module+ test
  (require typed/rackunit)

  (check-equal? (note "bronky" 39)
                (Note #"sonic-pi-bronky"
                  '((#"note" 39)
                  (#"note_slide" 0)
                  (#"note_slide_shape" 5)
                  (#"node_slide_curve" 0)
                  (#"amp" 1)
                  (#"amp_slide" 0)
                  (#"amp_slide_shape" 5)
                  (#"pan" 0)
                  (#"pan_slide" 0)
                  (#"pan_slide_shape" 5)
                  (#"pan_slide_curve" 0)
                  (#"attack" 0)
                  (#"decay" 0)
                  (#"sustain" 0)
                  (#"release" 1)
                  (#"attack_level" 1)
                  (#"sustain_level" 1)
                  (#"env_curve" 2)
                  (#"out_bus" 12.0))))

  (check-equal? (note "bronky" 39 "decay" 0.9)
                (Note #"sonic-pi-bronky"
                  '((#"note" 39)
                  (#"note_slide" 0)
                  (#"note_slide_shape" 5)
                  (#"node_slide_curve" 0)
                  (#"amp" 1)
                  (#"amp_slide" 0)
                  (#"amp_slide_shape" 5)
                  (#"pan" 0)
                  (#"pan_slide" 0)
                  (#"pan_slide_shape" 5)
                  (#"pan_slide_curve" 0)
                  (#"attack" 0)
                  (#"decay" 0.9)
                  (#"sustain" 0)
                  (#"release" 1)
                  (#"attack_level" 1)
                  (#"sustain_level" 1)
                  (#"env_curve" 2)
                  (#"out_bus" 12.0)))))