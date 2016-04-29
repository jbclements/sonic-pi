#lang typed/racket/base

(require racket/match)

(provide note
         note?)

;; parameters can be either byte-strings or real numbers
(define-type ParamVal (U Bytes Real))

(define-type ParamAssoc (Listof (List Bytes ParamVal)))

;; a note has a distinguished synth name, then other params
(define-type Note (Pairof Bytes ParamAssoc))

(define-predicate note? Note)

(: default-vals ParamAssoc)
(define default-vals
  (map
   (λ ([x : (List Symbol ParamVal)])
     (list (string->bytes/utf-8 (symbol->string (car x)))
           (cadr x)))
   '((note 60)
     (note_slide 0)
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

;; given an association list of specified fields, return
;; an alist mapping all required parameters to either default
;; or specified values.
;; NB: should we check for dups? Or for bogus fields?
(: complete-field-list (ParamAssoc -> ParamAssoc))
(define (complete-field-list alist)
  (for/list ([pr (in-list default-vals)])
    (match-define (list field-name default-val) pr)
    (match (assoc field-name alist)
      [(list _ new-val) (list field-name new-val)]
      [#f (list field-name default-val)])))

(: note (String Real (U String Real) * -> Note))
(define (note synth midi-pitch . param-parts)
  (define other-params (group-params param-parts))
  (cons (bytes-append #"sonic-pi-" (string->bytes/utf-8 synth))
        (complete-field-list (cons `(#"note" ,midi-pitch)
                                   other-params))))

;; turn an interleaved list of names and values into a
;; paramassoc (also changing strings into byte-strings as we go)
(: group-params ((Listof (U String Real)) -> ParamAssoc))
(define (group-params param-parts)
  (match param-parts
    [(cons (? string? name) (cons val rst))
     (cons (list (string->bytes/utf-8 name)
                 (cond [(string? val) (string->bytes/utf-8 val)]
                       [else val]))
           (group-params rst))]
    ['() '()]
    [(cons non-string (cons val rst))
     (error 'group-params "expected field name (string), got ~e"
            non-string)]
    [(cons leftover '())
     (error 'group-params "leftover value in params: ~e"
            leftover)]))

(module+ test
  (require typed/rackunit)

  (check-equal? (group-params '("x" 0.5 "y" 0.421 "z" "blue"))
                '((#"x" 0.5)
                  (#"y" 0.421)
                  (#"z" #"blue")))
  
  (check-equal? (complete-field-list '())
                default-vals)

  (check-equal? (complete-field-list '((#"amp" 0.5)))
                '((#"note" 60)
                  (#"note_slide" 0)
                  (#"note_slide_shape" 5)
                  (#"node_slide_curve" 0)
                  (#"amp" 0.5)
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
                  (#"out_bus" 12.0)))

  (check-equal? (complete-field-list '((#"amp" 0.5)
                                       (#"note_slide" 3)))
                '((#"note" 60)
                  (#"note_slide" 3)
                  (#"note_slide_shape" 5)
                  (#"node_slide_curve" 0)
                  (#"amp" 0.5)
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
                  (#"out_bus" 12.0)))

  (check-equal? (note "bronky" 39)
                '(#"sonic-pi-bronky"
                  (#"note" 39)
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
                  (#"out_bus" 12.0)))

  (check-equal? (note "bronky" 39 "decay" 0.9)
                '(#"sonic-pi-bronky"
                  (#"note" 39)
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
                  (#"out_bus" 12.0))))