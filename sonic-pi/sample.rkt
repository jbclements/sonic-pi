#lang typed/racket/base

(require racket/match
         racket/path
         racket/list)

(provide sample
         Sample
         Sample-name
         Sample-path
         Sample-params
         sample?
         control-sample)

;; parameters can be either byte-strings or real numbers
(define-type ParamVal (U Bytes Real))

(define-type ParamField (List Bytes ParamVal))
(define-type ParamAssoc (Listof ParamField))

;; a sample has a distinguished name,
;; an absolute path, then other params
(define-struct Sample ([name : Bytes] [path : Bytes] [params : ParamAssoc]) #:transparent)
#;(define-type Sample (Pairof (Pairof Bytes Bytes) ParamAssoc))

;; the problem with this is that a note is of the same type
;; so we need a way to distinguish notes and samples
#;(define-predicate sample? Sample)

(define sample? Sample?)

;; default values for a sample
;; commented out values either need to be calculated,
;; have unknown defaults. they will likely be used in the
;; future.
(: default-vals ParamAssoc)
(define default-vals
  (map
   (λ ([x : (List Symbol ParamVal)])
     (list (string->bytes/utf-8 (symbol->string (car x)))
           (cadr x)))
   '((buf 0)
     (amp 1)
     (amp_slide 0)
     (amp_slide_shape 1)
     (amp_slide_curve 0)
     (attack 0.0)
     (decay 0)
     (sustain -1)
     (release 0.0)
     (attack_level 1)
     (decay_level -1)
     (sustain_level 1)
     (env_curve 1)
     (pan 0)
     (pan_slide 0)
     (pan_slide_shape 1)
     (pan_slide_curve 0)
     (lpf -1)
     (lpf_slide 0)
     (lpf_slide_shape 1)
     (lpf_slide_curve 0)
     (hpf -1)
     (hpf_slide 0)
     (hpf_slide_shape 1)
     (hpf_slide_curve 0)
     (rate 1)
     (out_bus 12)
   )))

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

;; for now a sample can only be identified by it's name
;; TODO: resolve_specific_sampler
(: sample (String (U String Real) * -> Sample))
(define (sample name . param-parts)
  (define other-params (group-params param-parts))
  (Sample (string->bytes/utf-8 (string-append "sonic-pi-" "basic_stereo_player"))
          (string->bytes/utf-8 (resolve-path name))
          (complete-field-list other-params)))

;; resolve the path for a sample
;; for now the path is relative for Unix
(: resolve-path (String -> String))
(define (resolve-path path)
  (if (complete-path? path)
      path
      (path->string
       (build-path (current-directory)
                   (string-append "samples/" path ".flac")))))

;; control a sample's arguments
(: control-sample (Sample (U String Real) * -> Sample))
(define (control-sample s . param-parts)
  (Sample (Sample-name s)
          (Sample-path s)
          (set-params (Sample-params s) (group-params param-parts))))

;; sets new parameters for a sample
(: set-params (ParamAssoc ParamAssoc -> ParamAssoc))
(define (set-params orig new)
  (for/list ([pr (in-list orig)])
    (match-define (list field-name default-val) pr)
    (match (assoc field-name new)
      [(list _ new-val) (list field-name new-val)]
      [#f (list field-name default-val)])))

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
  (check-exn #px"expected field name"
             (λ () (group-params '("abc" 0.5 78 "def"))))
  (check-exn #px"leftover value"
             (λ () (group-params '("abc" 0.5 "def" 78 "ghi"))))

  (check-equal? (group-params '("x" 0.5 "y" 0.421 "z" "blue"))
                '((#"x" 0.5)
                  (#"y" 0.421)
                  (#"z" #"blue")))
  
  (check-equal? (complete-field-list '())
                default-vals)

  (check-equal? (complete-field-list '((#"amp" 0.5)))
                '((#"buf" 0)
                  (#"amp" 0.5)
                  (#"amp_slide" 0)
                  (#"amp_slide_shape" 1)
                  (#"amp_slide_curve" 0)
                  (#"attack" 0.0)
                  (#"decay" 0)
                  (#"sustain" -1)
                  (#"release" 0.0)
                  (#"attack_level" 1)
                  (#"decay_level" -1)
                  (#"sustain_level" 1)
                  (#"env_curve" 1)
                  (#"pan" 0)
                  (#"pan_slide" 0)
                  (#"pan_slide_shape" 1)
                  (#"pan_slide_curve" 0)
                  (#"lpf" -1)
                  (#"lpf_slide" 0)
                  (#"lpf_slide_shape" 1)
                  (#"lpf_slide_curve" 0)
                  (#"hpf" -1)
                  (#"hpf_slide" 0)
                  (#"hpf_slide_shape" 1)
                  (#"hpf_slide_curve" 0)
                  (#"rate" 1)
                  (#"out_bus" 12)
                  ))

  (check-equal? (complete-field-list '((#"amp" 0.5)
                                       (#"pan" 1)))
                '((#"buf" 0)
                  (#"amp" 0.5)
                  (#"amp_slide" 0)
                  (#"amp_slide_shape" 1)
                  (#"amp_slide_curve" 0)
                  (#"attack" 0.0)
                  (#"decay" 0)
                  (#"sustain" -1)
                  (#"release" 0.0)
                  (#"attack_level" 1)
                  (#"decay_level" -1)
                  (#"sustain_level" 1)
                  (#"env_curve" 1)
                  (#"pan" 1)
                  (#"pan_slide" 0)
                  (#"pan_slide_shape" 1)
                  (#"pan_slide_curve" 0)
                  (#"lpf" -1)
                  (#"lpf_slide" 0)
                  (#"lpf_slide_shape" 1)
                  (#"lpf_slide_curve" 0)
                  (#"hpf" -1)
                  (#"hpf_slide" 0)
                  (#"hpf_slide_shape" 1)
                  (#"hpf_slide_curve" 0)
                  (#"rate" 1)
                  (#"out_bus" 12)
                  ))

  (check-equal? (sample "ambi_choir")
                (Sample #"sonic-pi-basic_stereo_player"
                  (string->bytes/utf-8
                   (path->string
                    (build-path (current-directory)
                                (string-append "samples/" "ambi_choir" ".flac"))))
                  '((#"buf" 0)
                  (#"amp" 1)
                  (#"amp_slide" 0)
                  (#"amp_slide_shape" 1)
                  (#"amp_slide_curve" 0)
                  (#"attack" 0.0)
                  (#"decay" 0)
                  (#"sustain" -1)
                  (#"release" 0.0)
                  (#"attack_level" 1)
                  (#"decay_level" -1)
                  (#"sustain_level" 1)
                  (#"env_curve" 1)
                  (#"pan" 0)
                  (#"pan_slide" 0)
                  (#"pan_slide_shape" 1)
                  (#"pan_slide_curve" 0)
                  (#"lpf" -1)
                  (#"lpf_slide" 0)
                  (#"lpf_slide_shape" 1)
                  (#"lpf_slide_curve" 0)
                  (#"hpf" -1)
                  (#"hpf_slide" 0)
                  (#"hpf_slide_shape" 1)
                  (#"hpf_slide_curve" 0)
                  (#"rate" 1)
                  (#"out_bus" 12)
                  )))

  (check-equal? (sample "ambi_choir" "attack" 1)
                (Sample #"sonic-pi-basic_stereo_player"
                  (string->bytes/utf-8
                   (path->string
                    (build-path (current-directory)
                                (string-append "samples/" "ambi_choir" ".flac"))))
                  '((#"buf" 0)
                  (#"amp" 1)
                  (#"amp_slide" 0)
                  (#"amp_slide_shape" 1)
                  (#"amp_slide_curve" 0)
                  (#"attack" 1)
                  (#"decay" 0)
                  (#"sustain" -1)
                  (#"release" 0.0)
                  (#"attack_level" 1)
                  (#"decay_level" -1)
                  (#"sustain_level" 1)
                  (#"env_curve" 1)
                  (#"pan" 0)
                  (#"pan_slide" 0)
                  (#"pan_slide_shape" 1)
                  (#"pan_slide_curve" 0)
                  (#"lpf" -1)
                  (#"lpf_slide" 0)
                  (#"lpf_slide_shape" 1)
                  (#"lpf_slide_curve" 0)
                  (#"hpf" -1)
                  (#"hpf_slide" 0)
                  (#"hpf_slide_shape" 1)
                  (#"hpf_slide_curve" 0)
                  (#"rate" 1)
                  (#"out_bus" 12)
                  )))
  (check-equal? (control-sample (sample "ambi_choir")
                                "buf" 2 "out_bus" 10)
                (Sample #"sonic-pi-basic_stereo_player"
                  (string->bytes/utf-8
                   (path->string
                    (build-path (current-directory)
                                (string-append "samples/" "ambi_choir" ".flac"))))
                  '((#"buf" 2)
                  (#"amp" 1)
                  (#"amp_slide" 0)
                  (#"amp_slide_shape" 1)
                  (#"amp_slide_curve" 0)
                  (#"attack" 0.0)
                  (#"decay" 0)
                  (#"sustain" -1)
                  (#"release" 0.0)
                  (#"attack_level" 1)
                  (#"decay_level" -1)
                  (#"sustain_level" 1)
                  (#"env_curve" 1)
                  (#"pan" 0)
                  (#"pan_slide" 0)
                  (#"pan_slide_shape" 1)
                  (#"pan_slide_curve" 0)
                  (#"lpf" -1)
                  (#"lpf_slide" 0)
                  (#"lpf_slide_shape" 1)
                  (#"lpf_slide_curve" 0)
                  (#"hpf" -1)
                  (#"hpf_slide" 0)
                  (#"hpf_slide_shape" 1)
                  (#"hpf_slide_curve" 0)
                  (#"rate" 1)
                  (#"out_bus" 10)
                  ))))