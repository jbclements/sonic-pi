#lang typed/racket/base

(require racket/match
         racket/path
         racket/list)

(provide sample
         sample-name
         sample-path
         sample-params
         sample?
         control-sample
         resolve-specific-sampler)

;; parameters can be either byte-strings or real numbers
(define-type ParamVal (U Bytes Real))
(define-type ParamField (List Bytes ParamVal))
(define-type ParamAssoc (Listof ParamField))

;; a sample has a distinguished name,
;; an absolute path, then other params
(define-struct Sample ([name : Bytes] [path : Bytes] [params : ParamAssoc]) #:transparent)

;; rename 
(define sample? Sample?)
(define sample-name Sample-name)
(define sample-path Sample-path)
(define sample-params Sample-params)

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
(: complete-field-list (ParamAssoc  ParamAssoc -> ParamAssoc))
(define (complete-field-list alist thelist)
  (for/list ([pr (in-list thelist)])
    (match-define (list field-name thelist) pr)
    (match (assoc field-name alist)
      [(list _ new-val) (list field-name new-val)]
      [#f (list field-name thelist)])))

;; create a sample given only the name/path and arguments
;; default to basic_stereo_player but set the correct one
;; once it is loaded
(: sample (String (U String Real) * -> Sample))
(define (sample name . param-parts)
  (define other-params (group-params param-parts))
  (Sample #"sonic-pi-basic_stereo_player"
          (string->bytes/utf-8 (resolve-path name))
          (complete-field-list other-params default-vals)))

;; resolve the path for a sample
;; for now the path is relative for Unix
(: resolve-path (String -> String))
(define (resolve-path path)
  (if (complete-path? path)
      path
      (path->string
       (build-path (current-directory)
                   (string-append "samples/" path ".flac")))))

;; resolve the specific sampler that is used
;; interesting note: sonic-pi handles the sampler
;; based on basic or complex arguments but, since
;; we pass all the arguments, I'm going to use
;; the complex sampler for all of them. bad choice?
(: resolve-specific-sampler (Sample (Listof Real) -> Sample))
(define (resolve-specific-sampler s b-info)
  (Sample (num-chans->sampler (third b-info))
          (sample-path s)
          (sample-params s)))

;; gets the appropriate mixer for the number of channels
(define (num-chans->sampler num-chans)
  (match num-chans
    [1 #"sonic-pi-mono_player"]
    [2 #"sonic-pi-stereo_player"]))

;; control a sample's arguments
(: control-sample (Sample (U String Real) * -> Sample))
(define (control-sample s . param-parts)
  (Sample (Sample-name s)
          (Sample-path s)
          (complete-field-list (group-params param-parts) (Sample-params s))))

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
  
  (check-equal? (complete-field-list '() default-vals)
                default-vals)

  (check-equal? (complete-field-list '((#"amp" 0.5)) default-vals)
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
                  (#"out_bus" 12)))

  (check-equal? (complete-field-list '((#"amp" 0.5)
                                       (#"pan" 1))
                                     default-vals)
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