#lang typed/racket/base

;; for now this is just a file for functions that
;; are used by multiple modules

(require racket/match
         racket/list)
(require/typed setup/dirs
               [find-user-pkgs-dir (-> Path)])

(provide complete-field-list
         merge-field-list
         group-params
         root-dir
         ParamVal
         ParamField
         ParamAssoc)

;; root directory for sonic-pi located in [pkgs]/sonic-pi/sonic-pi
(: root-dir (-> Path))
(define (root-dir)
  (build-path (find-user-pkgs-dir)
              "sonic-pi"
              "sonic-pi"))

;; parameters can be either byte-strings or real numbers
(define-type ParamVal (U Bytes Real))
(define-type ParamField (List Bytes ParamVal))
(define-type ParamAssoc (Listof ParamField))


;; given an association list of specified fields and an
;; association list of default fields, return
;; an alist mapping all required parameters to either default
;; or specified values.
;; NB: should we check for dups? Or for bogus fields?
(: complete-field-list (ParamAssoc ParamAssoc -> ParamAssoc))
(define (complete-field-list alist defaultlist)
  (for/list ([pr (in-list defaultlist)])
    (match-define (list field-name defaultlist) pr)
    (match (assoc field-name alist)
      [(list _ new-val) (list field-name new-val)]
      [#f (list field-name defaultlist)])))

;; given an association list of specified fields and
;; specified defaults, return an alist that is the
;; merging of the two, taking the specified field values
;; over the defaults
;; NB: this allows bogus arguments but they'll just be ignored by SuperCollider
;; NB: this is terribly slow
(: merge-field-list (ParamAssoc ParamAssoc -> ParamAssoc))
(define (merge-field-list alist defaultlist)
  (append (complete-field-list alist defaultlist)
          (remove-dups alist defaultlist)))

;; removes any of the ParamVals from alist
;; that are already in thelist
(: remove-dups (ParamAssoc ParamAssoc -> ParamAssoc))
(define (remove-dups alist thelist)
  (cond
    [(empty? alist) empty]
    [else (if (contains-param thelist (first alist))
              (remove-dups (rest alist) thelist)
              (cons (first alist)
                    (remove-dups (rest alist) thelist)))]))

;; determines if a list contains a param field-name
(: contains-param (ParamAssoc ParamField -> Boolean))
(define (contains-param thelist thefield)
  (for/or ([pr (in-list thelist)])
    (match-define (list field-name thelist) pr)
    (equal? field-name (first thefield))))

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
  ;; use note default values to test with
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
                '((#"note_slide" 0)
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
                                       (#"note_slide" 3))
                                     default-vals)
                '((#"note_slide" 3)
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
  (check-equal? (merge-field-list '((#"foo" 10)
                                    (#"out_bus" 14.0))
                                  default-vals)
                '((#"note_slide" 0)
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
                  (#"out_bus" 14.0)
                  (#"foo" 10)))
  )

