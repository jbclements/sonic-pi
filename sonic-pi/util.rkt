#lang typed/racket/base

(require racket/match)
(provide complete-field-list
         group-params
         ParamVal
         ParamField
         ParamAssoc)


;; for now this is just a file for functions that
;; are used by multiple modules


;; parameters can be either byte-strings or real numbers
(define-type ParamVal (U Bytes Real))
(define-type ParamField (List Bytes ParamVal))
(define-type ParamAssoc (Listof ParamField))


;; given an association list of specified fields, return
;; an alist mapping all required parameters to either default
;; or specified values.
;; NB: should we check for dups? Or for bogus fields?
(: complete-field-list (ParamAssoc ParamAssoc -> ParamAssoc))
(define (complete-field-list alist thelist)
  (for/list ([pr (in-list thelist)])
    (match-define (list field-name thelist) pr)
    (match (assoc field-name alist)
      [(list _ new-val) (list field-name new-val)]
      [#f (list field-name thelist)])))

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

