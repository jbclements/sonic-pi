#lang racket/base

(require (for-syntax syntax/parse
                     racket/base)
         racket/contract
         racket/match)

;; 2016-04-29: GETTING RID OF THIS FILE

;; this file is just for the DRY macro magic on notes

;; NB: why not a struct? I think its just that the association list
;; made it easier to abstract over providing defaults, but that seems
;; pretty weak... then again, this seems unlikely to become a bottleneck.

;; a note is a string representing a synth name and a byte-string association-list
;; representing the values of various fields.
(define note? (cons/c bytes? (listof (list/c bytes? any/c))))

(provide note?
         make-note)



;; we want to define a note as a struct (actually just an association list)
;; with a bunch of default values for various fields.
(define-syntax (note-struct-definer stx)
  (syntax-parse stx
    [(_ doc-text
        maker-name (req-field ...) (field-name default-value) ...)
     (define field-names (syntax->list #'(field-name ...)))
     (define kwds (map (lambda (s)
                         (string->keyword (symbol->string (syntax-e s))))
                       field-names))
     (define opts (syntax->list #'((field-name default-value) ...)))
     (with-syntax ([(arg-piece ...) (apply append (map list kwds opts))]
                   [(byte-string ...) (map (lambda (id)
                                             (string->bytes/utf-8 (symbol->string
                                                                   (syntax-e id))))
                                           field-names)]
                   [(kwd ...) kwds])
       #`(begin
           (define (maker-name req-field ... arg-piece ...)
             
             (list req-field ... (list byte-string field-name) ...))
           ;; this value can be pasted into the docs...
           ;; a bit nasty, but better than creating it by hand.
           (define doc-text
             (list
              (list (quote kwd) (quote field-name) 'number? default-value) ...))))]))

(note-struct-definer
 doc-text
 make-note
 (synth)
 (note 60)
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
 (out_bus 12.0))

(module+ test
  (require rackunit)

  (display doc-text)
  (newline)
  
  (check-equal?
   (make-note #"beep")
   '(#"beep"
     (#"note" 60)
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

  (check-equal?
   (make-note #"beep" #:pan -1 #:attack 4)
   '(#"beep"
     (#"note" 60)
     (#"note_slide" 0)
     (#"note_slide_shape" 5)
     (#"node_slide_curve" 0)
     (#"amp" 1)
     (#"amp_slide" 0)
     (#"amp_slide_shape" 5)
     (#"pan" -1)
     (#"pan_slide" 0)
     (#"pan_slide_shape" 5)
     (#"pan_slide_curve" 0)
     (#"attack" 4)
     (#"decay" 0)
     (#"sustain" 0)
     (#"release" 1)
     (#"attack_level" 1)
     (#"sustain_level" 1)
     (#"env_curve" 2)
     (#"out_bus" 12.0))))
