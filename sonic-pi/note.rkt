#lang typed/racket/base

(require racket/match
         racket/list
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
(: note (String (U String Real) (U String Real) * -> Note))
(define (note synth midi-pitch . param-parts)
  (define other-params (group-params param-parts))
  (Note (bytes-append #"sonic-pi-" (string->bytes/utf-8 synth))
        (cons (list #"note" (get-pitch midi-pitch))
              (complete-field-list other-params default-vals))))

;; convert a given note in musical notation to a midi-pitch
;; if not already given
(: get-pitch ((U String Real) -> Real))
(define (get-pitch midi-pitch)
  (if (real? midi-pitch)
      midi-pitch
      (note->midi midi-pitch)))

;; convert a note in musical notation to a midi-pitch
;; I'm going off the table found at
;;   http://www.midimountain.com/midi/midi_note_numbers.html
(: note->midi (String -> Real))
(define (note->midi n)
  (let ([n2 (regexp-match #rx"[a-gA-G][sSbBfF]?[0-9]?"
                          n)])
    #;(printf "Note:~v\nAcc:~v\nOct:~v\n\n"
            (note-offset n)
            (accidental n)
            (octave n))
    (+ (note-offset n)
       (accidental n)
       (* 12 (octave n)))))

;; converts the note string to it's offset in the midi table
(: note-offset (String -> Real))
(define (note-offset n)
  (let ([note (regexp-match #rx"[a-gA-G]" n)])
      (if note
          (match (string-downcase (car note))
            ["a" 9]
            ["b" 11]
            ["c" 0]
            ["d" 2]
            ["e" 4]
            ["f" 5]
            ["g" 7])
          (error 'note "invalid note representation"))))

;; finds the offset in the table when sharp or flat
;; is specified
(: accidental (String -> Real))
(define (accidental n)
  (let ([acc (regexp-match #rx"[sSbBfF]"
                           ;; need to ignore first
                           ;; letter because notes
                           ;; can be b or f
                           (substring n 1 (string-length n)))])
    (if acc
        (match (string-downcase (car acc))
          ["s" 1]
          ["b" -1]
          ["f" -1])
        ;; we could still get a bad acc here
        (if (regexp-match #rx"[a-zA-Z]"
                          (substring n 1 (string-length n)))
            (error "bad accidental")
            0))))

;; gets the octave for a note. the default is 4
(: octave (String -> Real))
(define (octave n)
  (let ([num (regexp-match #rx"[0-9]" n)])
    (if num
        (cast (string->number (car num)) Real)
        4)))


;; change a note's parameters to specified new ones
(: control-note (Note (U String Real) * -> Note))
(define (control-note n . param-parts)
  (Note (Note-name n)
        (complete-field-list (group-params param-parts) (Note-params n))))

(module+ test
  (require typed/rackunit)
  (check-equal? (note->midi "C") 48)
  (check-equal? (note->midi "C0") 0)
  (check-equal? (note->midi "Fs") 54)
  (check-equal? (note->midi "gb") 54)
  (check-equal? (note->midi "bb3") 46)
  (check-equal? (note->midi "df") 49)
  (check-equal? (note->midi "A9") 117)
  (check-equal? (note->midi "es6") 77)
  (check-exn exn:fail? (λ () (note->midi "hs")))
  (check-exn exn:fail? (λ () (note->midi "ar")))
  
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
                  (#"out_bus" 12.0))))
  (check-equal? (note "bronky" "C2" "decay" 0.9)
                (Note #"sonic-pi-bronky"
                  '((#"note" 24)
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
  (check-equal? (control-note (note "bronky" "AF3") "decay" 0.9)
                (Note #"sonic-pi-bronky"
                  '((#"note" 44)
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
  )