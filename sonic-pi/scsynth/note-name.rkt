#lang typed/racket

;; convert note names to note numbers.
(provide note-name->num)

(: note-name->num (Symbol -> Natural))
(define (note-name->num name)
  (match (regexp-match #px"^([ABCDEFG])([bs])?([012345678])?$" (symbol->string name))
    [(list _ note-name flat-sharp-str octave-str)
     (: octave Natural)
     (define octave
       (match octave-str
         [#f 60]
         ;; ensure-nonneg to make type checker happy
         [else (ensure-nonneg
                (+ 12 (* 12
                         (round
                          (inexact->exact
                           (match (string->number
                                   (ensure-string octave-str))
                             [(? real? r) r]
                             [else (error (error
                                           'note-name->num
                                           "internal error 20150708"))]))))))]))
     (: pitch-class Natural)
     (define pitch-class
       (match note-name
         ["C" 0]
         ["D" 2]
         ["E" 4]
         ["F" 5]
         ["G" 7]
         ["A" 9]
         ["B" 11]
         [other (error 'note-nome-num
                       "internal-error 201507072248")]))
     (: flat-sharp Integer)
     (define flat-sharp
       (match flat-sharp-str
         [#f 0]
         ["b" -1]
         ["s" 1]
         [other (error 'note-name-num
                       "internal error 201507072247")]))
     
     (define result (+ octave pitch-class flat-sharp))
     ;; ensure-nonneg to keep type checker happy
     (ensure-nonneg result)]
    [#f (raise-argument-error 'note-name->num
                              "symbol denoting note name"
                              0 name)]))

(: ensure-nonneg (Integer -> Natural))
(define (ensure-nonneg i)
  (cond [(< i 0) (raise-argument-error 'ensure-nonneg
                                       "nonnegative number" 0 i)]
        [else i]))

(: ensure-string ((U False String) -> String))
(define (ensure-string s)
  (cond [(false? s) (raise-argument-error 'ensure-str
                                       "string" 0 s)]
        [else s]))

(module* test racket
  (require rackunit
           (submod ".."))

  (check-exn #px"expected: symbol denoting note name"
             (λ () (note-name->num 'Cz)))

  (check-equal? (note-name->num 'C) 60)
  (check-equal? (note-name->num 'Eb) 63)
  (check-equal? (note-name->num 'B3) 59)
  (check-equal? (note-name->num 'Fs2) 42))