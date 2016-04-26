#lang racket

(require sonic-pi/scsynth/scsynth-abstraction
         2htdp/image
         2htdp/universe)

(define ctxt (startup))

(define job (start-job ctxt))

(define table
  '(("1" (0 4 7 11))
    ("4" (0 4 5 9))
    ("5" (-1 2 5 7 11))
    ("6" (0 4 7 9))
    ("8" (0 4 7 12))
    ("0" ())))

(define chMT '())

(define (pick-from ch)
  (list-ref ch (random (length ch))))

;; a world is a list of note-number-offsets

;; play a random note from the list, don't change the world:
(define (rand-note w)
  (cond [(not (empty? w))
         (define note (pick-from w))
         (play-note job
                    (make-note #"beep"
                               #:attack 0.0
                               #:release 0.5
                               #:note (+ 80 note))
                    0.0)
         
         (define note2 (pick-from w))
         (play-note job
                    (make-note #"beep"
                               #:attack 0.0
                               #:release 0.5
                               #:note (+ 80 note2))
                    0.0)]
        [else 'do-nothing])
  w)

;; given a world and a key, change the world to the list associated
;; with the key in the table if defined.
(define (handle-key w k)
  (match (dict-ref table k #f)
    [#f w]
    [(list success) success]))


(big-bang '(0 4 7 11)
          [to-draw (λ (w) (rectangle 100 100 "solid" "blue"))]
          [on-tick rand-note 0.0625]
          [on-key handle-key])
