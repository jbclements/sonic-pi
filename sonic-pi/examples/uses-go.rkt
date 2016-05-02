#lang racket

(require sonic-pi/go)

(current-synth "prophet")

(define (zz offset octave)
  (+ 12 (* 12 octave) offset))

(define (triad n)
  (list n (+ n 4) (+ n 7)))
(define (minor-triad n)
  (list n (+ n 3) (+ n 7)))

(define Bf2 (zz 10 2))
(define g2 (zz 7 2))
(define Ft (triad (zz 5 3)))
(define Cmt (minor-triad (zz 0 3)))
(define Bft (triad Bf2))
(define Gmt (minor-triad g2))
(define Ef4 (zz 3 4))
(define F4 (zz 5 4))
(define Bf4 (zz 10 4))
(define C5 (zz 0 5))
(define A4 (zz 9 4))

(define s 0.5)

(define chords (list Bft Gmt Gmt Ft Ft Ft Bft Cmt Bft))

(for ([c (in-list chords)]
      [i (in-naturals)])
  (map (λ (n) (note-at n (* i s))) c))

(define (q n)
  (list 1 n))
(define (e n)
  (list 0.5 n))
(define (dq n)
  (list 1.5 n))
(define (h n)
  (list 2 n))
(define qr
  (list 1 #f))

(define melody
  (list (q F4) (dq Bf4) (e C5)
        (h A4) qr
        (q Bf4) (q Ef4) (q Ef4)))

(current-synth "supersaw")

(let loop ([melody melody]
           [t 0])
  (cond [(empty? melody) 'done]
        [else
         (match-define (list len nn) (first melody))
         (when nn (note-at nn t))
         (loop (rest melody) (+ t (* s len)))]))

