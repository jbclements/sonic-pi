#lang racket

(require "scsynth-communication.rkt")

;; this plays the key 
(define my-note
  (let ()
    (define id (create-synth "sin-inst" #t))
    (send-msg (n-set1 id "freq" freq))
    (note id freq)))
(sleep 1)
;; stop playing note
(note-off my-note)