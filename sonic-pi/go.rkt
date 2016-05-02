#lang racket/base

;; a simplified interface (imperative as heck)

(require "scsynth/scsynth-abstraction.rkt"
         (prefix-in n: "note.rkt"))

(provide note
         note-at
         current-synth)

(define ctxt (startup))
(define job-ctxt (start-job ctxt))

(define current-synth (make-parameter "beep"))

(define (note num . args)
  (play-note job-ctxt
             (apply n:note (current-synth) num args)
             (current-inexact-milliseconds)))

(define (note-at num time . args)
  (play-note job-ctxt
             (apply n:note (current-synth) num args)
             (+ (* 1000 time)
                (current-inexact-milliseconds))))



(module+ main

  (note 80 "attack" 0.5))