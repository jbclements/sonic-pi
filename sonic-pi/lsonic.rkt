#lang racket

(require "scsynth/scsynth-abstraction.rkt")

(provide (except-out (all-from-out racket) sleep #%module-begin)
         (rename-out [my-module-begin #%module-begin]
                     [psleep sleep])
         synth)

(require
  "scsynth/scsynth-abstraction.rkt"
  (for-syntax syntax/parse)
  rackunit)

;; all kinds of interesting interface questions here. Implicit sequence
;; wrapped around the whole thing? Implicit parallelism for loops next to
;; each other? How does Sonic Pi handle this?

;; ANSWER: actually, sonic pi adds nearly nothing, just falls through to
;; existing ruby model. there's a notion of threads, and you have to
;; synchronize manually in many cases.

;; very nice how sonic pi saves state, reopens desktop

;; optional arguments seem very important, here.

;; sonic pi uses ADSR from the get-go.

;; a uscore is a representation of a user's program
;; a uscore is a list of uevents
;; a uevent is one of
;; - (pisleep ...), representing the passage of time, or
;; - a note, representing a note to be played at the current time

;; a score is (stream/c event) WITH NON-DECREASING TIMES
;; an event is (list/c time-in-msec synth-note)
;; where time-in-msec is relative to (current-inexact-milliseconds)

;; given a job-ctxt and an event, queue the note
(define (queue-event job-ctxt evt)
  (play-note job-ctxt (second evt) (first evt)))

;; given a job-ctxt and a list of events, queue them all.
(define (queue-events job-ctxt score)
  (cond [(stream-empty? score)
         (void)]
        [else
         ;; check for job death....
         (define e (stream-first score))
         (define dt (current-lead e))
         (cond [(< dt MAX-QUEUE-LEAD)
                (queue-event job-ctxt e)
                (queue-events (stream-rest score))]
               [else ;; too early to play
                (define sleep-msec
                  ;; turns out sleep tends to oversleep...
                  (max MIN-SLEEP-MSEC
                       (- dt EXPECTED-MAX-SLEEP-LAG QUEUE-LEAD)))
                (sleep (/ sleep-msec 1000))
                ;; could keep statistics here...
                (queue-events job-ctxt score)])]))


;; don't want to busy-wait... don't sleep less than this many msec:
(define MIN-SLEEP-MSEC 10)
;; ideal lead time
(define QUEUE-LEAD 200)
(define EXPECTED-MAX-SLEEP-LAG 20)

;; how long until the event is supposed to happen?
(define (current-lead event)
  (define t (current-inexact-milliseconds))
  (match-define (list note-time note) event)
  (- note-time t))

;; lead time in milliseconds. Don't queue a note more than this
;; far in advance:
(define MAX-QUEUE-LEAD 250)

(define MSEC-PER-SEC 1000)

;; DANGER: loop with only sleep can lead to spaz-out...
;; given a user score and a virtual time, produce a score associating
;; a time with each note.
(define (uscore->score uscore vtime)
  (cond [(empty? uscore) empty-stream]
        [else (cond [(pisleep? (first uscore))
                     (uscore->score (rest uscore)
                                    (+ vtime (* MSEC-PER-SEC
                                                (pisleep-duration
                                                 (first uscore)))))]
                    [(note? (first uscore))
                     (stream-cons (list vtime (first uscore))
                                  (uscore->score (rest uscore)
                                                 vtime))]
                    [else (raise-argument-error 'uscore->score
                                                "list of notes and sleeps"
                                                0 uscore vtime)])]))


;; can't actually test this?
(define START-MSEC-GAP 500)

;; play a user-score
(define (play job-ctxt uscore)
  (queue-events job-ctxt
                (uscore->score uscore
                               (+ (current-inexact-milliseconds)
                                  START-MSEC-GAP))))

(struct pisleep (duration) #:prefab)

(define (psleep t)
  (pisleep t))

(define synth make-note)

(define-syntax (my-module-begin stx)
  (syntax-parse stx
    [(_ e:expr ...)
     #'(#%module-begin
        (define ctxt (startup))
        (define job-ctxt (start-job ctxt))
        (with-handlers
           ([exn:fail? (lambda (exn)
                         (printf "ending job due to error...\n")
                         (end-job job-ctxt)
                         (raise exn))])
           (play job-ctxt (list e ...))
          )
        (printf "waiting an arbitrary and hard-coded 15 seconds...\n")
        (sleep 15)
        (printf "ending job...\n")
        (end-job job-ctxt)
        (printf "finished.\n"))]))

#;(go
 (synth #"beep" #:note 60  #:release 0.5)
 (synth #"prophet" #:note 72 #:attack 4 #:release 2 #:amp 0.5)
 (psleep 0.5)
 (synth #"prophet" #:note 74 #:attack 4 #:release 2 #:amp 0.5)
 (psleep 0.5)
 (synth #"prophet" #:note 79 #:attack 4 #:release 2 #:amp 0.5)
 (psleep 0.5)
 (synth #"prophet" #:note 77 #:attack 4 #:release 2 #:amp 0.5)
 (psleep 0.5)
 (synth #"prophet" #:note 73 #:attack 4 #:release 2 #:amp 0.5)
 (psleep 0.5)
 (synth #"prophet" #:note 69 #:attack 4 #:release 2 #:amp 0.5)
 (psleep 0.5)
 (synth #"prophet" #:note 80 #:attack 4 #:release 2 #:amp 0.5))



(check-equal? (stream->list
               (uscore->score
                (list (synth #"beep" #:note 60)
                      (psleep 4)
                      (synth #"beep" #:note 66)
                      (synth #"beep" #:note 69))
                2000))
              (list (list 2000 (synth #"beep" #:note 60))
                    (list 6000 (synth #"beep" #:note 66))
                    (list 6000 (synth #"beep" #:note 69))))