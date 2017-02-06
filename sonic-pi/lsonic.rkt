#lang racket

;; Copyright 2015-2016 John Clements (clements@racket-lang.org)
;; released under Mozilla Public License 2.0

(provide (except-out (all-from-out racket) sleep #%module-begin)
         (rename-out [my-module-begin #%module-begin]
                     [psleep sleep])
         synth
         sample)

(require
  "scsynth/scsynth-abstraction.rkt"
  "note.rkt"
  "sample.rkt"
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
;; - (loop list-of-uscore), representing a ... hmmm.... not sure about this.

;; a score is (stream/c event) WITH NON-DECREASING TIMES
;; an event is (list/c time-in-msec synth-note)
;; where time-in-msec is relative to (current-inexact-milliseconds)

;; given a job-ctxt and an event, queue the note
(define (queue-event job-ctxt evt)
  (cond
    [(sample? (second evt))
     (define b-info
       (load-sample job-ctxt (Sample-path (second evt))))
     (play-sample job-ctxt
                  (control-sample (second evt) "buf" (first b-info))
                  (first evt))]
    [(note? (second evt)) (play-note job-ctxt (second evt) (first evt))])
    )

;; given a job-ctxt and a list of events, queue them all.
(define (queue-events job-ctxt score)
  (cond [(stream-empty? score)
         (void)]
        [else
         ;; check for job death....
         (define e (stream-first score))
         (match (play-now? (current-lead e))
           ['play
            (queue-event job-ctxt e)
            (queue-events job-ctxt (stream-rest score))]
           [(list 'delay (? number? sleep-msec))
            (sleep (/ sleep-msec MSEC-PER-SEC))
                ;; could keep statistics here...
            (queue-events job-ctxt score)])]))

;; given an event, return 'play to indicate it should be queued now
;; or '(delay n) to indicate that the program should sleep for n milliseconds
(define (play-now? dt)
  (cond [(< dt MAX-QUEUE-LEAD)
         'play]
        [else ;; too early to play
         (define sleep-msec
           ;; turns out sleep tends to oversleep...
           (max MIN-SLEEP-MSEC
                (- dt EXPECTED-MAX-SLEEP-LAG QUEUE-LEAD)))
         (list 'delay sleep-msec)]))


;; don't want to busy-wait... don't sleep less than this many msec:
(define MIN-SLEEP-MSEC 10)
;; ideal lead time
(define QUEUE-LEAD 200)
(define EXPECTED-MAX-SLEEP-LAG 80)



;; how long until the event is supposed to happen?
(define (current-lead event)
  (define t (current-inexact-milliseconds))
  (match-define (list note-time note) event)
  (- note-time t))

;; lead time in milliseconds. Don't queue a note more than this
;; far in advance:
(define MAX-QUEUE-LEAD 1000)

(define MSEC-PER-SEC 1000)

;; given a user score and a virtual time, produce a score associating
;; a time with each note.
;; DANGER: loop with only sleep can lead to spaz-out...
(define (uscore->score uscore vtime)
  (cond [(empty? uscore) empty-stream]
        [else (cond [(pisleep? (first uscore))
                     (uscore->score (rest uscore)
                                    (+ vtime (* MSEC-PER-SEC
                                                (pisleep-duration
                                                 (first uscore)))))]
                    [(sample? (first uscore))
                     (stream-cons (list vtime (first uscore))
                                  (uscore->score (rest uscore)
                                                 vtime))]
                    [(note? (first uscore))
                     (stream-cons (list vtime (first uscore))
                                  (uscore->score (rest uscore)
                                                 vtime))]
                    
                    [else (raise-argument-error 'uscore->score
                                                "list of notes and sleeps"
                                                0 uscore vtime)])]))


;; the piece is scheduled to start this far in the future to give time
;; to get things started.
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

(define synth note)

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

(module+ test
  (require rackunit
           rackunit/text-ui)
  
  (run-tests
   (test-suite
    "lsonic"
    (check-equal? (play-now? 990) 'play)
    (check-equal? (play-now? 1001) '(delay 721))
    
    (check-equal? (stream->list
                   (uscore->score
                    (list (synth "beep" 60)
                          (psleep 4)
                          (synth "beep" 66)
                          (synth "beep" 69))
                    2000))
                  (list (list 2000 (synth "beep" 60))
                        (list 6000 (synth "beep" 66))
                        (list 6000 (synth "beep" 69)))))))