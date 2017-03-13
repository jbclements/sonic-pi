#lang racket

;; Copyright 2015-2016 John Clements (clements@racket-lang.org)
;; released under Mozilla Public License 2.0
(require
  "scsynth/scsynth-abstraction.rkt"
  "note.rkt"
  "sample.rkt"
  "fx.rkt"
  "sample-loader.rkt"
  "allocator.rkt"
  (for-syntax syntax/parse)
  rackunit)

(provide (except-out (all-from-out racket) sleep #%module-begin)
         (rename-out [my-module-begin #%module-begin]
                     [psleep sleep])
         synth
         sample
         fx
         rand
         psleep
         loop
         block
         choose
         rrand
         control)

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
     (queue-sample job-ctxt evt)]
    [(note? (second evt))
     (play-synth job-ctxt
                 (note-name (second evt))
                 (first evt)
                 (note-params (control-note (second evt) "out_bus" (current-outbus))))]
    [(fx? (second evt))
     (define out-bus (current-outbus))
     (define in-bus (fresh-bus-id))
     (set-current-outbus in-bus)
     (trigger-fx (job-ctxt-ctxt job-ctxt) (set-fx-busses (second evt) in-bus out-bus))
     (queue-block job-ctxt (fx-block (second evt)) in-bus)
     (set-current-outbus out-bus)]
    [(Loop? (second evt)) (queue-block job-ctxt
                           (Loop-block (second evt))
                                       (current-outbus))]))

;; queue a block from fx with a new out_bus
(define (queue-block job-ctxt block new-out-bus)
  (map (λ (s) (queue-event job-ctxt s))
         (stream->list block)))

;; queue a sample
(define (queue-sample job-ctxt evt)
  ; load sample if not already loaded
  (define s-loaded (sample-loaded? (sample-path (second evt))))
  (define b-info (if s-loaded
                     s-loaded
                     (load-sample job-ctxt (sample-path (second evt)))))
  ;; set the sample buffer id
  (define s (resolve-specific-sampler (control-sample (second evt) "buf" (first b-info) "out_bus" (current-outbus)) b-info))
  ;; play the sample
  (play-synth job-ctxt
              (sample-name s)
              (first evt)
              (sample-params s)))


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
#;(define (uscore->score uscore vtime)
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
                    [(Loop? (first uscore))
                     (uscore->score (append
                                     (Loop-block (first uscore))
                                     (rest uscore))
                                    vtime)]
                    [(fx? (first uscore))
                     (define f-block (uscore->score ((fx-block (first uscore)))
                                                     vtime))
                     (define new_vtime (first (last (stream->list f-block))))
                     (stream-cons (list vtime (set-block (first uscore) f-block))
                                  (uscore->score (rest uscore) new_vtime)
                                  )]
                    
                    [else (raise-argument-error 'uscore->score
                                                "list of notes and sleeps"
                                                0 uscore vtime)])]))
;; due to psleeps at the end of a block being ignored, I've
;; decided that time should be kept separately. I'm keeping the old
;; way of doing it commented out in case this ends up not being the best course
;; of action
(define (uscore->score uscore)
  (cond [(empty? uscore) empty-stream]
        [else (display (first uscore))
              (newline)
              (cond [(pisleep? (first uscore))
                     (set-vtime (+ (current-vtime)
                                   (* MSEC-PER-SEC
                                      (pisleep-duration
                                       (first uscore)))))
                     (uscore->score (rest uscore))]
                    [(sample? (first uscore))
                     (stream-cons (list (current-vtime) (first uscore))
                                  (uscore->score (rest uscore)))]
                    [(note? (first uscore))
                     (stream-cons (list (current-vtime) (first uscore))
                                  (uscore->score (rest uscore)))]
                    [(Loop? (first uscore))
                     (uscore->score (append
                                     (repeat-block
                                      (Loop-block (first uscore))
                                      (Loop-reps (first uscore)))
                                     (rest uscore)))]
                    [(Rand? (first uscore))
                     (stream-cons (list
                                   (current-vtime)
                                   (list-ref (Rand-block (first uscore))
                                             (random (length (Rand-block (first uscore))))))
                                  (uscore->score (rest uscore)))]
                    [(fx? (first uscore))
                     (define f-block (uscore->score (force (fx-block (first uscore)))))
                     (stream-cons (list (current-vtime) (set-block (first uscore) f-block))
                                  (uscore->score (rest uscore))
                                  )]
                    
                    [else (raise-argument-error 'uscore->score
                                                "list of notes and sleeps"
                                                0 uscore (current-vtime))])]))


;; the piece is scheduled to start this far in the future to give time
;; to get things started.
(define START-MSEC-GAP 500)

;; play a user-score
#;(define (play job-ctxt uscore)
  (queue-events job-ctxt
                (uscore->score uscore
                               (+ (current-inexact-milliseconds)
                                  START-MSEC-GAP))))
(define (play job-ctxt uscore)
  (set-vtime (+ (current-inexact-milliseconds)
                START-MSEC-GAP))
  (queue-events job-ctxt
                (uscore->score uscore)))

;; a pisleep is a number representing time in ms to sleep
(struct pisleep (duration) #:prefab)
;; a loop is a number representing the number of reps and
;; a block, which is a closure over a user score
(struct Loop (reps block))
;; a simple rand structure to select a random event from a small score
(struct Rand (block))

;; create a pisleep structure
(define (psleep t)
  (pisleep t))

;; control a note or sample
;; NB: currently broken. doesn't matter until
;; lsonic allows definition of variables anyway
(define (control s . args)
  (cond [(note? s) (control-note s args)]
        [(sample? s) (control-sample s args)]
        [(fx? s) (control-fx s args)]
        [else (error 'control "not a note or sample")]))

;; current time to execute an event in milliseconds
(define cur-vtime (box 0))
(define (current-vtime)
  (unbox cur-vtime))
(define (set-vtime time)
  (set-box! cur-vtime time))

;; create a loop structure, given a number of reps and a block 
(define (loop reps block)
  (Loop reps block))
#;(define (loop reps block)
  (Loop reps (repeat-block block reps)))
;; repeat a block [reps] times in a loop
(define (repeat-block block reps)
  (flatten
   (for/list ([n reps])
    (let ([b block])
      (force b))))
  #;(cond
    [(zero? reps) empty]
    [else (append (force block) (repeat-block block (sub1 reps)))]))
;; choose from a variable amount of arguments
(define (choose . args)
  (list-ref args (random (length args))))
;; random float range
(define (rrand min max)
  (+ min (* max (random))))

;; a block is a closure around a block of user score
(define (block . args)
  (lazy args))

;; define a random object
(define (rand block)
  (Rand block))

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
    (set-vtime 2000)
    (check-equal? (stream->list
                   (uscore->score
                    (list (synth "beep" 60)
                          (psleep 4)
                          (synth "beep" 66)
                          (synth "beep" 69))))
                  (list (list 2000 (synth "beep" 60))
                        (list 6000 (synth "beep" 66))
                        (list 6000 (synth "beep" 69))))
    ;; man this test sure got complicated...
    ;; because of stream not being transparent, I extracted
    ;; the fx, then the score from that fx, and turned that
    ;; into a list and checked it
    (set-vtime 2000)
    (check-equal? (stream->list
                   (fx-block
                    (second (second
                     (stream->list
                      (uscore->score
                       (list (synth "beep" 60)
                             (psleep 4)
                             (fx "reverb"
                                 (list (synth "beep" 60)
                                       (psleep 4)
                                       (sample "elec_blip")
                                       (psleep 4)
                                       (synth "beep" 60))))))))))
                  (list (list 6000 (synth "beep" 60))
                        (list 10000 (sample "elec_blip"))
                        (list 14000 (synth "beep" 60))))
    )))