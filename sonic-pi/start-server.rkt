#lang racket

(require
  "scsynth/scsynth-abstraction.rkt"
  (for-syntax syntax/parse)
  rackunit)

;; all kinds of interesting interface questions here. Implicit sequence
;; wrapped around the whole thing? Implicit parallelism for loops next to
;; each other? How does Sonic Pi handle this?

;; very nice how sonic pi saves state, reopens desktop

;; optional arguments seem very important, here.

;; sonic pi uses ADSR from the get-go.

;; create a synth for every note?

;; a uscore is a representation of a user's program
;; a uscore is a list of uevents

;; a score is (listof (list/c time-in-msec synth-note))


(define (queue-event job-synth-group evt)
  (play-note job-synth-group (second evt) (first evt)))

(define (queue-events job-synth-group score)
  (for ([e (in-list score)])
    (queue-event job-synth-group e)))

(define MSEC-PER-SEC 1000)

;; convert a sequence of user events into a score, associating
;; a time with each one. When we hit loops this will have to get
;; lazy...
(define (uscore->score uscore vtime)
  (cond [(empty? uscore) empty]
        [else (cond [(pisleep? (first uscore))
                     (uscore->score (rest uscore)
                                    (+ vtime (* MSEC-PER-SEC
                                                (pisleep-duration
                                                 (first uscore)))))]
                    [(note? (first uscore))
                     (cons (list vtime (first uscore))
                           (uscore->score (rest uscore)
                                          vtime))])]))


;; can't actually test this?
(define START-MSEC-GAP 500)

;; play a user-score
(define (play job-synth-group uscore)
  (queue-events job-synth-group (uscore->score uscore
                                               (+ (current-inexact-milliseconds)
                                                  START-MSEC-GAP))))

(struct pisleep (duration) #:prefab)

(define (psleep t)
  (pisleep t))


(define synth make-note)

(match-define (list job-synth-group job-mixer) (start-job))
(time (synchronize))
#;(
(time (synchronize))
(time (synchronize))
(play-note job-synth-group 60)
(sleep 1)
(play-note job-synth-group 61)
(sleep 1)
(play-note job-synth-group 62)
(sleep 2)
(end-job job-synth-group job-mixer)
)

(define-syntax (go stx)
  (syntax-parse stx
    [(_ e:expr ...)
     #'(with-handlers
           ([exn:fail? (lambda (exn)
                         (printf "ending job due to error...\n")
                         (end-job job-synth-group job-mixer)
                         (raise exn))])
           (play job-synth-group (list e ...)))]))

;; block sleep!

(go
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

(sleep 15)
(printf "ending job...\n")
(end-job job-synth-group job-mixer)

(check-equal? (uscore->score
               (list (synth #"beep" #:note 60)
                     (psleep 4)
                     (synth #"beep" #:note 66)
                     (synth #"beep" #:note 69))
               2000)
              (list (list 2000 (synth #"beep" #:note 60))
                    (list 6000 (synth #"beep" #:note 66))
                    (list 6000 (synth #"beep" #:note 69))))