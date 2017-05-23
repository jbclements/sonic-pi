#lang racket

;; Copyright 2015-2016 John Clements (clements@racket-lang.org)
;; released under Mozilla Public License 2.0
(require
  "scsynth/scsynth-abstraction.rkt"
  "note.rkt"
  "sample.rkt"
  "fx.rkt"
  "scsynth/sample-loader.rkt"
  "allocator.rkt"
  "thread-communication.rkt"
  "loop.rkt"
  (for-syntax syntax/parse)
  rackunit)

(provide (except-out (all-from-out racket) sleep #%module-begin)
         (rename-out [my-module-begin #%module-begin])
         synth
         chord
         psleep
         sample
         fx
         thread_s
         loop
         live_loop
         block
         choose
         choose-list
         rrand
         rrand_i
         control
         play
         )

;; let's provide a function to evaluate
;; the definitions-text the button gets.
;; borrowed from
;;  http://stackoverflow.com/questions/10399315/how-to-eval-strings-in-racket
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(define (l-eval text)
  (eval (read (open-input-string text)) ns)
  )

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
;; - a (note ...), representing a note to be played at the current time, or
;; - a (sample ...), representing a sample to be played at the current time, or
;; - an (fx ... block), representing a sound effect to be applied to other uevents
;; - a (thread block), representing a sound to be played in a new thread,
;; - a (live_loop name block), representing an infinite loop with live coding abilities

;; block is a closure around a list of uevents

;; thread representings running a block at the same time as the rest of the user score
(define-struct thread_s (block))

;; a score is (stream/c event) WITH NON-DECREASING TIMES
;; an event is a structure containing
;;  - the virtual time relative to (current-inexact-milliseconds)
;;  - the uevent to be run
;;  - a given bus
(define-struct Event (vtime uevent inbus outbus) #:transparent)


;; given a job-ctxt and an event, queue the event
(define (queue-event job-ctxt evt)
  (cond
    [(sample? (Event-uevent evt))
     (queue-sample job-ctxt (Event-uevent evt) (Event-vtime evt) (Event-outbus evt))]
    [(note? (Event-uevent evt))
     (play-synth job-ctxt
                 (note-name (Event-uevent evt))
                 (Event-vtime evt)
                 (note-params (control-note (Event-uevent evt) "out_bus" (Event-outbus evt))))]
    [(fx? (Event-uevent evt))
     (queue-fx job-ctxt (Event-uevent evt)
               (Event-vtime evt) (Event-inbus evt)
               (Event-outbus evt))]
    ;; when we encounter a live_loop, there are two cases
    ;;  - the live loop already exists, so we send it a message
    ;;  - the live loop does not exist, so we create it and save it
    [(Live_Loop? (Event-uevent evt))
     (let ([name (Live_Loop-name (Event-uevent evt))]
           [block (Live_Loop-block (Event-uevent evt))])
       (cond
         [(thread-exists? name)
          #;(printf "Updating Live_loop: ~v\n" name)
          (thread-send (get-thread-by-name name)
                       (Event-uevent evt))]
         [else
          #;(printf "Sending off Live_Loop: ~v\n"  name)
          (save-thread name (thread (λ ()
                                      (play-forever job-ctxt evt)
                                      )))]))
     ]))

;; live loop that queues a block, then checks
;; for a new block value at each iteration.
;; if a block value is found, we start playing that forver
(define (play-forever job-ctxt evt)
  (define score (uscore->score ((Live_Loop-block (Event-uevent evt)))
                               (Event-vtime evt)
                               (Event-outbus evt)))
  
  (queue-events job-ctxt
                (second score))
  (let ([new_evt (thread-try-receive)])
    (if new_evt
        (begin
          #;(printf "New event! ~v\n" new_evt)
          (play-forever job-ctxt (Event (first score)
                                        new_evt
                                        (Event-inbus evt)
                                        (Event-outbus evt))))
        (play-forever job-ctxt (Event (first score)
                                      (Event-uevent evt)
                                      (Event-inbus evt)
                                      (Event-outbus evt))))))

;; queue a given sample at the specified time
(define (queue-sample job-ctxt samp time bus)
  ; load sample if not already loaded
  (define s-loaded (sample-loaded? (sample-path samp)))
  (define b-info (if s-loaded
                     s-loaded
                     (load-sample job-ctxt (sample-path samp))))
  ;; set the sample buffer id
  (define s (resolve-specific-sampler (control-sample samp "buf" (first b-info) "out_bus" bus) b-info))
  ;; play the sample
  (play-synth job-ctxt
              (sample-name s)
              time
              (sample-params s)))

;; queue an effect and it's corresponding block
(define (queue-fx job-ctxt effect time inbus outbus)
  (define f (set-fx-busses effect
                           inbus
                           outbus)) 
  (play-synth job-ctxt
              (fx-name f)
              time
              (fx-params f))
  (queue-block job-ctxt (fx-block f)))

;; queue a block from fx with a new out_bus
(define (queue-block job-ctxt block)
  (map (λ (s) (queue-event job-ctxt s))
              (stream->list block)))


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
            (queue-events job-ctxt score)
                ;; could keep statistics here...
            ])]))

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
  (- (Event-vtime event) t))

;; lead time in milliseconds. Don't queue a note more than this
;; far in advance:
(define MAX-QUEUE-LEAD 1000)

(define MSEC-PER-SEC 1000)

;; given a user score and a virtual time, produce a score associating
;; a time with each note. Use store-passing style to return a list
;; containing the end time and the stream of events
;; DANGER: loop with only sleep can lead to spaz-out...
(define (uscore->score uscore vtime [outbus 12])
  (cond [(empty? uscore) (list vtime empty-stream)]
        [else (cond [(pisleep? (first uscore))
                     ;; because the last psleep can be ignored, let's
                     ;; create an empty event with the new time
                     (let ([newtime (+ vtime (* MSEC-PER-SEC
                                                (pisleep-duration (first uscore))))])
                       (uscore->score (rest uscore) newtime outbus))]
                    [(sample? (first uscore))
                     (let ([s (uscore->score (rest uscore) vtime outbus)])
                       (list (first s) (stream-cons (Event vtime (first uscore) 0 outbus)
                                                    (second s))))]
                    [(note? (first uscore))
                     (let ([s (uscore->score (rest uscore) vtime outbus)])
                       (list (first s) (stream-cons (Event vtime (first uscore) 0 outbus)
                                                    (second s))))]
                    [(Loop? (first uscore))
                     (cond
                       [(zero? (Loop-reps (first uscore)))
                        (uscore->score (rest uscore)
                                       vtime
                                       outbus)]
                       [else (let* ([b (uscore->score ((Loop-block (first uscore))) vtime outbus)]
                                   [r (uscore->score (append (list (sub1loop (first uscore)))
                                                             (rest uscore))
                                                     (first b)
                                                     outbus)])
                               (list (first r) (stream-append (second b)
                                                              (second r))))])]
                    [(Live_Loop? (first uscore))
                     (let ([rst (uscore->score (rest uscore) vtime outbus)])
                       (list (first rst)
                             (stream-cons (Event vtime (first uscore) 0 outbus)
                                          (second rst))))]
                    [(fx? (first uscore))
                     (let* ([newbus (fresh-bus-id)]
                            [fblock (uscore->score ((fx-block (first uscore)))
                                                   vtime
                                                   newbus)])
                       (list (first fblock)
                             (stream-cons (Event vtime (set-block (first uscore) (second fblock)) newbus outbus)
                                          (second (uscore->score (rest uscore)
                                                         (first fblock)
                                                         outbus)))))]
                    [(thread_s? (first uscore))
                     (list vtime
                           (merge-score (second (uscore->score ((thread_s-block (first uscore))) vtime outbus))
                                        (second (uscore->score (rest uscore) vtime outbus))))]
                    [else (raise-argument-error 'uscore->score
                                                "list of notes, sleeps, samples, loops, threads, or fx's"
                                                0 uscore vtime)])]))

;; merge two scores together to simulate threading
(define (merge-score score1 score2)
  (cond
    [(stream-empty? score1) score2]
    [(stream-empty? score2) score1]
    [else
     (cond
       [(<= (Event-vtime (stream-first score1))
           (Event-vtime (stream-first score2)))
        (stream-cons (stream-first score1)
                     (merge-score (stream-rest score1) score2))]
       [else (stream-cons (stream-first score2)
                          (merge-score score1 (stream-rest score2)))])]))

;; the piece is scheduled to start this far in the future to give time
;; to get things started.
(define START-MSEC-GAP 500)

;; play a user-score
(define (play job-ctxt uscore)
  #;(printf "Playing user score\n")
  (queue-events job-ctxt
                (second (uscore->score uscore
                                       (+ (current-inexact-milliseconds)
                                          START-MSEC-GAP))))
  (listen-for-messages job-ctxt))

;; listens for messages to either play a new uscore
;; or stop everything
(define (listen-for-messages job-ctxt)
  (let ([evt (thread-receive)])
    (if (equal? evt 'stop)
        (begin (kill-all-threads)
               (end-job job-ctxt))
        (begin
          #;(printf "Main thread message: ~v\n" evt)
          (thread
           (λ () (queue-events job-ctxt
                        (second (uscore->score (l-eval evt)
                                               (current-inexact-milliseconds))))))
          (listen-for-messages job-ctxt)))))

;; a pisleep is a number representing time in ms to sleep
(struct pisleep (duration) #:prefab)

;; a simple rand structure to select a random event from a small score
(struct Rand (block))

;; create a pisleep structure
(define (psleep t)
  (pisleep t))

;; control a note, sample, or fx
(define (control s . args)
  (cond [(note? s) (apply control-note
                          (flatten (list s args)))]
        [(sample? s) (apply control-sample (flatten (list s args)))]
        [(fx? s) (apply control-fx (flatten (list s args)))]
        [else (error 'control "not a note, sample, or fx")]))

;; choose from a variable amount of arguments
(define (choose . args)
  (list-ref args (random (length args))))
(define (choose-list l)
  (list-ref l (random (length l))))

;; random in a range.
;; if the arguments are integers, it gets random integers ONLY
;; if the arguments are floats, it calculates a random float
(define (rrand min max)
  (+ min (* (- max min) (random))))

;; random integer in any range, inclusive
(define (rrand_i min max)
  (if (> min 0)
      (random min max)
      (let ([diff (- 1 min)])
        (- (random (+ diff min)
                   (add1 (+ diff max)))
           diff))))


;; a block is a closure around a block of user score
;; this is a nasty macro. is there a better way?
(define-syntax block
  (syntax-rules ()
    [(_ a ...) (λ () (list a ...))]))

(define synth note)

(define-syntax (my-module-begin stx)
  (syntax-parse stx
    [(_ e:expr ...)
     #'(#%module-begin
        (random-seed 52) ;; totally arbitrary
        (define ctxt (startup))
        (define job-ctxt (start-job ctxt))
        (define lsonic (make-logger 'lsonic
                                    (current-logger)))
        (log-message lsonic
                     'info
                     'lsonic
                     "main thread"
                     (thread
                      (λ ()
                        (with-handlers
                            ([exn:fail? (lambda (exn)
                                          (printf "ending job due to error...\n")
                                          (end-job job-ctxt)
                                          (raise exn))])
                          (play job-ctxt (list e ...))))))
        )]))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (check-equal? (play-now? 990) 'play)
  (check-equal? (play-now? 1001) '(delay 721))

  ;; test control
  (check-equal? (control (note "beep" 60) "pan" 1 "decay" 0.5)
                (control-note (note "beep" 60) "pan" 1 "decay" 0.5))

  ;; simple note and sleep test
  (check-equal?
   (stream->list
    (second (uscore->score (list (synth "beep" 60)
                         (psleep 4)
                         (synth "beep" 66)
                         (synth "beep" 69))
                   2000)))
   (list (Event 2000 (synth "beep" 60) 0 12)
         (Event 6000 (synth "beep" 66) 0 12)
         (Event 6000 (synth "beep" 69) 0 12)))
    
  ;; simple loop test
  (check-equal?
   (stream->list
    (second (uscore->score (list (loop 3
                               (block (synth "beep" 60)
                                      (psleep 1)))
                         (synth "beep" 70))
                   2000)))
   (list (Event 2000 (synth "beep" 60) 0 12)
         
         (Event 3000 (synth "beep" 60) 0 12)
         
         (Event 4000 (synth "beep" 60) 0 12)
         
         (Event 5000 (synth "beep" 70) 0 12)))
  
  ;; simple fx test
  (define fx_score (stream->list
                    (second (uscore->score
                     (list (fx "bitcrusher"
                               (block (sample "ambi_choir")
                                      (pisleep 2)))
                           (synth "beep" 60))
                     2000))))
  (define fx_block (stream->list (fx-block (Event-uevent (first fx_score)))))
  (check-equal? fx_block
                (list (Event 2000 (sample "ambi_choir") 0 14)
                      ))

 ;; timing test
  (define fx_loop_score (uscore->score (list (fx "bitcrusher"
                                                 (block (loop 4 (block
                                                                 (synth "bell" 60)
                                                                 (psleep 1))))))
                                       2000))
  (check-equal? (first fx_loop_score) 6000)
  (check-equal? (stream->list (fx-block (Event-uevent (first (stream->list (second fx_loop_score))))))
                (list (Event 2000 (synth "bell" 60) 0 16)
                      (Event 3000 (synth "bell" 60) 0 16)
                      (Event 4000 (synth "bell" 60) 0 16)
                      (Event 5000 (synth "bell" 60) 0 16)))
 
  ;;thread tests
  (check-equal?
   (stream->list
    (second (uscore->score (list (synth "blade" 50)
                         (psleep 1)
                         (thread_s (block
                                  (synth "tb303" 50)))
                         (psleep 1)
                         (synth "blade" 50))
                   2000)))
   (list (Event 2000 (synth "blade" 50) 0 12)
         ;(Event 3000 (void) 0 12)
         (Event 3000 (synth "tb303" 50) 0 12)
         ;(Event 4000 (void) 0 12)
         (Event 4000 (synth "blade" 50) 0 12)))
  
  (check-equal?
   (stream->list
    (second
     (uscore->score (list (synth "blade" 50)
                         (psleep 1)
                         (thread_s (block
                                  (synth "tb303" 50)))
                         (synth "blade" 50))
                   2000)))
   (list (Event 2000 (synth "blade" 50) 0 12)
         ;(Event 3000 (void) 0 12)
         (Event 3000 (synth "tb303" 50) 0 12)
         (Event 3000 (synth "blade" 50) 0 12)))

  ;; small system test
  (define ctxt (startup))
  (define job-ctxt (start-job ctxt))
  (define t1 (thread
              (λ ()
                (play job-ctxt
                      (list (Live_Loop "test1"
                                  (block (sample "loop_garzul")
                                         (synth "prophet" "e1"
                                                "release" 8)
                                         (psleep 8))))))))
  (sleep 4) 
  (thread-send t1 "(list (live_loop \"test1\"
                              (block (sample \"loop_garzul\")
                                         (synth \"beep\" 60
                                                \"release\" 8)
                                         (psleep 8))))")
  (sleep 20)
  (thread-send t1 'stop)
  )

