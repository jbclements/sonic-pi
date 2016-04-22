#lang racket

;; Copyright 2015-2016 John Clements (clements@racket-lang.org)
;; released under Mozilla Public License 2.0


;; this file provides a simpler interface to scysnth. This file started life
;; as a simple packet capture, watching to see how (the real) sonic PI
;; interacted with scsynth. At this point, it's still doing lots of things
;; that don't make any sense for this project, e.g. creating a 'recording'
;; and an 'fx' group, even though there aren't any recording or FX mechanisms.

(require "scsynth-communication.rkt"
         "note.rkt"
         (for-syntax syntax/parse)
         osc
         racket/runtime-path)

(provide (contract-out
          [startup (-> ctxt?)]
          [start-job (-> ctxt? job-ctxt?)]
          [play-note (-> job-ctxt? note? inexact-real? void?)]
          [end-job (-> job-ctxt? void?)]
          [rename synchronize/ctxt synchronize (-> ctxt? void?)])
         make-note
         note?)

(define-runtime-path here ".")
;; this shouldn't be external. Fix this:
(define SYNTHDEF-PATH (build-path here "synthdefs"))
(unless (directory-exists? SYNTHDEF-PATH)
  (error 'synthdef-path
         "expected synthdefs at path: ~v\n" SYNTHDEF-PATH))

;; this represents the context of a running sonic pi graph, containing
;; the 'comm' structure, the group of the mixers and the group of the
;; synth groups
(define-struct ctxt (comm mixer-group synth-group-group) #:transparent)
;; this represents the context of a single job, containing a ctxt,
;; the job-specific mixer, and the job-specific synth-group
(define-struct job-ctxt (ctxt mixer synth-group) #:transparent)

;; represents a synth or group ID
(define ID? exact-nonnegative-integer?)

(define (nonnegative-real? x) (and (real? x) (<= 0 x)))
(define (note-num? x) (and (real? x) (< 0 x)))


;; don't test this file:
(module test racket/base)

;; call the lower-level synchronize on a context
(define (synchronize/ctxt the-ctxt)
  (synchronize (ctxt-comm the-ctxt)))

;; experimentation suggests that scsynth doesn't ever handle 64-bit ints.
;; here's the error message on trying to create a node with id 2^39:
;; [ "#bundle", -2791018499003203584, 
;;    [ "/s_new", "sonic-pi-beep", !unknown tag 'h' 0x68 ! ]
;; (I also get the sense that scsynth should be printing the timestamp
;; as an unsigned int...)

;; establish communication with scsynth, kill everything that's running,
;; and create a new graph. This architecture comes straight from Sonic
;; Pi. If it works for them, I'm assuming it will work for us.
(define (startup)
  (define the-comm (comm-open))
  (send-command the-comm #"/dumpOSC" 1)
  (synchronize the-comm)
  ;; clear the server and create the groups & mixer that we'll need:
  (send-command the-comm #"/clearSched")
  (send-command the-comm #"/g_freeAll" 0)
  (send-command the-comm #"/notify" 1)
  (send-command the-comm #"/d_loadDir" (string->bytes/utf-8
                                        (path->string SYNTHDEF-PATH)))
  (synchronize the-comm)
  ;; I don't think the current architecture is properly
  ;; guaranteeing that things get freed; specifically,
  ;; things in these groups might not be freed by the g_freeAll
  ;; above. Curiously, scsynth's deepFree doesn't free nested groups.
  (define mixer-group (new-group the-comm 'head ROOT-GROUP))
  (define fx-group (new-group the-comm 'before mixer-group))
  (define synth-group-group (new-group the-comm 'before fx-group))
  (define recording-group (new-group the-comm 'after mixer-group))
  (define mixer (new-synth the-comm #"sonic-pi-mixer" 'head mixer-group
                           #"in_bus" 10))
  (send-command/elt the-comm `#s(osc-message #"/n_set"
                                             (,mixer #"invert_stereo" 0.0)))
  (send-command/elt the-comm `#s(osc-message #"/n_set"
                                             (,mixer #"force_mono" 0.0)))
  (synchronize the-comm)
  (ctxt the-comm mixer-group synth-group-group))


;; create a fresh node id.
(define fresh-node-id!
  ;; each node must have a unique ID.
  ;; Worrying about overflow is probably silly....
  (let ([node-id (box 2)])
    (λ ()
      (cond [(int32? (unbox node-id))
             (set-box! node-id (add1 (unbox node-id)))
             (sub1 (unbox node-id))]
            [else (error 'node-id "current node id too large: ~v\n"
                         (unbox node-id))]))))

;; given the comm, a placement command, and a node ID,
;; create a new group in the specified location.
;; return the new ID
(define (new-group comm placement-command relative-to)
  (define new-node-id (fresh-node-id!))
  (define command-num
    (placement-command->number placement-command))
  (send-command comm #"/g_new" new-node-id command-num relative-to)
  new-node-id)

;; create a new synth node, using the given name, placement-command,
;; relative-to, and arguments.
(define (new-synth comm synthdef-name placement-command relative-to . args)
  (define new-node-id (fresh-node-id!))
  (define command-num (placement-command->number placement-command))
  (send-command/elt
   comm
   (osc-message
    #"/s_new"
    (append
     (list synthdef-name new-node-id command-num relative-to)
     args)))
  new-node-id)

;; convert a placement command symbol to the corresponding number
;; (defined in the scsynth API)
(define (placement-command->number pc)
  (match pc
    ['head 0]
    ['tail 1]
    ['before 2]
    ['after 3]
    ['replace 4]))

;; this could be WAY more structured. For now, it's just "something that
;; appears to work." I think I probably shouldn't spend a lot of time
;; building abstractions until I have some idea what abstractions I need.
;; "premature abstraction" etc. etc.
;; this one already exists:
(define ROOT-GROUP 0)

;; send a single message inside of a bundle with a timestamp
(define (send-bundled-message comm time address . args)
  (send-command/elt
   comm
   (osc-bundle (match time
                 ['now 'now]
                 [else (milliseconds->osc-date time)])
               (list (osc-message address args)))))

;; play the given note-num at the given time (inexact milliseconds) by
;; adding a synth to the (job?) synth group
(define (play-note job-ctxt note time)
  (apply
   send-bundled-message
   (ctxt-comm (job-ctxt-ctxt job-ctxt))
   time
   #"/s_new"
   (bytes-append #"sonic-pi-" (first note))
   (fresh-node-id!)
   0
   (job-ctxt-synth-group job-ctxt)
   (apply append (rest note))))

;; start a job. I don't even know what a job is!
(define (start-job the-ctxt)
  (match-define (struct ctxt (comm mixer-group synth-group-group)) the-ctxt)
  (define job-synth-group (new-group comm 'tail synth-group-group))
  ;; see note at beginning about params here, all borrowed from Sonic PI.
  (define job-mixer
    (new-synth comm
               #"sonic-pi-basic_mixer"
               'head mixer-group
               #"amp"
               1
               #"amp_slide"
               0.10000000149011612
               #"amp_slide_shape"
               5
               #"amp_slide_curve"
               0
               #"in_bus"
               12
               #"amp"
               0.30000001192092896
               #"out_bus"
               10))
  (job-ctxt the-ctxt job-mixer job-synth-group))

;; end a job. Code ported directly from Sonic Pi. It looks like this
;; fades out the mixer associated with the job and then frees it and
;; its associated synths
(define (end-job the-job-ctxt)
  (match-define (struct job-ctxt
                  ((struct ctxt (comm _1 _2)) job-mixer job-synth-group))
    the-job-ctxt)
  (send-command comm #"/n_set" job-mixer #"amp_slide" 1.0)
  (send-command comm #"/n_set" job-mixer #"amp" 0.0)
  (sleep 1)
  (send-command comm #"/n_free" job-mixer)
  (send-command comm #"/n_free" job-synth-group))



(module+ main

  (define ctxt (startup))
  (define job-ctxt (start-job ctxt))
  (play-note job-ctxt (make-note #"beep" #:note 100)
             (+ 500 (current-inexact-milliseconds)))
  (sleep 5)
  (end-job job-ctxt))