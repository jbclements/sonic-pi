#lang racket

(require "scsynth-communication.rkt"
         osc)

;; this file is built upon packet capture, watching Sonic PI interact
;; with scsynth...

;; experimentation suggests that scsynth doesn't ever handle 64-bit ints.
;; here's the error message on trying to create a node with id 2^39:
;; [ "#bundle", -2791018499003203584, 
;;    [ "/s_new", "sonic-pi-beep", !unknown tag 'h' 0x68 ! ]
;; (I also get the sense that scsynth should be printing the timestamp as an unsigned int...)

(send-command #"/dumpOSC" 1)

;; each node must have a unique ID. Worrying about overflow is probably silly....
(define node-id (box 2))
(define (fresh-node-id!)
  (cond [(int32? (unbox node-id))
         (set-box! node-id (add1 (unbox node-id)))
         (sub1 (unbox node-id))]
        [else (error 'node-id "current node id too large: ~v\n" (unbox node-id))]))

;; scsynth constants
(define INSERT-AT-HEAD 0)
(define INSERT-AT-TAIL 1)
(define INSERT-BEFORE-NODE 2)
(define INSERT-AFTER-NODE 3)
(define REPLACE-NODE 4)

;; create a new group in the specified location.
;; return the new ID
(define (new-group placement-command relative-to)
  (define new-node-id (fresh-node-id!))
  (define command-num
    (placement-command->number placement-command))
  (send-command #"/g_new" new-node-id command-num relative-to)
  new-node-id)

;; create a new synth node, using the given name, placement-command,
;; relative-to, and arguments.
(define (new-synth synthdef-name placement-command relative-to . args)
  (define new-node-id (fresh-node-id!))
  (define command-num (placement-command->number placement-command))
  (send-command/elt
   (osc-message
    #"/s_new"
    (append
     (list synthdef-name new-node-id command-num relative-to)
     args)))
  new-node-id)

;; convert a placement command symbol to the corresponding number (defined in the scsynth API)
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
(define (send-bundled-message time address . args)
  (send-command/elt
   (osc-bundle (match time
                 ['now 'now]
                 [else (milliseconds->osc-date time)])
               (list (osc-message address args)))))

;; play a note by adding a synth to the (job?) synth group
(define (play-note job-synth-group note-num)
  (send-bundled-message
   'now #;(+ 1000 (current-inexact-milliseconds))
   #"/s_new"
   #"sonic-pi-beep"
   (fresh-node-id!)
   0
   job-synth-group
   #"note"
   note-num
   #"note_slide"
   0
   #"note_slide_shape"
   5
   #"note_slide_curve"
   0
   #"amp"
   1
   #"amp_slide"
   0
   #"amp_slide_shape"
   5
   #"amp_slide_curve"
   0
   #"pan"
   0
   #"pan_slide"
   0
   #"pan_slide_shape"
   5
   #"pan_slide_curve"
   0
   #"attack"
   0
   #"decay"
   0
   #"sustain"
   0
   #"release"
   1
   #"attack_level"
   1
   #"sustain_level"
   1
   #"env_curve"
   2
   #"out_bus"
   ;; inexact number here?
   12.0))


(send-command #"/clearSched")
(send-command #"/g_freeAll" 0)
(send-command #"/notify" 1)
(send-command #"/d_loadDir"
              #"/Applications/Sonic Pi.app/etc/synthdefs")
(define mixer-group (new-group 'head ROOT-GROUP))
(define fx-group (new-group 'before mixer-group))
(define synth-group (new-group 'before fx-group))
(define recording-group (new-group 'after mixer-group))
(define mixer (new-synth #"sonic-pi-mixer" 'head mixer-group
                         #"in_bus" 10))
(send-command/elt `#s(osc-message #"/n_set" (,mixer #"invert_stereo" 0.0)))
(send-command/elt `#s(osc-message #"/n_set" (,mixer #"force_mono" 0.0)))



;; start a job. I don't even know what a job is!
(define (start-job)
  (define job-synth-group (new-group 'tail synth-group))
  (define job-mixer (new-synth #"sonic-pi-basic_mixer" 'head mixer-group
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
  (list job-synth-group job-mixer))

;; end a job.
(define (end-job job-synth-group job-mixer)
  (send-command #"/n_set" job-mixer #"amp_slide" 1.0)
  ;; is there a delay in here?
  (send-command #"/n_set" job-mixer #"amp" 0.0)
  (send-command #"/n_free" job-mixer)
  (send-command #"/n_free" job-synth-group))


(synchronize)



