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

(define setup-cmds
  '(#s(osc-message #"/clearSched" ())
    #s(osc-message #"/g_freeAll" (0))
    #s(osc-message #"/notify" (1))
    #s(osc-message
       #"/d_loadDir"
       (#"/Applications/Sonic Pi.app/etc/synthdefs"))
    ))

;; consult sonic-pi src to see what all these groups are for...
'(0 (cons (4 ; synth_group
           (7 (cons
               ;; new "synth" created for each note, apparently.
               (9 (sonic-pi-beep #:note 60
                                 #:note-slide 0
                                 ...
                                 ;; inexact number?
                                 ;; TODO: ask about this?
                                 #:out_bus 12.0
                                 ))
               empty)))
          (cons (3 ; fx_group
                 empty)
                (cons (2 ;mixer_group
                       (cons
                        (8 (sonic-pi-basic_mixer #:amp 1
                                                 #:amp_slide 0.1000
                                                 ...
                                                 #:in_bus 12
                                                 ;; another amp?
                                                 #:amp 0.300
                                                 #:out_bus 10
                                                 ))
                        (cons (6 (mixer #:in-bus 10
                                        #:invert_stereo 0.0
                                        #:force-mono 0.0)) empty)))
                      (cons (5 ; recording_group
                             empty) empty)))))

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


(define (create-groups-and-mixers-cmds)
  (send-command #"/sync" 1)
  (send-command #"/clearSched")
  (send-command #"/g_freeAll" 0)
  (define mixer-group (new-group 'head ROOT-GROUP))
  (define fx-group (new-group 'before mixer-group))
  (define synth-group (new-group 'before fx-group))
  (define recording-group (new-group 'after mixer-group))
  (define mixer (new-synth #"sonic-pi-mixer" 'head mixer-group
                           #"in_bus" 10))
  (send-command/elt `#s(osc-message #"/n_set" (,mixer #"invert_stereo" 0.0)))
  (send-command/elt `#s(osc-message #"/n_set" (,mixer #"force_mono" 0.0)))
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
  job-synth-group
  #;(
(for-each send-command/elt )
 `(#s(osc-message #"/sync" (1))
    #s(osc-message #"/clearSched" ())
    #s(osc-message #"/g_freeAll" (0))
    ;; mixer group
    #s(osc-message #"/g_new" (,MIXER-GROUP ,INSERT-AT-HEAD ,ROOT-GROUP))
    ;; fx group
    #s(osc-message #"/g_new" (,FX-GROUP ,INSERT-BEFORE-NODE ,MIXER-GROUP))
    ;; synth group
    #s(osc-message #"/g_new" (,SYNTH-GROUP ,INSERT-BEFORE-NODE ,FX-GROUP))
    ;; recording group
    #s(osc-message #"/g_new" (,RECORDING-GROUP ,INSERT-AFTER-NODE ,MIXER-GROUP))
    ;; mixer goes in mixing group
    #s(osc-message #"/s_new" (#"sonic-pi-mixer" ,MIXER ,INSERT-AT-HEAD ,MIXER-GROUP #"in_bus" 10))
    ;; why as separate messages?
    #s(osc-message #"/n_set" (,MIXER #"invert_stereo" 0.0))
    #s(osc-message #"/n_set" (,MIXER #"force_mono" 0.0))
    ;; new_synth_group
    #s(osc-message #"/g_new" (,JOB-SYNTH-GROUP ,INSERT-AT-TAIL ,SYNTH-GROUP))
    ;; looks like this might be a "job mixer"...
    #s(osc-message
       #"/s_new"
       (#"sonic-pi-basic_mixer"
        8
        ,INSERT-AT-HEAD
        ,MIXER-GROUP
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
        10)))))

#;(define (new-synth-message))

(define (send-bundled-message time address . args)
  (send-command/elt
   (osc-bundle (milliseconds->osc-date time)
               (list (osc-message address args)))))

(define (play-note note-num)
  (send-bundled-message
   (+ 1000 (current-inexact-milliseconds))
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


(for-each send-command/elt setup-cmds)
(synchronize)
(define job-synth-group (create-groups-and-mixers-cmds))
(synchronize)
(play-note 60)



'(
  #s(osc-message #"/n_set" (8 #"amp_slide" 1.0))
  #s(osc-message #"/n_set" (8 #"amp" 0.0))
  #s(osc-message #"/n_free" (8))
  #s(osc-message #"/n_free" (7))
  #s(osc-message #"/quit" ()))

;(synchronized-command #"/d_load" #"boozle")
;(synchronized-command #"/d_loadDir" #"/Users/clements/sonic-pi/sonic-pi/scsynth/synthdefs")
;(synchronized-command #"/status")

;; this plays the key 
#;((define my-note
  (let ()
    (define id (create-synth "sin-inst" #t))
    (send-msg (n-set1 id "freq" freq))
    (note id freq)))
(sleep 1)
;; stop playing note
(note-off my-note))