#lang s-exp sonic-pi/lsonic

(fx "slicer" "phase" 0.25
    (block (fx "reverb" "room" 0.5 "mix" 0.3
               (block
                (loop 4 
                      (block (control 
                              (choose-list
                               (chord
                                "dsaw"
                                (choose "b1" "b2" "e1" "e2" "b3" "e3")
                                "minor"))
                              "release" 8
                              "note_slide" 4
                              "cutoff" 30
                              "cutoff_slide" 4
                              "detune" (rrand 0 0.2)
                              "pan" (rrand -1 0))
                             (control (choose-list
                                       (chord
                                        "dsaw"
                                        (choose "b1" "b2" "e1" "e2" "b3" "e3")
                                        "minor"))
                                      "release" 8
                                      "note_slide" 4
                                      "cutoff" (rrand 80 120)
                                      "cutoff_slide" 4
                                      "detune" (rrand 0 0.2)
                                      "pan" (rrand 0 1))
                             (psleep 8)))))))