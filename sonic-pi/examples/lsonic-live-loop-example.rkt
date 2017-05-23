#lang s-exp sonic-pi/lsonic

(fx "reverb"
    "room" 1
    (block (live_loop "boom"
                      (block #;(sample "bd_boom"
                                     "amp" 10
                                     "rate" 1)
                             (sample "elec_blip"
                                       "amp" 10
                                       "rate" 1)
                             (psleep 8)))))

(fx "echo" "mix" 0.3
    "phase" 0.25
    (block (live_loop "guit"
                      (block #;(sample "guit_e_fifths"
                                     "rate" 0.5)
                             
                             (sample "guit_em9" "rate" 0.5)
                             (psleep 8)))))
