#lang s-exp sonic-pi/lsonic

(fx "echo" "mix" 0.3
    "phase" 0.25
    (block (live_loop "guit"
                      (block (sample "guit_e_fifths"
                                     "rate" 0.5)
                             
                             #;(sample "guit_em9" "rate" 0.5)
                             (psleep 8)))))

(fx "reverb"
    "room" 1
    (block (live_loop "boom"
                      (block (sample "bd_boom"
                                     "amp" 10
                                     "rate" 1)
                             #;(sample "elec_blip"
                                       "amp" 10
                                       "rate" 1)
                             (psleep 8)))))

;; Wob rhythm from sonic-pi
#;(fx "wobble" "phase" 2
    (block (fx "echo" "mix" 0.6
               (block (live_loop "drum"
                                 (block (sample "drum_heavy_kick")
                                        (sample "bass_hit_c" "rate" 0.8
                                                "amp" 0.4)
                                        (psleep 1)))))))

#;(fx "reverb"
    (block (live_loop "choir"
                      (block (sample "ambi_choir" "rate" (choose 0.5
                                                                 (/ 1.0 3)
                                                                 (/ 3.0 5))
                                     "pan" (rrand -1 1))
                             (psleep 0.5)))))