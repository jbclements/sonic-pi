#lang s-exp sonic-pi/lsonic

(live_loop "boom"
           (block
            (fx "reverb" "room" 1
                (block (sample "bd_boom"
                               "amp" 10
                               "rate" 1)
                       (sample "elec_blip"
                               "amp" 10
                               "rate" 1)
                       (psleep 8)))))
(live_loop "guit"
           (block (fx "echo" "mix" 0.3 "phase" 0.25
                      (block (sample "guit_em9" "rate" 0.5)
                             (sample "guit_e_fifths" "rate" 0.5)
                             (psleep 8)))))

;; Wob rhythm from sonic-pi
#;(live_loop "drum"
           (block
            (fx "wobble" "phase" 2
                (block (fx "echo" "mix" 0.6
                           (block (sample "drum_heavy_kick")
                                  (sample "bass_hit_c" "rate" 0.8 "amp" 0.4)
                                  (psleep 1)))))))
#;(live_loop "choir"
           (block (fx "reverb"
                      (block (sample "ambi_choir"
                                     "rate" (choose 0.5 (/ 1.0 3) (/ 3.0 5))
                                     "pan" (rrand -1 1))
                             (psleep 0.5)
                             ))))