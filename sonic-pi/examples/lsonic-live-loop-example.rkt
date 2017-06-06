#lang s-exp sonic-pi/lsonic

#;(live_loop "boom"
           (block
            (fx "reverb" "room" 1
                (block
                 ;(sample "bd_boom" "amp" 10 "rate" 1)
                 (sample "elec_blip" "amp" 10 "rate" 1)
                 (psleep 8)))))
#;(live_loop "guit"
           (block
            (fx "echo" "mix" 0.3 "phase" 0.25
                (block
                 ;(sample "guit_em9" "rate" 0.5)
                 (sample "guit_e_fifths" "rate" 0.5)
                 (psleep 8)))))

(live_loop "extra"
           (block
            (fx "echo"
             (block
              (chord "zawa" "e1" "minor")
              (psleep 4)))
                  ))