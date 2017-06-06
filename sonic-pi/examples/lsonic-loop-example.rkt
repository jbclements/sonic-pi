#lang s-exp sonic-pi/lsonic

(loop 4
      (block
       (fx "reverb"
           (block
            (sample "drum_heavy_kick")
            (psleep 0.5)
            (sample "drum_cymbal_soft")
            (psleep 0.5)))))
(loop 8
      (block
       (sample "bd_ada")
       (psleep 0.25)
       (sample "bd_haus")
       (psleep 0.25)))
(loop 16
      (block
       (sample (choose "bd_ada" "bd_tek"))
       (psleep 0.25)))