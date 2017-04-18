#lang s-exp sonic-pi/lsonic

(fx "bitcrusher"
 (block
  (loop 4
        (block (sample "drum_heavy_kick")
              (psleep 0.5)
              (sample "drum_cymbal_soft")
              (psleep 0.5)))))
(loop 8
      (block (sample "elec_blip")
           (psleep 0.25)
           (sample "bd_haus")
           (psleep 0.25)))
(loop 16
      (block #;(rand (list
                      (sample "bd_ada")
                      (sample "bd_tek")))
             ;; new and improved!
             (sample (choose "bd_ada" "bd_tek"))
             (psleep 0.25)))
