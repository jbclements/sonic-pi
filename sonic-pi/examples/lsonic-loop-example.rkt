#lang s-exp sonic-pi/lsonic


(fx "bitcrusher"
 (list
  (loop 4
        (list (sample "drum_heavy_kick")
              (psleep 0.5)
              (sample "drum_cymbal_soft")
              (psleep 0.5)))))
(loop 8
      (list (sample "drum_cymbal_pedal")
           (psleep 0.25)
           (sample "bd_haus")
           (psleep 0.25)))
(loop 16
      (list (rand (list
                   (sample "bd_ada")
                   (sample "bd_tek")))
            (psleep 0.25)))
(psleep 2)