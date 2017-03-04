#lang s-exp sonic-pi/lsonic


(fx "bitcrusher"
 (list
  (loop 4
        (list (sample "drum_heavy_kick")
              (psleep 0.5)
              (sample "drum_cymbal_soft")
              (psleep 0.5)))))
(sample "bass_drop_c" "release" 4 "attack" 0.5)
(psleep 2)
(loop 8
      (list (sample "drum_cymbal_pedal")
           (psleep 0.25)
           (sample "bd_haus")
           (psleep 0.25)))
(loop 2
      (list (loop 4
                  (list (sample "bd_ada")
                        (psleep 0.25)))
            (loop 4
                  (list (sample "bd_tek")
                        (psleep 0.25)))
            ))