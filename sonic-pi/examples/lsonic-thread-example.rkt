#lang s-exp sonic-pi/lsonic

(thread_s
 (block
  (loop 16
        (block
         (fx "reverb"
             (block
              (sample "ambi_choir"
                      "pan" (rrand -1 1)
                      "rate" (choose 0.5 (/ 1.0 3) (/ 3.0 5)))
              (psleep 0.5)))))))
(loop 12
      (block
       (fx "wobble" "phase" 2
           (block
            (fx "echo" "mix" 0.6
                (block
                 (sample "drum_heavy_kick")
                 (sample "bass_hit_c" "rate" 0.8 "amp" 0.4)
                 (psleep 1)))))))