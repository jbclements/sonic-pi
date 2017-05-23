#lang s-exp sonic-pi/lsonic

(thread_s (block
           (fx "reverb" (block
                         (loop 16 (block
                                   (sample "ambi_choir" "rate" (choose 0.5
                                                                       (/ 1.0 3)
                                                                       (/ 3.0 5))
                                           "pan" (rrand -1 1))
                                   (psleep 0.5)))))))

(fx "wobble" "phase" 2
    (block (fx "echo" "mix" 0.6
               (block (loop 12
                            (block
                             (sample "drum_heavy_kick")
                             (sample "bass_hit_c" "rate" 0.8 "amp" 0.4)
                             (psleep 1)))))))
