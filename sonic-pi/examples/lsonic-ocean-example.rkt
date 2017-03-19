#lang s-exp sonic-pi/lsonic

(fx "reverb" "mix" 0.5
    (block (loop 5
                 (block (synth (choose "bnoise" "cnoise" "gnoise")
                               52
                               "amp" (rrand 0.5 1.5)
                               "attack" (rrand 0 4)
                               "sustain" (rrand 0 2)
                               "release" (rrand 1 3)
                               "cutoff_slide" (rrand 0 3)
                               "cutoff" (rrand 60 115)
                               "pan" (choose -1 0 1)
                               "pan_slide" (rrand 0 1))
                        (psleep (rrand 2 3))))))
