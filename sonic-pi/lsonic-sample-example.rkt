#lang s-exp sonic-pi/lsonic


;; example using fx, samples, notes, and sleeps
(synth "beep" 60)
(psleep 0.5)
(sample "elec_plip")
(psleep 0.5)
(synth "beep" 50)
(psleep 2)

;; bitcrusher really messes up the sound so you can
;; be sure the effect was applied
(fx "bitcrusher"
  (list
   (synth "beep" 60)
   (psleep 0.5)
   (sample "elec_plip")
   (psleep 0.5)
   (synth "beep" 50)))

(psleep 2)

;; nested fx works too!
(fx "echo"
 (list
 (fx "bitcrusher"
  (list
   (synth "beep" 60)
   (psleep 0.5)
   (sample "elec_plip")
   (psleep 0.5)
   (synth "beep" 50)))))

;; let's make sure we can go back to normal
(psleep 4)
(synth "beep" 60)
(psleep 0.5)
(sample "elec_plip")
(psleep 0.5)
(synth "beep" 50)