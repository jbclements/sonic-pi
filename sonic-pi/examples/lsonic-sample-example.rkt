#lang s-exp sonic-pi/lsonic

;; example using fx, samples, notes, and sleeps
(synth "pretty_bell" 60)
(psleep 0.5)
(sample "elec_plip")
(psleep 0.5)
(synth "pretty_bell" 50)
(psleep 2)

(fx "reverb"
  (block
   (synth "pretty_bell" 60)
   (psleep 0.5)
   (sample "elec_plip")
   (psleep 0.5)
   (synth "pretty_bell" 50)))

(psleep 2)

;; nested fx works too!
(fx "echo"
 (block
 (fx "reverb"
  (block
   (synth "pretty_bell" 60)
   (psleep 0.5)
   (sample "elec_plip")
   (psleep 0.5)
   (synth "pretty_bell" 50)))))

;; let's make sure we can go back to normal
(psleep 4)
(synth "pretty_bell" 60)
(psleep 0.5)
(sample "elec_plip")
(psleep 0.5)
(synth "pretty_bell" 50)