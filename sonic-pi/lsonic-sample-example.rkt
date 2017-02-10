#lang s-exp sonic-pi/lsonic

;; come up with good sample example, then commit
(sample "loop_amen" "pan" 1)
(psleep 1.25)
(sample "loop_amen" "pan" -1)
(psleep 1.25)
(sample "ambi_choir" "rate" 0.5)