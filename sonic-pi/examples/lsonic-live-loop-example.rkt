#lang s-exp sonic-pi/lsonic

(live_loop "test1"
           (block (sample "loop_garzul")
                  (synth "prophet" "e1"
                         "release" 8)
                  (psleep 8)))