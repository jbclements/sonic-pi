#!/bin/sh

# -a : # of audio buses
# -u : UDP port
# -m : real-time memory size
# -D : max-number of synth defs
./scsynth -a 1024 -u 57118 -m 131072 -D 0
