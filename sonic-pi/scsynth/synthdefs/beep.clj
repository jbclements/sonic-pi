;;--
;; This file is part of Sonic Pi: http://sonic-pi.net
;; Full project source: https://github.com/samaaron/sonic-pi
;; License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
;;
;; Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
;; All rights reserved.
;;
;; Permission is granted for use, copying, modification, distribution,
;; and distribution of modified versions of this work as long as this
;; notice is included.
;;++

;; This file uses Overtone to compile the synths from Clojure to
;; SuperCollider compatible binary files. Overtone is Sonic Pi's big
;; brother. See: http://overtone.github.io


(ns sp.synths.saw
  (:use [overtone.live])

  (:require [clojure.string :as str]
            [overtone.sc.dyn-vars :as dvars]))

(defn save-synthdef [sdef folder]
  (let [path (str folder "/" (last (str/split (-> sdef :sdef :name) #"/")) ".scsyndef") ]
    (overtone.sc.machinery.synthdef/synthdef-write (:sdef sdef) path)
    path))

(defn save-to-pi [sdef]
  (save-synthdef sdef "/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/"))


;; saw
;; saw-d - detuned
;; saw-m - modulated
;; saw-n - normalised
;; saw-u - unfiltered (no low pass filter)

;; saw-dn
;; saw-dm
;; saw-du

;; saw-mn
;; saw-mu

;; saw-nu

;; saw-dmn
;; saw-dmu

;; saw-dnu

;; saw-dmnu

(do
  (without-namespace-in-synthdef

      (defsynth sonic-pi-beep [note 52
                            note_slide 0
                            note_slide_shape 5
                            note_slide_curve 0
                            amp 1
                            amp_slide 0
                            amp_slide_shape 5
                            amp_slide_curve 0
                            pan 0
                            pan_slide 0
                            pan_slide_shape 5
                            pan_slide_curve 0
                            attack 0
                            decay 0
                            sustain 0
                            release 0.2
                            attack_level 1
                            sustain_level 1
                            env_curve 2
                            out_bus 0]
     (let [note      (varlag note note_slide note_slide_curve note_slide_shape)
           amp       (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           amp-fudge 1
           pan       (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           freq      (midicps note)
           snd       (sin-osc freq)
           env       (env-gen:kr (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)]
       (out out_bus (pan2 (* amp-fudge env snd) pan amp))))
))
