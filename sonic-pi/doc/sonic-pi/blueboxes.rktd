1618
((3) 0 () 6 ((q lib "sonic-pi/fx.rkt") (q lib "sonic-pi/sample.rkt") (q lib "sonic-pi/lsonic.rkt") (q lib "sonic-pi/scsynth/scsynth-abstraction.rkt") (q lib "sonic-pi/note.rkt") (q lib "sonic-pi/loop.rkt")) () (h ! (equal) ((c def c (c (? . 0) q control-fx)) q (1307 . 4)) ((c def c (c (? . 0) q set-fx-busses)) q (1464 . 5)) ((c def c (c (? . 1) q sample?)) q (1103 . 3)) ((c def c (c (? . 3) q end-job)) q (102 . 3)) ((c def c (c (? . 2) q choose-list)) q (2776 . 3)) ((c def c (c (? . 1) q control-sample)) q (976 . 4)) ((c def c (c (? . 2) q thread_s)) q (2558 . 3)) ((c def c (c (? . 0) q fx?)) q (1413 . 3)) ((c def c (c (? . 3) q start-job)) q (34 . 3)) ((c def c (c (? . 4) q note)) q (364 . 5)) ((c def c (c (? . 1) q sample)) q (860 . 4)) ((c def c (c (? . 4) q chord)) q (530 . 5)) ((c def c (c (? . 2) q loop)) q (2627 . 4)) ((c def c (c (? . 5) q live_loop)) q (1755 . 4)) ((c def c (c (? . 2) q choose)) q (2716 . 3)) ((c def c (c (? . 2) q block)) q (2353 . 3)) ((c def c (c (? . 5) q loop)) q (1666 . 4)) ((c def c (c (? . 2) q synth)) q (1853 . 5)) ((c def c (c (? . 3) q startup)) q (0 . 2)) ((c def c (c (? . 2) q psleep)) q (2175 . 3)) ((c def c (c (? . 2) q sample)) q (2237 . 4)) ((c def c (c (? . 2) q rrand_i)) q (2929 . 4)) ((c def c (c (? . 4) q control-note)) q (685 . 4)) ((c def c (c (? . 2) q fx)) q (2417 . 5)) ((c def c (c (? . 2) q chord)) q (2020 . 5)) ((c def c (c (? . 3) q play-synth)) q (173 . 6)) ((c def c (c (? . 2) q rrand)) q (2854 . 4)) ((c def c (c (? . 0) q set-block)) q (1583 . 4)) ((c def c (c (? . 0) q fx)) q (1166 . 5)) ((c def c (c (? . 4) q note?)) q (803 . 3))))
procedure
(startup) -> context?
procedure
(start-job ctxt) -> job-context?
  ctxt : context?
procedure
(end-job job-ctxt) -> void?
  job-ctxt : job-context?
procedure
(play-synth job-ctxt name time param-part ...) -> void?
  job-ctxt : job-context?
  name : string?
  time : inexact-real?
  param-part : (or string? real?)
procedure
(note synth-name pitch param-part ...) -> note?
  synth-name : string?
  pitch : (or string? real?)
  param-part : (or string? real?)
procedure
(chord synth-name pitch interval) -> (listof note?)
  synth-name : string?
  pitch : (or string? real?)
  interval : string?
procedure
(control-note note param-part ...) -> note?
  note : note?
  param-part : (or string? real?)
procedure
(note? note) -> boolean?
  note : any/c
procedure
(sample name param-part ...) -> sample?
  name : string?
  param-part : (or string? real?)
procedure
(control-sample sample param-part ...) -> sample?
  sample : samle?
  param-part : (or string? real?)
procedure
(sample? sample) -> boolean?
  sample : any/c
procedure
(fx name param-part ... block) -> fx?
  name : string?
  param-part : (or string? real?)
  block : (-> any/c)
procedure
(control-fx f param-part ...) -> fx?
  f : fx?
  param-part : (or string? real?)
procedure
(fx? fx) -> boolean?
  fx : any/c
procedure
(set-fx-busses fx in-bus out-bus) -> fx?
  fx : fx?
  in-bus : integer
  out-bus : integer?
procedure
(set-block fx block) -> fx?
  fx : fx?
  block : (-> any/c)
procedure
(loop reps block) -> loop?
  reps : integer?
  block : (-> any/c)
procedure
(live_loop name block) -> live_loop?
  name : string?
  block : (-> any/c)
procedure
(synth synth-name pitch param-part ...) -> note?
  synth-name : string?
  pitch : (or string? real?)
  param-part : (or string? real?)
procedure
(chord synth-name pitch interval) -> (listof note?)
  synth-name : string?
  pitch : (or string? real?)
  interval : string?
procedure
(psleep time) -> void
  time : inexact-real?
procedure
(sample name param-part ...) -> sample?
  name : string?
  param-part : (or string? real?)
procedure
(block sounds) -> (-> any/c)
  sounds : any/c
procedure
(fx name param-part ... block) -> fx?
  name : string?
  param-part : (or string? real?)
  block : (-> any/c)
procedure
(thread_s block) -> thread_s?
  block : (-> any/c)
procedure
(loop reps block) -> loop?
  reps : integer?
  block : (-> any/c)
procedure
(choose elem ...) -> any/c
  elem : any/c
procedure
(choose-list elements) -> any/c
  elements : (listof any/c)
procedure
(rrand min max) -> real?
  min : real?
  max : real?
procedure
(rrand_i min max) -> integer?
  min : integer?
  max : integer?
procedure
(live_loop name block) -> live_loop?
  name : string?
  block : (-> any/c)
