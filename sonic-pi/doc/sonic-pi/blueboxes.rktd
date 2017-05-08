332
((3) 0 () 1 ((q lib "sonic-pi/note.rkt")) () (h ! (equal) ((c def c (c (? . 0) q end-job)) q (102 . 3)) ((c def c (c (? . 0) q play-note)) q (173 . 5)) ((c def c (c (? . 0) q start-job)) q (34 . 3)) ((c def c (c (? . 0) q note?)) q (461 . 3)) ((c def c (c (? . 0) q note)) q (304 . 5)) ((c def c (c (? . 0) q startup)) q (0 . 2))))
procedure
(startup) -> context?
procedure
(start-job ctxt) -> job-context?
  ctxt : context?
procedure
(end-job job-ctxt) -> void?
  job-ctxt : job-context?
procedure
(play-note job-ctxt note time) -> void?
  job-ctxt : job-context?
  note : note?
  time : inexact-real?
procedure
(note synth-name note-num param-part ...) -> note?
  synth-name : string?
  note-num : real?
  param-part : (or string? real?)
procedure
(note? note) -> boolean?
  note : any/c
