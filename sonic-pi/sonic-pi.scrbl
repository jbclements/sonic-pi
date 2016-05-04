#lang scribble/manual

@title{Sonic Pi: WORK IN PROGRESS}

@author[(author+email "John Clements" "clements@racket-lang.org")]

@(require (for-label racket))

This package is a collection of functions and a very primitive language
level that follows the lead of Sonic Pi. Specifically, (like Sonic Pi)
it uses scsynth as its sound generation engine, and creates the same
network of units that Sonic Pi does.

This is very much a work in progress. I'm releasing it as a project
so that others can try it out and steal parts of it.

Want to try it out? Install the package, then open @filepath{lsonic-example.rkt}
in DrRacket and click Run.

Tell me what happens!



@section{An Imperative Interface}

@defmodule[sonic-pi/scsynth/scsynth-abstraction]{
One thing that I don't like that much about Sonic Pi is its unabashedly
imperative interface.

However, underneath what I hope will be a nice clean language (lsonic),
I'm also going to publish the lower-level imperative interface. For one
thing, it's pretty obvious what it'll look like: it'll look like Sonic
Pi. That means less design work. Here are some functions:

@defproc[(startup) context?]{
 Starts an scsynth, returns a context that can be used to ... well, to
 create a job context.
}

@defproc[(start-job [ctxt context?]) job-context?]{
 Given a context, starts a job, and return a handle for that job. A job
 corresponds to a ``piece of music.'' 
}

@defproc[(end-job [job-ctxt job-context?]) void?]{
 Ends a job: fades out the job and cancels all scheduled notes associated
 with the job.
}

@defproc[(play-note [job-ctxt job-context?] [note note?] [time inexact-real?]) void?]{
 Play @racket[note] using @racket[job-ctxt], at time @racket[time], specified
 in inexact milliseconds since the epoch. If the @racket[time] value is less than
 the current number of milliseconds since the epoch, the note will be played
 immediately. (Note that this means that you can always specify @racket[0.0] to
 play a note immediately.)
}

}

@defmodule[sonic-pi/note]{

@defproc[(note [synth-name string?] [note-num real?] [param-part (or string? real?)] ...) note?]{
Creates a note. In addition to the @racket[synth-name] and @racket[note-num] (represented as a MIDI
 note number), users may specify non-default values for one of many other parameters using and
 interleaved parameter-name / value style. For instance:

@racketblock[(note "saw" 78 "attack" 0.5 "amp" 0.5)]

Here's the full list of parameters and their defaults:

@racketblock[
   '((note_slide 0)
     (note_slide_shape 5)
     (node_slide_curve 0)
     (amp 1)
     (amp_slide 0)
     (amp_slide_shape 5)
     (pan 0)
     (pan_slide 0)
     (pan_slide_shape 5)
     (pan_slide_curve 0)
     (attack 0)
     (decay 0)
     (sustain 0)
     (release 1)
     (attack_level 1)
     (sustain_level 1)
     (env_curve 2)
     (out_bus 12.0))]

 }

 

@defproc[(note? [note any/c]) boolean?]{
  returns true for notes.
}

}

