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
 Starts an scsynth, returns a context that can be used to play things.
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

@defproc[(make-note [synth-name bytes?]
                    (#:note note number? 60)
                    (#:note_slide note_slide number? 0)
                    (#:note_slide_shape note_slide_shape number? 5)
                    (#:node_slide_curve node_slide_curve number? 0)
                    (#:amp amp number? 1)
                    (#:amp_slide amp_slide number? 0)
                    (#:amp_slide_shape amp_slide_shape number? 5)
                    (#:pan pan number? 0)
                    (#:pan_slide pan_slide number? 0)
                    (#:pan_slide_shape pan_slide_shape number? 5)
                    (#:pan_slide_curve pan_slide_curve number? 0)
                    (#:attack attack number? 0)
                    (#:decay decay number? 0)
                    (#:sustain sustain number? 0)
                    (#:release release number? 1)
                    (#:attack_level attack_level number? 1)
                    (#:sustain_level sustain_level number? 1)
                    (#:env_curve env_curve number? 2)
                    (#:out_bus out_bus number? 12.0)) note?]{
  makes a note. I believe this list of parameters is inherited from
 ... scsynth?
  There's a terrible shortage of documentation surrounding scsynth.
}

@defproc[(note? [note any/c]) boolean?]{
  returns true for notes.
}

}

