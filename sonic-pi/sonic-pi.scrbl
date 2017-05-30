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

Want to try it out? Install the package, then open one of the examples
in @filepath{examples/}
in DrRacket and click Run.

Tell me what happens!

@section{Prerequisites}
SuperCollider must be installed on the system prior to use.
For Windows users, it must be installed to @filepath{C:\\Program Files}

@section{An Imperative Interface}

@subsection{SuperCollider Abstraction}
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

 @defproc[(play-synth [job-ctxt job-context?] [name string?] [time inexact-real?] [param-part (or string? real?)] ...) void?]{
  Play @racket[note], @racket[sample], or @racket[fx] using @racket[job-ctxt], at time @racket[time], specified
  in inexact milliseconds since the epoch. If the @racket[time] value is less than
  the current number of milliseconds since the epoch, the note will be played
  immediately. (Note that this means that you can always specify @racket[0.0] to
  play a note immediately.)
 }

}

@subsection{Note}
@defmodule[sonic-pi/note]{

 @defproc[(note [synth-name string?] [pitch (or string? real?)] [param-part (or string? real?)] ...) note?]{
  Creates a note. In addition to the @racket[synth-name] and @racket[pitch], users may specify non-default values for one of many other parameters using and
  interleaved parameter-name / value style. For instance:

  @racketblock[(note "saw" 78 "attack" 0.5 "amp" 0.5)]

  The pitch may be specified as a MIDI note number, or in musical notation. Musical notation may include
  the note, accidental, or octave in that order :

  @racketblock[(note "bronky" 39)
               (note "bronky" "A")
               (note "pulse" "C2")
               (note "beep" "af3")]

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

 @defproc[(chord [synth-name string?] [pitch (or string? real?)] [interval string?]) (listof note?)]{
  creates a list of notes in the specified chord. For example:
  @racketblock[(chord "saw" "a" "minor")]

  Here is a list of all supported intervals:
  @racketblock['("major" "minor" "major7" "dom7" "minor7" "aug" "dim" "dim7")]
 }

 @defproc[(control-note [note note?] [param-part (or string? real?)] ...) note?]{
  Reassigns the optional paramaters to the specified paramaters, using the note's
  original values as the default.
 }

 @defproc[(note? [note any/c]) boolean?]{
  returns true for notes.
 }

}

@subsection{Sample}
@defmodule[sonic-pi/sample]{

 @defproc[(sample [name string?] [param-part (or string? real?)] ...) sample?]{
  Create a sample with a given @racket[name] as either a file path or the name of one of
  the samples provided in @filepath{sonic-pi/samples}. The samples are taken directly from
  the original Sonic Pi repo. The full list can be found at @filepath{sonic-pi/samples/README.md}
 }

 @defproc[(control-sample [sample samle?] [param-part (or string? real?)] ...) sample?]{
  Reassigns the optional parameters to the specified ones, using the sample's original
  values as the defaults.
 }

 @defproc[(sample? [sample any/c]) boolean?]{
  returns true for samples.
 }

}

@subsection{FX}
@defmodule[sonic-pi/fx]{

 @defproc[(fx [name string?] [param-part (or string? real?)] ... [block (-> any/c)]) fx?]{
  Creates a sound effect to be applied to other sounds. @racket[name] may be
  one of the predefined sound effects provided by the original Sonic Pi. Unlike with notes,
  all paramaters provided are passed to SuperCollider, not just the defaults. The last param-part is expected to be
  a @racket[block], containing a set of sounds to be played with this sound effect. For instance:

  @racketblock[(fx "bitcrusher"
                   "bits" 8
                   (λ () (synth "beep" 60)))]
 }

 @defproc[(control-fx [f fx?] [param-part (or string? real?)] ...) fx?]{
  Reassigns a sound effect with new parameters. For instance:
  @racketblock[(define my-fx (fx "bitcrusher" "bits" 8
                                 (λ () (synth "beep" 60))))
               (control-fx my-fx "bits" 2)]
 }

 @defproc[(fx? [fx any/c]) boolean?]{
  returns true for fx's
 }

 @defproc[(set-fx-busses [fx fx?] [in-bus integer] [out-bus integer?]) fx?]{
  Sets the @racket[in-bus] and @racket[out-bus] parameters for a sound effect
 }

 @defproc[(set-block [fx fx?] [block (-> any/c)]) fx?]{
  Sets the block of music to be played for a sound effect
 }
}

@subsection{Loops}
@defmodule[sonic-pi/loop]{
 @defproc[(loop [reps integer?] [block (-> any/c)]) loop?]{
  Creates a loop to played @racket[reps] times}

 @defproc[(live_loop [name string?] [block (-> any/c)]) live_loop?]{
  Creates a live_loop structure to play continuous notes}
}

@section{A Live Coding Environment}
This package provides a primitive language similar to Sonic Pi
with a simple live coding environment within DrRacket.
The provided @racket[play] and @racket[stop] buttons will
update the playing music in real time and stop the music and
shutdown SuperCollider, respectively.

@subsection{Sounds}
@defmodulelang[sonic-pi/lsonic]{

 @defproc[(synth [synth-name string?] [pitch (or string? real?)] [param-part (or string? real?)] ...) note?]{
  Plays a note of the given @racket[pitch] using the given @racket[synth-name]. This is unlike
  Sonic Pi, which does not require you to input a synth each time.
  Users may specify non-default values for one of many other parameters using and
  interleaved parameter-name / value style. For instance:

  @racketblock[(synth "saw" 78 "attack" 0.5 "amp" 0.5)]

  The pitch may be specified as a MIDI note number, or in musical notation. Musical notation may include
  the note, accidental, and octave :

  @racketblock[(synth "bronky" 39)
               (synth "bronky" "A")
               (synth "pulse" "C2")
               (synth "beep" "af3")]

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

 @defproc[(chord [synth-name string?] [pitch (or string? real?)] [interval string?]) (listof note?)]{
  Play a chord. For example:
  @racketblock[(chord "saw" "a" "minor")]

  Here is a list of all supported @racket[interval]s:
  @racketblock['("major" "minor" "major7" "dom7" "minor7" "aug" "dim" "dim7")]
 }

 @defproc[(psleep [time inexact-real?]) void]{
  Sleep for @racket[time] seconds before playing more.
  The following two beeps will happen at the same time
  @racketblock[(synth "beep" 60)
               (synth "beep" 62)]
  Whereas the next two will happen one after the other
  @racketblock[(synth "beep" 60)
               (psleep 1)
               (synth "beep" 62)]
 }

 @defproc[(sample [name string?] [param-part (or string? real?)] ...) sample?]{
  Play a sample. These are the same samples provided by Sonic Pi.
  To sample a local file, set the @racket[name] to the full file path of the sample
  The optional arguments to sample
  do not include all the same as the original Sonic Pi. Here is a list of the supported parameters:
  @racketblock['((amp 1)
                 (amp_slide 0)
                 (amp_slide_shape 1)
                 (amp_slide_curve 0)
                 (attack 0.0)
                 (decay 0)
                 (sustain -1)
                 (release 0.0)
                 (attack_level 1)
                 (decay_level -1)
                 (sustain_level 1)
                 (env_curve 1)
                 (pan 0)
                 (pan_slide 0)
                 (pan_slide_shape 1)
                 (pan_slide_curve 0)
                 (lpf -1)
                 (lpf_slide 0)
                 (lpf_slide_shape 1)
                 (lpf_slide_curve 0)
                 (hpf -1)
                 (hpf_slide 0)
                 (hpf_slide_shape 1)
                 (hpf_slide_curve 0)
                 (rate 1))]
 }

 @defproc[(block [sounds any/c]) (-> any/c)]{
  Creates a @racket[block] of sound(s) similar to the @elem["do ... end" #:style "hspace"] blocks of Sonic Pi.
  This is accomplished by wrapping the set of events in a closure. Thus
  @racketblock[(block (synth "pretty_bell" 60))]
  is equivalent to
  @racketblock[(λ () (synth "pretty_bell" 60))]
  @racket[block]s are used when applying fx, looping, or threading. For example:
  @racketblock[(live_loop "foo"
                          (block (sample "bd_boom")
                                 (psleep 2)))]
 }

 @defproc[(fx [name string?] [param-part (or string? real?)] ... [block (-> any/c)]) fx?]{
  Play @racket[block] of sounds using the specified sound effect.
  The effects provided are the same as Sonic Pi. Each fx has a different set of
  parameters for it, so the defaults are determined by the synth definition. All params given will be passed
  and any that don't apply to the fx will be ignored.
  For example:
  @racketblock[(fx "reverb" "room" 2
                   (block (synth "saw" "bf")))]
 }

 @defproc[(thread_s [block (-> any/c)]) thread_s?]{
  Plays the @racket[block] in a separate thread from the rest of the music.
  This does not actually spawn a new thread of execution, but simulates threading
  by scheduling events simultaneously. Use to overlay sounds:
  @racketblock[(thread_s (block
                          (fx "reverb" (block
                                        (loop 16 (block
                                                  (sample "ambi_choir" "rate" (choose 0.5
                                                                                      (/ 1.0 3)
                                                                                      (/ 3.0 5))
                                                          "pan" (rrand -1 1))
                                                  (psleep 0.5)))))))


               (fx "wobble" "phase" 2
                   (block (fx "echo" "mix" 0.6
                              (block (loop 12
                                           (block
                                            (sample "drum_heavy_kick")
                                            (sample "bass_hit_c" "rate" 0.8 "amp" 0.4)
                                            (psleep 1)))))))]
 }

 @defproc[(loop [reps integer?] [block (-> any/c)]) loop?]{
  Loop the @racket[block reps] times. To loop infinitely, provide
  a large number for the @racket[reps]. Loops are blocking, so use in
  a thread to overlay loops.

  @defproc[(choose [elem any/c] ...) any/c]{
   Randomly pick one element from the ones provided. If any element passed to it is a list, it
   will behave like @racket[choose-list].
   For example:
   @racketblock[(choose (sample "bd_haus")
                        (sample "bd_boom")
                        (sample "elec_plip"))]}

  @defproc[(choose-list [elements (listof any/c)]) any/c]{
   Same as @racket[choose] but for elements specifically in a list, though @racket[choose] will
   accept lists as well.
   @racketblock[(choose-list (chord "beep" "a" "minor"))]}
 }

 @defproc[(rrand [min real?] [max real?]) real?]{
  Generates a random number between @racket[min] and @racket[max].}

 @defproc[(rrand_i [min integer?] [max integer?]) integer?]{
  Generates a random integer from @racket[min] to @racket[max], inclusive.}
}

@subsection{Live Coding}
Live coding is implemented in a very simple fashion for use with DrRacket.
There are two button provided with this package: @racket[stop] and @racket[play].

When using the live coding environment, first press the DrRacket run button. This will
start up SuperCollider and begin playing your music. After making changes to the source,
press the provided @racket[play] button to update the music while it plays. It may take two
or three iterations of a loop to update. When you wish to end the music that is playing,
press the @racket[stop] button. This will also shut down SuperCollider,
so you will need to re-run the program to start it back up. The @racket[stop] button will
always attempt to shut down SuperCollider so this can be used if it failed to shut down previously.

@defproc[(live_loop [name string?] [block (-> any/c)]) live_loop?]{
 Play a @racket[block] of music in an infinite loop with the ability to update.
 Live loops are executed in their own threads so they must have unique names and they are non-blocking.
 The following example comes from the Sonic Pi tutorial:
 
 @racketblock[
 (live_loop "boom"
           (block
            (fx "reverb" "room" 1
                (block (sample "bd_boom"
                               "amp" 10
                               "rate" 1)
                       (sample "elec_blip"
                               "amp" 10
                               "rate" 1)
                       (psleep 8)))))
(live_loop "guit"
           (block (fx "echo" "mix" 0.3 "phase" 0.25
                      (block (sample "guit_em9" "rate" 0.5)
                             (sample "guit_e_fifths" "rate" 0.5)
                             (psleep 8)))))]
 After running, comment as you see fit, and press the play button to hear it change.
 It may take a few loops for the changes to take effect.
}

@section{Bugs}
There are bound to be plenty of bugs, so report!

Here is a list of known bugs:
@itemize[
 @item{Loops may fall out of sync or skip an iteration due to timing delays}
 @item{Bitcrusher fx may not work in Windows}
 ]


