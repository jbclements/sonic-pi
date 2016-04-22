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