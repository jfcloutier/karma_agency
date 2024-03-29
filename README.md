# Notes

```prolog
?- [load].
```

```prolog
?- [load_tests].
?- run_tests.
```

## Requirements

Use the development version of SWI-PROLOG (https://www.swi-prolog.org/build/PPA.html)

```sh
% sudo apt-add-repository ppa:swi-prolog/devel
% sudo apt-get update
% sudo apt-get install swi-prolog
```

Install xterm

```sh
sudo apt-get install xterm
```

Define ~/.config/sw-prolog/init.pl

```prolog
:- set_prolog_flag(history, 50).
:- use_module(library(clpfd)).
:- use_module(library(threadutil)).
:- set_prolog_flag(double_quotes, chars).

ttrace(Thread) :-
    thread_signal(Thread, (attach_console, trace)).

tnotrace(Thread) :-
    thread_signal(Thread, (attach_console, notrace)).
```

## Tools

To debug a thread named `t1`, do

```prolog
% To use GUI tracer
guitracer.
% To instrument the thread for tracing
tdebug(t1).
% To initiate tracing on the thread, with output in an XTerm window
ttrace(t1).
% to stop tracing
tnotrace(t1).
% to dsiable debugging a trace
tnodebug(t1).
```

## Design and implementation

See

* [Design notes](notes/symbolic_cognitive_robotics.md)
* [Apperception Engine](https://zenodo.org/records/10325868)
