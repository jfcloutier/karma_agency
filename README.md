# Karma Agency

**THIS REPO IS UNDER CONSTRUCTION**

TODO - send_control vs send_message vs send_query

## Getting started

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

## Dependencies

Clone the following repos in the same directory that contains the `karma_agency` directory:

* [Karma Prolog Utils](https://github.com/jfcloutier/karma_prolog_utils) - Prolog utility modules
* [Karma Actors](https://github.com/jfcloutier/karma_actors) - Prolog actor framework
* [Karma Apperception](https://github.com/jfcloutier/karma_apperception) - A practical Apperception Engine
* [Karma Body](https://github.com/jfcloutier/karma_body) - Elixir code giving access to the robot's sensors and motors, embodied or simulated
* [Karma World](https://github.com/jfcloutier/karma_world) - Elixir code implementing a virtual, simulation environment - to accelerate the development of Agency

## Running the app

% Start agency with the domain and port where th body is hosted

```prolog
?- [load].
?- agency:start('localhost:4000').
```

## Running the tests

```prolog
?- [load].
?- [load_tests].
?- run_tests.
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
% to disable debugging a trace
tnodebug(t1).
```

## Design and implementation

See

* [Design notes](notes/symbolic_cognitive_robotics.md)
* [Apperception Engine](https://zenodo.org/records/10325868)
