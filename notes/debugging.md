# Debugging

## In init.pl

```prolog
ttrace(Thread) :-
    thread_signal(Thread, (attach_console, trace)).

tnotrace(Thread) :-
    thread_signal(Thread, (attach_console, notrace)).
```

## Threads

Prepare for debugging using the graphical tracer

```prolog
% All threads
?- tdebug.

% Named thread
?- tdebug(ThreadId).

% Turn off
?- tnodebug.
?- tnodebug(ThreadId).
```
