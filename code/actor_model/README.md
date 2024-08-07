# The Karma actor framework

## TODO

* Supervisor
  * List children in supervisor options, to (re)start them on supervisor (re)start
    * worker(Name, Module, Params, Options) - Kind is worker or supervisor
        or worker(Name, Module, Options)
        or supervisor(Name, Options)
        or pubsub - restart is permanent
      * put Params in init goal
  * Make sure ALL supervised actors are terminated whenever a supervisor terminates
  * Dynamically added children are NOT restarted when a supervisor restarts
  * A supervisor can provide the list of its children (statically and dynamically added)

## About

An actor module starts and runs named, supervised threads (a.k.a actors).

An actor module must implement `start(Name, Options, Supervisor)`, `stop(Name)` and `kill(Name)`.

Start options:

* topics(Topics) - Topics is the list of topics for the events the worker wants to receive (defaults to [])
* handler(Handler) - Handler is the fully qualified name of the clause header handling events (required)
* init(Init) - Init is the fully qualified name of the clause called when initiating the agent (defaults to worker:noop/0)
* terminate(Terminate) - Terminate is the fully qualified name of the clause called when terminating the agent (defaults to worker:noop/0)

If the module runs a singleton actor, it must also implement `name(Name)`.

An actor is started via `supervisor:start_child(Supervisor, Module, Options)` (for a singleton actor) or `start_child(Supervisor, Module, Name, Options)` (for a named actor).

A supervisor actor restarts its terminated, supervised actors according to the option `restart(Restart)` with which they were started, where Restart is one of:

* permanent (always restart),
* temporary (never restart)
* transient (restart on abnormal exit - the default).

A supervisor can supervise another supervisor. If a supervised supervisor is restarted, only its `permanent` children are restarted.

Actors interact by sending messages to each other:

* directly via `send(Name, Message)` (Name is the name of the target actor)
* indirectly via pubsub `publish(Topic, Payload)` where the Message sent is `event(Topic, Payload, SenderName)`

Message sent by an actor via pubsub is received by all actors that have subscribed to the message topic.

An actor can subscribe to a list of topics or to an individual topic via

* `subscribe_all(Topics)` where topics is a list of topics
* `subscribe(Topic)`

An actor unsubscribes from all topics with `unsubscribe_all()`.

On termination, an actor must send an `exited(Module, Name, Exit)` message to its supervisor where Exit = `exit(normal)` means normal exit.

See `./tests/actor_model` for simple examples of using the Karma actor model.
