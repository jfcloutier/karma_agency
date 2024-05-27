/*
An effector is an a priori cognition actor that communicates with a body effector to execute actions.
The body considers each possible action a given device can take (always sequentially) as defining a separate effector.
Same-device effectors are combined in one effector_ca.
*/

:- module(effector_ca, []).
:- use_module(actor_model(actor_utils)).

:- use_module(code(logger)).

name(Effector, Name) :-
    atomic_list_concat([effector, Effector.id, Effector.capabilities.action], ':', Name).

init(Options, State) :-
    log(info, effector_ca, 'Initiating with ~p', [Options]),
    empty_state(EmptyState),
    option(effectors(Effectors), Options),
    put_state(EmptyState, effectors, Effectors, State1),
    action_domain(State1, ActionDomain),
    put_state(State1, action_domain, ActionDomain, State).

terminate :-
    log(warn, effector_ca, 'Terminating').

handle(message(actuate(Action), _), State, State) :-
    actuate(State, Action).

handle(message(Message, Source), State, State) :-
   log(info, effector_ca, '~@ is NOT handling message ~p from ~w', [self, Message, Source]).

handle(event(Topic, Payload, Source), State, State) :-
    log(info, effector_ca, '~@ is NOT handling event ~w, with payload ~p from ~w)', [self, Topic, Payload, Source]).

handle(query(action_domain), State, ActionDomain) :-
    get_state(State, action_domain, ActionDomain).

handle(query(Query), _, tbd) :-
    log(info, effector_ca, '~@ is NOT handling query ~p', [self, Query]).

%%%%

action_domain(State, ActionDomain) :-
    findall(Action, (member(Effector, State.effectors), Action = Effector.capabilities.action), ActionDomain).

action_url(State, Action, ActionUrl) :-
    member(Effector, State.effectors),
    Action == Effector.capabilities.action,
    ActionUrl = Effector.url.

actuate(State, Action) :-
    action_url(State, Action, ActionUrl),
    body:actuate(ActionUrl).