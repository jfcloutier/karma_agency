:- module(apperception_engine, [apperceive/3]).

%% An apperception engine.

%% Given a sequence of observations, it searches for theories (a logic program) best explaining the sequence.
%% The search completes after a preset amount of time or when a great theory is found.

%% The apperception engine searches by iterating through theory templates of roughly increasing complexity,
%% each template specifying a region of the search space of theories.
%% For each template, the apperception engine iterates through theories specified by the template and evaluates them.
%% It rejects those that are not unified and retains the highest rated ones found so far.

/*
[load].
[code(logger)].
[apperception(sequence), apperception(type_signature), apperception(domains), apperception(template_engine), apperception(theory_engine), apperception(rating), apperception(apperception_engine)].
[tests(apperception/leds_observations), tests(apperception/eca_observations)].
set_log_level(note).
log_to('test.log').
sequence(eca_observations, Sequence), 
MaxSignatureExtension = max_extension{max_object_types:1, max_objects:1, max_predicate_types:1},
ApperceptionLimits = apperception_limits{max_signature_extension: MaxSignatureExtension, good_enough_coverage: 100, keep_n_theories: 3, time_secs: 300},
apperceive(Sequence, ApperceptionLimits, Theories).
*/


:- use_module(code(logger)).
:- use_module(apperception(template_engine)).
:- use_module(apperception(theory_engine)).
:- use_module(apperception(type_signature)).
:- use_module(apperception(rating)).
:- use_module(library(chr)).

% Constraints

:- chr_constraint deadline(+float),
                  before_deadline(+float),
                  templates_tuple(+any, +int),
                  tuple_templates_count(+int),
                  inc_templates_count,
                  max_theories_count(+int),
                  theories_count(+int),
                  better_theory(+any, +any),
                  collected_theory(-any).

:- chr_option(check_guard_bindings, on).

'update deadline' @ deadline(_) \ deadline(_)#passive <=> true.
'time is up' @ deadline(T1) \ before_deadline(T2) <=> T1 < T2 | fail.
'time is not up' @ before_deadline(_) <=> true.

'new templates tuple' @ templates_tuple(Tuple1, _) \ templates_tuple(Tuple2, _)#passive <=> Tuple1 \== Tuple2 | tuple_templates_count(0).
'same template tuple' @ templates_tuple(_, _) \ templates_tuple(_, _)#passive <=> true.
'one templates count' @ tuple_templates_count(_) \ tuple_templates_count(_)#passive <=> true.
'increment tuple templates count' @ templates_tuple(_, Max)#passive \ tuple_templates_count(Count)#passive, inc_templates_count <=> 
                                        Count < Max | Count1 is Count + 1, tuple_templates_count(Count1).
'max tuple template count reached' @ inc_templates_count <=> fail.

'max theories count' @ max_theories_count(_) \ max_theories_count(_)#passive <=> true.
'theories count ceiling' @ max_theories_count(Max)#passive \ theories_count(Count) <=> Count > Max | theories_count(Max).

'better theory exists over max count' @ max_theories_count(Max)#passive, theories_count(Count)#passive, better_theory(_, Rating1) \ better_theory(_, Rating2) <=> 
                                            Count >= Max, better_rating(Rating1, Rating2) | true.
'keep theory' @ better_theory(_, _) \ theories_count(Count)#passive  <=> Count1 is Count + 1, theories_count(Count1).

'collecting theories' @ collected_theory(Collected), better_theory(Theory, _) <=> Collected = Theory.
'done collecting theories' @ collected_theory(_) <=> true.

% Given a sequence of observations and some limits, find good causal theories.
apperceive(Sequence, ApperceptionLimits, Theories) :-
    init(ApperceptionLimits),
    min_type_signature(Sequence, MinTypeSignature),
    predicates_observed_to_vary(MinTypeSignature, Sequence, VaryingPredicateNames),
    sequence_as_trace(Sequence, SequenceAsTrace),
    tuple_templates_count(0),
    create_theory_template_engine(MinTypeSignature, VaryingPredicateNames, ApperceptionLimits.max_signature_extension, TemplateEngine),
    !,
    best_theories(ApperceptionLimits, SequenceAsTrace, TemplateEngine, Theories),
    log(note, apperception_engine, 'THEORIES = ~p~n', [Theories]),
    destroy_engine(TemplateEngine).

% Initialize apperception
init(ApperceptionLimits) :-
    set_deadline(ApperceptionLimits),
    max_theories_count(ApperceptionLimits.keep_n_theories),
    theories_count(0).

% Set the deadline for apperception
set_deadline(ApperceptionLimits) :-
    get_time(Now),
    Deadline is Now + ApperceptionLimits.time_secs,
    deadline(Deadline).

% Throw an exception if time is expired
check_time_expired :-
    get_time(Now),
    before_deadline(Now) -> true; abort_with_best_theories(time_expired).

% Search for best theories until time expired or a perfect solution is found, or all templates have been searched.
best_theories(ApperceptionLimits, SequenceAsTrace, TemplateEngine, Theories) :-
    catch(
        (
         get_time(StartTime),
         search_for_theories(ApperceptionLimits, SequenceAsTrace, TemplateEngine, StartTime),
         !,
         collect_best_theories(Theories)
        ),
        error(Thrown, Context),
        handle_exception(Thrown, Context, Theories)
    ).

handle_exception(Thrown, context(apperception_engine, Theories), Theories) :-
    member(Thrown, [found_good_enough_theory, time_expired]),
    !,

    log(note, apperception_engine, 'Search terminated by ~p', [Thrown]).

handle_exception(Thrown, Context, []) :-
         log(note, apperception_engine, 'EXCEPTION ~p, in context ~p', [Thrown, Context]).


% Search for causal theories in multiple templates concurrently.
% Grab N templates and concurrently find the best theories in each template.
% Keep the best theories from the templates searched.
% Repeat.
% Terminates when an exception is thrown, either because a perfect solution was found in a template, or time's up,
% or all templates have been searched.
search_for_theories(ApperceptionLimits, SequenceAsTrace, TemplateEngine, StartTime) :-
    check_time_expired,
    next_templates(TemplateEngine, Templates),
    search_templates(ApperceptionLimits, Templates, SequenceAsTrace, StartTime),
    !,
    search_for_theories(ApperceptionLimits, SequenceAsTrace, TemplateEngine, StartTime).

search_for_theories(_, _, _, _).

% Get N templates where N is the number of CPU cores.
next_templates(TemplateEngine, Templates) :-
    current_prolog_flag(cpu_count, N),
    get_templates(TemplateEngine, N, Templates).

get_templates(TemplateEngine, N, [Template | Others]) :-
    N > 0,
    next_template_from_engine(TemplateEngine, Template),
    log(info, apperception_engine, 'Template ~p', [Template]),
    N1 is N -1,
    !,
    get_templates(TemplateEngine, N1, Others).
get_templates(_, _, []).

next_template_from_engine(TemplateEngine, Template) :-
    engine_next_reified(TemplateEngine, Answer),
    !,
    handle_template_engine_answer(Answer, Template),
    got_template_from(TemplateEngine).

% Restart the count of templates if the template was generated from a new tuple
handle_template_engine_answer(the(Template), Template) :-
    templates_tuple(Template.tuple, Template.max_tuple_templates).
handle_template_engine_answer(no, _) :- fail.
handle_template_engine_answer(throw(Exception), _) :-
    log(note, apperception_engine, 'Template engine threw ~p', [Exception]),
    fail.

% Let the Template Engine know whether we've obtained the maximum number of templates for the current tuple.
got_template_from(TemplateEngine) :-
    inc_templates_count ->
        safe_engine_post(TemplateEngine, max_tuple_templates_reached(false))
        ;
        log(warn, apperception_engine, 'Max templates reached for tuple'),
        safe_engine_post(TemplateEngine, max_tuple_templates_reached(true)).

% Ignore permission errors if previous post has not yet been fetched in the engine
safe_engine_post(Engine, Term) :-
    catch(
    (
        engine_post(Engine, Term),
        log(debug, apperception_engine, 'Posted ~p to engine', [Term])
    ),
    Error,
    log(info, apperception_engine, 'Ignoring error ~p posting ~p to engine', [Error, Term])).

search_templates(_, [], _, _) :- !, fail.
search_templates(ApperceptionLimits, Templates, SequenceAsTrace, StartTime) :-
    length(Templates, N),
    length(Vars, N),
    pairs_keys_values(Pairs, Templates, Vars),
    findall(find_best_theories_in_template(ApperceptionLimits, Template, SequenceAsTrace, StartTime, Theories), member(Template-Theories, Pairs), Goals),
    concurrent(N, Goals, []),
    !,
    keep_best_theories(Goals).

% Find the best theories in a template within allowed limits
find_best_theories_in_template(ApperceptionLimits, Template, SequenceAsTrace, StartTime, BestTheories) :-
    create_theory_engine(Template, SequenceAsTrace, TheoryEngine),
    best_theories_in_template(ApperceptionLimits, Template, TheoryEngine, SequenceAsTrace, StartTime, [], BestTheories),
    log(info, apperception_engine, 'Best theories for template ~p are ~p', [Template, BestTheories]),
    destroy_engine(TheoryEngine).
   
 best_theories_in_template(ApperceptionLimits, Template, TheoryEngine, SequenceAsTrace, StartTime, RatedTheories, BestTheories) :-
    next_theory(TheoryEngine, StartTime, Theory, Trace),
    % Rate the theory
    log(note, apperception_engine, 'Max static=~p, Max causal=~p, and causal rules ~p', [Template.limits.max_static_rules, Template.limits.max_causal_rules, Theory.causal_rules]),
    rate_theory(Theory, SequenceAsTrace, Trace, RatedTheory),
    log(info, apperception_engine, 'Found theory ~p', [RatedTheory]),
    maybe_keep_theory(RatedTheory, ApperceptionLimits, RatedTheories, KeptRatedTheories),
    !,
    best_theories_in_template(ApperceptionLimits, Template, TheoryEngine, SequenceAsTrace, StartTime, KeptRatedTheories, BestTheories).

best_theories_in_template(_, _, _,_ , _, BestTheories, BestTheories).

% Get next theory and its trace from theory engine, unless time is expired.
next_theory(TheoryEngine, StartTime, Theory, Trace) :-
   check_time_expired,
   log(info, apperception_engine, 'Getting next theory from engine'),
   engine_next_reified(TheoryEngine, Response),
   handle_theory_engine_reponse(Response, StartTime, Theory, Trace).

handle_theory_engine_reponse(the(Theory-Trace), StartTime, FoundTheory, Trace) :-
    get_time(Now),
    FoundTime is round(Now - StartTime),
    put_dict(found_time, Theory, FoundTime, FoundTheory).

handle_theory_engine_reponse(no, _, _, _) :- 
    log(info, apperception_engine, 'NO MORE THEORIES for the template'),
    fail.

handle_theory_engine_reponse(exception(error(template_search_time_expired, context(theory_engine, Reason))), _, _, _) :-
    log(info, apperception_engine, 'Time expired getting a theory from a template: ~p.', [Reason]),
    fail.

% handle_theory_engine_reponse(exception(Exception), _, _, _) :-
%     log(error, apperception_engine, '!!! EXCEPTION getting next theory: ~p', [Exception]),
%     fail.
abort_with_best_theories(Cause) :-
    collect_best_theories(BestTheories),
    throw(error(Cause, context(apperception_engine, BestTheories))).

keep_best_theories([]).
keep_best_theories([find_best_theories_in_template(_, _, _, _, Theories) | Others]) :-
    new_theories(Theories),
    keep_best_theories(Others).

% Posit new rated theories. Only the best ones across templates are be retained.
new_theories([]).
new_theories([RatedTheory | Others]) :-
    found_better_theory(RatedTheory),
    new_theories(Others).

found_better_theory(RatedTheory) :-
    better_theory(RatedTheory, RatedTheory.rating).

% Get all kept theories, sorted from best to worse.
collect_best_theories(BestToWorstTheories) :-
    collect_theories(Theories),
    predsort(compare_theories, Theories, WorstToBestTheories),
    reverse(WorstToBestTheories, BestToWorstTheories).

collect_theories([Theory | OtherTheories]) :-
    collected_theory(Theory),
    nonvar(Theory),
    !,
    collect_theories(OtherTheories).

collect_theories([]).

destroy_engine(Engine) :-
    catch(engine_destroy(Engine), _, true).

% Never keep a theory with a rating of 0; count it as a dud.
% If there are already the max number of kept theories, replace the lowest rated one
% the theory has higher rating.
maybe_keep_theory(RatedTheory, _, KeptTheories, KeptTheories) :-
    Coverage-_ = RatedTheory.rating,
    Coverage == 0,
    log(info, apperception_engine, 'DUD!'),
    !.
% Throw an exception if a perfect theory is found to halt search
maybe_keep_theory(RatedTheory, ApperceptionLimits, _, _) :-
    Coverage-_ = RatedTheory.rating,
    Coverage >= ApperceptionLimits.good_enough_coverage,
    found_better_theory(RatedTheory),
    abort_with_best_theories(found_good_enough_theory).   

maybe_keep_theory(RatedTheory, ApperceptionLimits, KeptTheories, [RatedTheory | KeptTheories]) :-
    length(KeptTheories, L),
    L < ApperceptionLimits.keep_n_theories, !,
    log(warn, apperception_engine, 'Keeping new theory with rating ~p (under quota)', [RatedTheory.rating]).

maybe_keep_theory(RatedTheory, _, KeptTheories, UpdatedKeptTheories) :-
    add_if_better(RatedTheory, KeptTheories, UpdatedKeptTheories).

add_if_better(RatedTheory, BestTheoriesSoFar, [RatedTheory | Others]) :-
    find_worse(BestTheoriesSoFar, RatedTheory.rating, WorseTheory),
    !,
    log(info, apperception_engine, 'Keeping better theory with rating ~p (better one)', [RatedTheory.rating]),
    delete(BestTheoriesSoFar, WorseTheory, Others).

add_if_better(_, BestTheoriesSoFar, BestTheoriesSoFar).

% Worse coverage
find_worse(BestTheoriesSoFar, Coverage-Cost, WorstTheory) :-
    predsort(compare_theories, BestTheoriesSoFar, [WorstTheory | _]),
    WorstCoverage-WorstCost = WorstTheory.rating,
    (WorstCoverage < Coverage
    ; WorstCoverage == Coverage, WorstCost > Cost
    ).

compare_theories(Delta, RatedTheory1, RatedTheory2) :-
    compare_ratings(Delta, RatedTheory1.rating, RatedTheory2.rating).

compare_ratings(Delta, Coverage1-Cost1, Coverage2-Cost2) :-
    (Coverage1 < Coverage2 ->
      Delta = '<'
      ;
      Coverage1 > Coverage2 ->
      Delta = '>'
      ;
      Cost1 > Cost2 ->
      Delta = '<'
      ;
      Cost1 < Cost2 ->
      Delta = '>'
      ;
      Delta = '='
    ).

better_rating(Rating1, Rating2) :-
    compare_ratings('>', Rating1, Rating2).

%%% Sequence as trace

% Covert a sequence to a trace so they can be compared
sequence_as_trace(Sequence, Trace) :-
    sequence_as_trace_(Sequence, [], Trace).

sequence_as_trace_([], ReversedTrace, Trace) :-
    reverse(ReversedTrace, Trace).

sequence_as_trace_([State | OtherStates], Acc, Trace) :-
    state_as_round(State, Round),
    sequence_as_trace_(OtherStates, [Round | Acc], Trace).

% Convert a trace round to a state in a sequence of observations
state_as_round(State, Round) :-
    state_as_round_(State, [], Round).

state_as_round_([], Round, Round).

state_as_round_([Observation | OtherObservations], Acc, Round) :-
    Observation =.. [sensed, PredicateName, ObservationArgs, _],
    !,
    convert_observation_args(ObservationArgs, Args),
    Fact =.. [PredicateName | Args],
    state_as_round_(OtherObservations, [Fact | Acc], Round).

state_as_round_([_ | OtherObservations], Acc, Round) :-
    state_as_round_(OtherObservations, Acc, Round).

convert_observation_args(ObservationArgs, Args) :-
    convert_observation_args_(ObservationArgs, [], Args).

 convert_observation_args_([], ReversedArgs, Args) :-
    reverse(ReversedArgs, Args).

convert_observation_args_([ObservationArg | OtherObservationArgs], Acc, Args) :-
    convert_observation_arg(ObservationArg, Arg),
    convert_observation_args_(OtherObservationArgs, [Arg | Acc], Args).

convert_observation_arg(ObservationArg, ObjectName) :-
    ObservationArg =.. [object, _, ObjectName], !.

convert_observation_arg(Arg, Arg).
