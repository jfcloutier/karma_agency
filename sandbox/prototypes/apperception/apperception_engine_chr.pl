:- module(apperception_engine, [apperceive/4]).

%% An apperception engine.

%% Given a sequence of observations, it searches for theories (a logic program) best explaining the sequence.
%% The search completes after a preset amount of time or when a great theory is found.

%% The apperception engine searches by iterating through theory templates of roughly increasing complexity,
%% each template specifying a region of the search space of theories.
%% For each template, the apperception engine iterates through theories specified by the template and evaluates them.
%% It rejects those that are not unified and retains the highest rated ones found so far.

/*
cd('sandbox/prototypes/apperception').
[logger, leds_observations, sequence, type_signature, domains, global, template_engine, theory_engine, rating, apperception_engine].
set_log_level(note).
sequence(leds_observations, Sequence), 
min_type_signature(Sequence, MinTypeSignature), 
MaxSignatureExtension = max_extension{max_object_types:1, max_objects:1, max_predicate_types:1},
ApperceptionLimits = apperception_limits{max_signature_extension: MaxSignatureExtension, max_theories_per_template: 100000, keep_n_theories: 1, time_secs: 3000},
apperceive(Sequence, ApperceptionLimits, Theories).
*/

:- use_module(logger).
:- use_module(global).
:- use_module(template_engine).
:- use_module(theory_engine).
:- use_module(rating).
:- use_module(library(chr)).

% Constraints

:- chr_constraint deadline(+float),
                  past_deadline(+float),
                  max_theories_count(+integer),
                  theories_count(+integer),
                  theory(+any, +integer, +integer),
                  collected_theory(-any).

:- chr_option(check_guard_bindings, on).

'update deadline' @ deadline(_) \ deadline(_)#passive <=> true.
'time is up' @ deadline(T1) \ past_deadline(T2) <=> T2 > T1.
'max theories kept' @ max_theories_count(_) \ max_theories_count(_)#passive <=> true.
'theory found, under max count' @ theory(_, _), max_theories_count(Max)#passive \ theories_count(Count)#passive <=> Count < Max | Count1 is Count + 1, theories_count(Count1).
'better theory found, at max count' @ theory(_, Rating1) \ theory(_, Rating2)#passive <=> better_rating(Rating1, Rating2) | true.
'collecting theories' @ collected_theory(Collected), theory(Theory, _) <=> Collected = Theory.
'done collecting theories' @ collected_theory(_) <=> true.

% Given a sequence and some limits, find winning theories.
apperceive(Sequence, ApperceptionLimits, Theories) :-
    init(ApperceptionLimits),
    min_type_signature(Sequence, MinTypeSignature),
    sequence_as_trace(Sequence, SequenceAsTrace),
    create_theory_template_engine(MinTypeSignature, ApperceptionLimits.max_signature_extension, TemplateEngine),
    best_theories(ApperceptionLimits, SequenceAsTrace, TemplateEngine, Theories), !,
    destroy_engine(TheoryTemplateEngine).

init(ApperceptionLimits) :-
    set_deadline(ApperceptioLimits),
    max_theories_count(ApperceptionLimits.keep_n_theories),
    theories_count(0).

set_deadline(ApperceptionLimits) :-
    get_time(Now),
    Deadline is Now + ApperceptionLimits.time_secs,
    deadline(Deadline).

check_time_expired :-
    get_time(Now),
    (past_deadline(Now) -> 
        throw(error(time_expired, context(apperception_engine, Deadline))) 
        ; 
        true).

% Until time expired or a great solution is found,
% grab N templates and concurrently find the best theories in each template,
% keep the best theories from the templates searched,
% and repeat.
best_theories(ApperceptionLimits, SequenceAsTrace, TemplateEngine, Theories) :-
    catch(
        search(ApperceptionLimits, SequenceAsTrace, TemplateEngine),
        error(Thrown, _),
        (collect_best_theories(Theories),
         destroy_engine(TemplateEngine)
        )
    ).

% Search for causal theories in multiple templates concurrently.
% Terminates when an exception is thrown, either because a perfect solution was found in a template, or time's up,
% or all templates have been searched.
search(ApperceptionLimits, SequenceAsTrace, TemplateEngine) :-
    check_time_expired,
    next_templates(TemplateEngine, Templates),
    search_templates(ApperceptionLimits, Templates, SequenceAsTrace),
    !,
    search(ApperceptionLimits, SequenceAsTrace, TemplateEngine).

search(_, _, _).

% Get N templates where N is the number of CPU cores.
next_templates(TemplateEngine, Templates) :-
    current_prolog_flag(cpu_count, N),
    findnsols(N, Template, engine_next(TemplateEngine, Template) , Templates).

search_templates(ApperceptionLimits, Templates, SequenceAsTrace) :-
    length(Templates, N),
    length(Vars, N),
    pairs_keys_values(Pairs, Templates, Vars),
    findall(find_best_theories_in_template(ApperceptionLimits, Template, SequenceAsTrace, Theories), member(Template-Theories, Pairs), Goals),
    concurrent(N, Goals, []),
    new_theories(Vars).

% Find the best theories in a template within allowed limits
find_best_theories_in_template(ApperceptionLimits, Template, BestTheories) :-
    setup_call_cleanup(
        create_theory_engine(Template, TheoryEngine),
        best_theories_in_template(ApperceptionLimits, TheoryEngine, SequenceAsTrace, 0, [], BestTheories),
        destroy_engine(TheoryEngine)
    ).

 best_theories_in_template(ApperceptionLimits, _, _, Count, BestTheories, BestTheories) :-
    ApperceptionLimits.keep_n_theories == Count, !.
    
 best_theories_in_template(ApperceptionLimits, TheoryEngine, SequenceAsTrace, Count, RatedTheories, BestTheories) :-
    next_theory(TheoryEngine, Theory, Trace),
    !,
    Count1 is Count + 1,
    % Rate the theory
    rate_theory(Theory, SequenceAsTrace, Trace, RatedTheory),
    log(note, apperception_engine, 'Found theory ~p', [RatedTheory]),
    maybe_keep_theory(RatedTheory, ApperceptionLimits.keep_n_theories, RatedTheories, KeptRatedTheories),
    !,
    best_theories_in_template(ApperceptionLimits, TheoryEngine, SequenceAsTrace, Count1, KeptRatedTheories, BestTheories).

best_theories_in_template(_, _, _, _, BestTheories, BestTheories).

% Get next theory and its trace from theory engine.
next_theory(TheoryEngine, Theory, Trace) :-
   check_time_expired,
   engine_next_reified(TheoryEngine, Response),
   !,
   handle_theory_engine_reponse(Response, Theory, Trace).

handle_theory_engine_reponse(the(Theory-Trace), Theory, Trace).

handle_theory_engine_reponse(no, _, _) :- 
    log(note, apperception_engine, 'NO MORE THEORIES for the template'),
    fail.

handle_theory_engine_reponse(exception(error(time_expired, context(theory_engine, _))), _, _) :-
    log(note, apperception_engine, 'Time expired getting a theory from a template.'),
    fail.

% handle_theory_engine_reponse(exception(Exception), _, _) :-
%     log(error, apperception_engine, '!!! EXCEPTION getting next theory: ~p', [Exception]),
%     fail.

% Posit new rated theories. Only the best ones are be retained.
 new_theories(RatedTheories) :-
    forall( member(RatedTheory, RatedTheories),
            new_theory(RatedTheory, RatedTheory.rating)
    ).

% Get all kept theories, sorted from best to worse.
collect_best_theories(BestToWorstTheories) :-
    collect_theories(Theories),
    predsort(compare_theories, Theories, WorstToBestTheories),
    reverse(WorstToBestTheories, BestToWorstTheories).

collect_theories([Theory | OtherTheories]) :-
    collected_theory(Theory),
    nonvar(Theory),
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
    log(note, apperception_engine, 'DUD!'),
    !.

maybe_keep_theory(RatedTheory, _, KeptTheories, UpdatedKeptTheories) :-
    Coverage-_ = RatedTheory.rating,
    Coverage == 100,
    throw(error(found_perfect_theory, context(apperception_engine, RatedTheory))).

maybe_keep_theory(RatedTheory, KeepHowMany, KeptTheories, [RatedTheory | KeptTheories]) :-
    length(KeptTheories, L),
    L < KeepHowMany, !,
    log(warn, apperception_engine, 'Keeping theory with rating ~p', [RatedTheory.rating]).

maybe_keep_theory(RatedTheory, _, KeptTheories, UpdatedKeptTheories) :-
    add_if_better(RatedTheory, KeptTheories, UpdatedKeptTheories).

add_if_better(RatedTheory, BestTheoriesSoFar, [RatedTheory | Others]) :-
    find_worse(BestTheoriesSoFar, RatedTheory.rating, WorseTheory),
    !,
    log(note, apperception_engine, 'Keeping theory with rating ~p', [RatedTheory.rating]),
    delete(BestTheoriesSoFar, WorseTheory, Others).

add_if_better(_, BestTheoriesSoFar, BestTheoriesSoFar).

% Worse coverage
find_worse(BestTheoriesSoFar, Coverage-Cost, WorstTheory) :-
    predsort(compare_theories, BestTheoriesSoFar, [WorstTheory | _]),
    WorstCoverage-WorstCost = WorstTheory.rating,
    (WorseCoverage < Coverage
    ; WorstCoverage == Coverage, WorstCost > Cost
    ).

compare_theories(Delta, RatedTheory1, RatedTheory2) :-
    compare_ratings(Delta, RatedTheory1.rating, RatedTheory2.rating).

compare_ratings(Coverage1-Cost1, Coverage2-Cost2) :-
    (Coverage1 < Coverage2 ->
      '<'
      ;
      Coverage1 > Coverage2 ->
      '>'
      ;
      Cost1 > Cost2 ->
      '>'
      ;
      Cost1 > Cost2 ->
      '<'
      ;
      '='
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
