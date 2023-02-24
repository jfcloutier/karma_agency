:- module(apperception_engine, [apperceive/3]).

:- use_module(logger).
:- use_module(global).
:- use_module(template_engine).
:- use_module(theory_engine).
:- use_module(trace).

%% An apperception engine.

%% Given a sequence of observations, it searches for theories (a logic program) best explaining the sequence.
%% The search completes after a preset amount of time or when a great theory is found.

%% The apperception engine searches by iterating through theory templates of roughly increasing complexity,
%% each template specifying a region of the search space of theories.
%% For each template, the apperception engine iterates through theories specified by the template and evaluates them.
%% It rejects those that are not unified and retains the highest rated ones found so far.

/*
cd('sandbox/prototypes/apperception').
[logger, leds_observations, sequence, type_signature, domains, global, template_engine, theory_engine, trace, unity, rules_engine, apperception_engine].
set_log_level(info).
sequence(leds_observations, Sequence), 
min_type_signature(Sequence, MinTypeSignature), 
MaxSignatureExtension = max_extension{max_object_types:1, max_objects:1, max_predicate_types:2},
ApperceptionLimits = apperception_limits{max_signature_extension: MaxSignatureExtension, max_theory_duds: 1000, max_theories_per_template: 100, keep_n_theories: 3, time_secs: 300},
apperceive(Sequence, ApperceptionLimits, Theories).
*/

% Given a sequence and some limits, find winning theories.
apperceive(Sequence, ApperceptionLimits, Theories) :-
    min_type_signature(Sequence, MinTypeSignature),
    create_theory_template_engine(MinTypeSignature, ApperceptionLimits.max_signature_extension, TheoryTemplateEngine),
    init_search(TheoryTemplateEngine, Search),
    find_best_theories(ApperceptionLimits, Search, Sequence, Theories), !,
    destroy_engine(TheoryTemplateEngine).

% Iterate over templates, exploring a maximum of theories for each, validating theories found and keeping the best of the valid theories.
find_best_theories(ApperceptionLimits, Search, _, Theories) :-
    time_expired(ApperceptionLimits, Search),
    log(warn, apperception_engine, 'TIME EXPIRED!'),
    !,
    stop_search(Search),
    Theories = Search.best_theories.

find_best_theories(ApperceptionLimits, Search, Sequence, Theories) :-   
    log(info, apperception_engine, 'Searching for a theory. Search = ~p', [Search]),
    find_theory(ApperceptionLimits, Search, Theory, UpdatedSearch),
    log(info, apperception_engine, 'Found theory ~p from template ~p', [Theory, Search.template]),
    % Making a trace can fail
    (make_trace(Theory, UpdatedSearch.template.type_signature, Trace, UpdatedSearch.module) ->
        log(warn, apperception_engine, 'Created trace ~p', [Trace]),
        rate_theory(Theory, Sequence, Trace, RatedTheory),
        maybe_keep_theory(RatedTheory, ApperceptionLimits.keep_n_theories, UpdatedSearch, LatestSearch)
        ;
        LatestSearch = UpdatedSearch
    ), !,
    find_best_theories(ApperceptionLimits, LatestSearch, Sequence, Theories).

find_best_theories(_, Search, _, Theories) :-
    log(warn, apperception_engine, 'No more theories!'),
    stop_search(Search),
    Theories = Search.best_theories.

init_search(TheoryTemplateEngine, Search) :-
    get_time(Now),
    engine_next(TheoryTemplateEngine, Template),
    create_theory_engine(Template, TheoryEngine),
    uuid(Module),
    Search = search{started_at: Now, theories_count: 0, best_theories: [], theory_duds: 0, template_engine: TheoryTemplateEngine, template: Template, theory_engine: TheoryEngine, module: Module}.

stop_search(Search) :-
    destroy_engine(Search.template_engine),
    destroy_engine(Search.theory_engine).

destroy_engine(Engine) :-
    catch(engine_destroy(Engine), _, true).

try_engine_next(Engine, Result) :-
    catch(engine_next(Engine, Result), _, fail).

time_expired(ApperceptionLimits, Search) :-
    get_time(Now),
    TimeSpent is Now - Search.started_at,
    TimeSpent > ApperceptionLimits.time_secs.

% Find a theory
find_theory(ApperceptionLimits, Search, Theory, LatestSearch) :-
    maybe_change_template(ApperceptionLimits, Search, UpdatedSearch),
    !,
    next_theory(UpdatedSearch, Theory, LatestSearch).

maybe_change_template(ApperceptionLimits, Search, UpdatedSearch) :-
    Search.theories_count >= ApperceptionLimits.max_theories_per_template,
    log(info, apperception_engine, 'Max theory count ~p for template!', [Search.theories_count]),
    !,
    next_theory_template(Search, UpdatedSearch).

maybe_change_template(ApperceptionLimits, Search, UpdatedSearch) :-
    Search.theory_duds >= ApperceptionLimits.max_theory_duds,
    log(warn, apperception_engine, 'Max theory duds reached ~p for template!', [Search.theory_duds]),
    !,
    next_theory_template(Search, UpdatedSearch).

maybe_change_template(_, Search, Search).

% Fails if no more templates
next_theory_template(Search, UpdatedSearch) :-
    destroy_engine(Search.theory_engine),
    try_engine_next(Search.template_engine, Template),
    log(info, apperception_engine, 'NEXT TEMPLATE ~p', [Template]),
    create_theory_engine(Template, TheoryEngine),
    put_dict(theories_count, Search, 0, Search1),
    put_dict(theory_duds, Search1, 0, Search2),
    put_dict(template, Search2, Template, Search3),
    put_dict(theory_engine, Search3, TheoryEngine, UpdatedSearch), !.

next_theory(Search, Theory, UpdatedSearch) :-
    engine_next_reified(Search.theory_engine, Response),
    !,
    handle_theory_engine_reponse(Response, Theory, Search, UpdatedSearch).

handle_theory_engine_reponse(the(Theory), Theory, Search, UpdatedSearch) :-
    Inc is Search.theories_count + 1,
    put_dict(theories_count, Search, Inc, UpdatedSearch), !.

handle_theory_engine_reponse(no, Theory, Search, UpdatedSearch) :-
    log(info, apperception_engine, 'NO MORE THEORIES for the template'),
    next_theory_template(Search, Search1),
    !,
    next_theory(Search1, Theory, UpdatedSearch).

handle_theory_engine_reponse(exception(error(time_expired, context(theory_engine, _))), Theory, Search, UpdatedSearch) :-
    log(info, apperception_engine, 'Time expired getting a theory. Trying another template.'),
    next_theory_template(Search, Search1),
    !,
    next_theory(Search1, Theory, UpdatedSearch).

handle_theory_engine_reponse(exception(Exception), _, _, _) :-
    log(error, apperception_engine, '!!! EXCEPTION getting next theory: ~p', [Exception]),
    fail.

% Rate how well the trace covers the sequence and how simple the theory is.
rate_theory(Theory, Sequence, Trace, RatedTheory) :-
    sequence_as_trace(Sequence, SequenceAsTrace),
    rate_coverage(Trace, SequenceAsTrace, CoverageRating),
    rate_theory_simplicity(Theory, SimplicityRating),
    Rating is CoverageRating + (CoverageRating * SimplicityRating),
    log(info, apperception_engine, 'Theory rated ~p (coverage ~p, simplicity ~p)', [Rating, CoverageRating, SimplicityRating]),
    put_dict(rating, Theory, Rating, RatedTheory).


% Find the best coverage and rate it as a percentage.
% Consider the Trace as circular.
% Cover the Sequence from beginning with the Trace, starting in turn with each round in the Trace, and score.
% Keep the maximum score
rate_coverage(TraceSequence, Sequence, CoverageRating) :-
    findall(CoverageRating, rate_shifted_coverage(TraceSequence, Sequence, CoverageRating), Ratings),
    max_member(CoverageRating, Ratings).

rate_shifted_coverage(TraceSequence, Sequence, CoverageRating):-
    length(TraceSequence, L),
    between(1, L, Start),
    rate_coverage_starting_at(TraceSequence, Start, Sequence, CoverageRating).

% Match sequence rouns with trace rounds from a starting point in the trace.
% Rate coverage per round pair
% Average rating over all pairs
rate_coverage_starting_at(TraceSequence, Start, Sequence, CoverageRating) :-
    pair_rounds(TraceSequence, Start, Sequence, [], RoundPairs),
    findall(Rating, (member(TraceRound-SequenceRound, RoundPairs), rate_round_pair(TraceRound-SequenceRound, Rating)), Ratings),
    sum_list(Ratings, Sum),
    length(Ratings, L),
    CoverageRating is div(Sum, L).

pair_rounds([], _, _, _ , []).
pair_rounds(_, _, [], RoundPairs, RoundPairs).
pair_rounds(TraceSequence, Index, [SequenceRound | OtherSequenceRounds], Acc, RoundPairs) :-
    trace_round_at(TraceSequence, Index, NextIndex, TraceRound),
    pair_rounds(TraceSequence, NextIndex, OtherSequenceRounds, [TraceRound-SequenceRound | Acc], RoundPairs).

% Consider the trace circular
trace_round_at(TraceSequence, Index, NextIndex, TraceRound) :-
    length(TraceSequence, L),
    (Index =< L ->
        nth1(Index, TraceSequence, TraceRound),
        NextIndex is Index + 1
        ;
        nth1(1, TraceSequence, TraceRound),
        NextIndex = 2
    ).

% Rate percent coverage of a sequence round by a trace round
rate_round_pair(TraceRound-SequenceRound, Rating) :-
    intersection(TraceRound, SequenceRound, CommonFacts),
    length(CommonFacts, CommonLength),
    length(SequenceRound, ObservedLength),
    (CommonLength == 0 -> Coverage = 0; Coverage is ObservedLength / CommonLength),
    Percent is Coverage * 100,
    round(Percent, Rating).
    

% Rate simplicity of the theory as a percentage.
% A logarithmic scale
rate_theory_simplicity(Theory, SimplicityRating) :-
    count_elements(Theory.static_rules, StaticRuleElements),
    count_elements(Theory.causal_rules, CausalRuleElements),
    count_elements(Theory.static_constraints, ConstraintElements),
    Count is StaticRuleElements + CausalRuleElements + ConstraintElements,
    BaseCount is max(Count - 100, 1),
    SimplicityRating is max(0, 100 - (log( BaseCount ) * 10)).

count_elements([], 0).
count_elements([Head | Tail], Count) :-
    count_elements(Head, C1),
    count_elements(Tail, C2),
    Count is C1 + C2.
count_elements(Term, Count) :-
    Term =.. [_ , Args], !,
    count_elements(Args, C1),
    Count is C1 + 1.
count_elements(_, 1).

% Never keep a theory with a rating of 0; count it as a dud.
% If there are already the max number of kept theories, replace the lowest rated one
% the theory has higher rating.
maybe_keep_theory(RatedTheory, _, Search, UpdatedSearch) :-
    RatedTheory.rating == 0.0, !,
    TheoryDuds is Search.theory_duds + 1,
    put_dict(theory_duds, Search, TheoryDuds, UpdatedSearch).

maybe_keep_theory(RatedTheory, KeepHowMany, Search, UpdatedSearch) :-
    BestTheories = Search.best_theories,
    length(BestTheories, L),
    L < KeepHowMany, !,
    log(info, apperception_engine, 'Keeping theory with rating ~p', [RatedTheory.rating]),
    put_dict(best_theories, Search, [RatedTheory | BestTheories], UpdatedSearch).

maybe_keep_theory(RatedTheory, _, Search, UpdatedSearch) :-
    BestTheories = Search.best_theories,
    sort(rating, @=<, BestTheories, SortedTheories),
    add_if_better(RatedTheory, SortedTheories, BetterTheories),
    put_dict(best_theories, Search, BetterTheories, UpdatedSearch).

add_if_better(RatedTheory, SortedTheories, [RatedTheory | Others]) :-
    find_worse(SortedTheories, RatedTheory.rating, WorseTheory),!,
    log(info, apperception_engine, 'Keeping theory with rating ~p', [RatedTheory.rating]),
    delete(SortedTheories, WorseTheory, Others).
add_if_better(_, SortedTheories, SortedTheories).

find_worse([WorseTheory | _], Rating, WorseTheory) :-
    WorseTheory.rating < Rating, !.
find_worse([_ | Others], Rating, WorseTheory) :-
    find_worse(Others, Rating, WorseTheory).