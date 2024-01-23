:- module(apperception_engine, [apperceive/3]).

/* 
   An apperception engine re-implementing the Apperception Engine described in "Making sense of sensory input" by Richard Evans, Jose Hernandez-Orallo, Johannes Welbl, Pushmeet Kohli, Marek Sergot (https://arxiv.org/abs/1910.02227)
   Our implementation is optimized to find, quickly enough, good enough causal theories (coherent logic programs) that explain past observations. 

   How an apperception engine works:
  
        Given a sequence of observations, it searches for causal theories (tiny logic programs) best explaining the sequence.
        The search completes after a preset amount of time or as soon as a good enough theory is found.
        A causal theory can then be used to predict incoming observations.
  
   The apperception engine searches by iterating through "regions"" of roughly increasing solution complexity,
   each region specifying templates (vocabularies of identical complexity) with which to express causal theories.
   The set of all templates defines the search space for causal theories.

   For each template, the apperception engine constructs theories specified by the templates on offer and evaluates them for Kantian unity, for accuracy and complexity.
   It rejects those that are not unified and retains the highest rated ones found so far.

   The search is restrained by a number of parameters:

      max_signature_extension: (max_extension{max_object_types:1, max_objects:1, max_predicate_types:1}) How many object types, objects  and predicate types can be abduced (imagined)
      good_enough_coverage:(Percent, e.g. 87) When a theory with at least this trace coverage is found, the search is terminated with this theory as the sole answer.
      keep_n_theories: (Number) How many theories a template can contribute as candidates for the best overall theories on each iteration
      funnel: (FromNumber-ToNumber) How many (most promising) templates are retained to be searched again on each iteration. The FromNumber is reduced by one after each iteration and never goes under ToNumber.

    The Apperception Engine applies a variety of heuristics and induction biases to improve the odds of finding a good enough sollution in good time,
    if not on the current attempt then perhaps on the next one.

    See https://zenodo.org/records/10325868
*/

/*
[load].
[code(logger)].
[apperception(sequence), apperception(type_signature), apperception(domains), apperception(template_engine), apperception(theory_engine), apperception(rating), apperception(apperception_engine)].
[tests(apperception/leds_observations), tests(apperception/eca_observations)].
set_log_level(note).
log_to('test.log').

sequence(leds_observations, Sequence), 
MaxSignatureExtension = max_extension{max_object_types:1, max_objects:1, max_predicate_types:1},
ApperceptionLimits = apperception_limits{max_signature_extension: MaxSignatureExtension, good_enough_coverage: 100, keep_n_theories: 3, funnel: 3-2, time_secs: 60},
apperceive(Sequence, ApperceptionLimits, Theories).

sequence(eca_observations, Sequence), 
MaxSignatureExtension = max_extension{max_object_types:0, max_objects:0, max_predicate_types:0},
ApperceptionLimits = apperception_limits{max_signature_extension: MaxSignatureExtension, good_enough_coverage: 100, keep_n_theories: 3, funnel: 10-5, time_secs: 300},
apperceive(Sequence, ApperceptionLimits, Theories).

*/


:- use_module(code(logger)).
:- use_module(apperception(template_engine)).
:- use_module(apperception(theory_engine)).
:- use_module(apperception(type_signature)).
:- use_module(apperception(rating)).
:- use_module(library(chr)).

% Constraints

%% Each thread has its own CHR store which sits on the stack and is thus subjected to backtracking.

% TODO - use chr_type
:- chr_constraint deadline(+float),
                  before_deadline(+float),
                  templates_region(+any, +int),
                  region_templates_count(+int),
                  inc_templates_count,
                  max_theories_count(+int),
                  get_max_theories_count(+any),
                  theories_count(+int),
                  better_theory(+any, +any, +any),
                  next_rating(+any, +any),
                  collect_theory(+any, +any),
                  trim_theory(+any, +any),
                  better_template(+any, +any),
                  collected_theory(-any),
                  template_exhausted(+any),
                  is_template_exhausted(+any),
                  clear_exhausted_templates.

:- chr_option(check_guard_bindings, on).

'update deadline' @ deadline(_) \ deadline(_)#passive <=> true.
'time is up' @ deadline(T1) \ before_deadline(T2) <=> T1 < T2 | fail.
'time is not up' @ before_deadline(_) <=> true.

'new templates region' @ templates_region(Region1, _) \ templates_region(Region2, _)#passive <=> Region1 \== Region2 | region_templates_count(0).
'same template region' @ templates_region(_, _) \ templates_region(_, _)#passive <=> true.
'one templates count' @ region_templates_count(_) \ region_templates_count(_)#passive <=> true.
'increment region templates count' @ templates_region(_, Max)#passive \ region_templates_count(Count)#passive, inc_templates_count <=> 
                                        Count < Max | Count1 is Count + 1, region_templates_count(Count1).
'max region template count reached' @ inc_templates_count <=> fail.

'max theories count' @ max_theories_count(_) \ max_theories_count(_)#passive <=> true.

'get max theories count' @ max_theories_count(Max) \ get_max_theories_count(M) <=> Max = M, true.

'accumulate and keep count of better theories' @ better_theory(_, _, _) \ theories_count(Count)#passive <=> Count1 is Count + 1 | theories_count(Count1).

'no need to trim theories' @ theories_count(Count), max_theories_count(Max) \ trim_theory(_, _) <=> Count =< Max | true.
'trim a template-redundant theory' @ better_theory(_, R1, Id) \ trim_theory(_, Id), better_theory(_, R2, Id), theories_count(Count) <=>
                                       better_or_same_rating(R1, R2), Count1 is Count -1 | theories_count(Count1).
'trim theory with worst rating' @ trim_theory(WorstRating, ''), better_theory(_, WorstRating, _), theories_count(Count) <=>  Count1 is Count - 1 | theories_count(Count1).
% Should not be needed
'done trimming theories' @ trim_theory(_, _) <=> true.

'collect theory' @ better_theory(T, _, _) \ collect_theory(Theory, Acc) <=> excluded(T, Acc) | Theory = T, true.
'done collecting theories' @ collect_theory(_, _) <=> true.

'collect rating' @ better_theory(_, R, _) \ next_rating(Rating, Acc) <=> excluded(R, Acc) | Rating = R, true.
'done collecting rating' @ next_rating(_, _) <=> true.

'fully explored template is exhausted' @ template_exhausted(Id1)#passive \ template_exhausted(Id2) <=> Id1 == Id2 | true.

'template exhausted?' @ template_exhausted(Id1) \ is_template_exhausted(Id2) <=> Id1 == Id2 | true.
'template not exhausted' @ is_template_exhausted(_) <=> fail.

'another better template' @ better_theory(Theory, _, _) \ better_template(Template, Acc) <=> template_not_exhausted(Theory.template.id), excluded(Theory.template, Acc) | Template = Theory.template, true.
'done collecting better templates' @ better_template(_, _) <=> true.

'collecting theories' @ collected_theory(Collected), better_theory(Theory, _, _) <=> Collected = Theory.
'done collecting theories' @ collected_theory(_) <=> true.

'clear exhausted templates' @ clear_exhausted_templates \ template_exhausted(_) <=> true.
'done clearing exhausted templates' @ clear_exhausted_templates <=> true.

% Given a sequence of observations and some limits, find good causal theories.
apperceive(Sequence, ApperceptionLimits, Theories) :-
    log(error, apperception_engine, 'STARTING'),
    get_time(Now),
    init(ApperceptionLimits, Now),
    min_type_signature(Sequence, MinTypeSignature),
    predicates_observed_to_vary(MinTypeSignature, Sequence, VaryingPredicateNames),
    sequence_as_trace(Sequence, SequenceAsTrace),
    region_templates_count(0),
    create_theory_template_engine(MinTypeSignature, VaryingPredicateNames, ApperceptionLimits.max_signature_extension, TemplateEngine),
    !,
    best_theories(ApperceptionLimits, SequenceAsTrace, TemplateEngine, first_iteration, 0, Now, Theories),
    log(error, apperception_engine, 'THEORIES = ~p~n', [Theories]),
    destroy_engine(TemplateEngine).

% Initialize apperception
init(ApperceptionLimits, Now) :-
    set_deadline(ApperceptionLimits, Now),
    FunnelFrom-_ = ApperceptionLimits.funnel,
    max_theories_count(FunnelFrom),
    theories_count(0).

% Set the deadline for apperception
set_deadline(ApperceptionLimits, Now) :-
    Deadline is Now + ApperceptionLimits.time_secs,
    deadline(Deadline).

% Throw an exception if apperception time is expired 
check_time_expired :-
    get_time(Now),
    (before_deadline(Now) -> 
    true
    ; 
    collect_best_theories(Theories), 
    abort_with_best_theories(time_expired, Theories)
    ).

% Search templates from templates engine or from already searched templates for best theories
% until time expired or a good enough solution is found, or time is expired.
best_theories(ApperceptionLimits, SequenceAsTrace, TemplateEngine, SearchedTemplates, Iteration, Epoch, Theories) :-
    catch(
        (
         get_time(StartTime),
         search_for_theories(ApperceptionLimits, SequenceAsTrace, TemplateEngine, SearchedTemplates, StartTime, Iteration, Epoch),
         !,
         get_time(Now),
         round(Now - StartTime, Elapsed),
         log(note, apperception_engine, 'Done searching all templates after ~p secs', [Elapsed]),
         next_iteration(ApperceptionLimits, Templates),
         Iteration1 is Iteration + 1,
         best_theories(ApperceptionLimits, SequenceAsTrace, TemplateEngine, Templates, Iteration1, Epoch, Theories)
        ),
        error(Thrown, Context),
        handle_exception(Thrown, Context, Theories)
    ).

handle_exception(Thrown, context(apperception_engine, Theories), Theories) :-
    member(Thrown, [found_good_enough_theory, time_expired, search_completed, template_exhausted, stopped]),
    !,
    log(note, apperception_engine, 'Search terminated by ~p', [Thrown]).

handle_exception(Thrown, Context, []) :-
    log(note, apperception_engine, 'EXCEPTION ~p, in context ~p', [Thrown, Context]).

next_iteration(ApperceptionLimits, Templates) :-
    collect_better_templates([], Templates),
    [_ | _] = Templates,
    !,
    get_max_theories_count(Max),
    (Max > 1 ->
        _-FunnelTo = ApperceptionLimits.funnel,
        Max1 is max(Max - 1, FunnelTo),
        max_theories_count(Max1),
        trim_theories
        ;
        true
    ).

% No template left to iterate on
next_iteration(_, _) :-
    collect_best_theories(Theories), 
    abort_with_best_theories(search_completed, Theories).

template_not_exhausted(Id) :-
    \+ is_template_exhausted(Id).

% List the unique templates of the better theories in the CHR store
collect_better_templates(Acc,  Templates) :-
    better_template(Template, Acc),
    (nonvar(Template) ->
    collect_better_templates([Template | Acc], Templates)
    ;
    Templates = Acc,
    findall(Id, (member(Template, Templates), Id = Template.id), Ids),
    log(note, apperception_engine, 'Searching again the best templates: ~p', [Ids]),
    clear_exhausted_templates
    ).

worst_rating(Rating) :-
    collect_ratings([], Ratings),
    !,
    predsort(compare_ratings, Ratings, [Rating | _]).

collect_ratings(Visited, Ratings) :-
    next_rating(Rating, Visited),
    (nonvar(Rating) ->
        collect_ratings([Rating | Visited], Ratings)
        ;
        Ratings = Visited
    ).

excluded(Element, List) :- 
    \+ memberchk(Element, List).

% Search for causal theories in multiple templates concurrently.
% Grab N templates and concurrently find the best theories in each template.
% Keep the best theories from the templates searched.
% Repeat.
% Terminates when an exception is thrown, either because a perfect solution was found in a template, or time's up,
% or all templates have been searched.
search_for_theories(ApperceptionLimits, SequenceAsTrace, TemplateEngine, first_iteration, StartTime, Iteration, Epoch) :-
    check_time_expired,
    next_templates(TemplateEngine, Templates),
    search_templates(ApperceptionLimits, Templates, SequenceAsTrace, StartTime, Iteration, Epoch),
    !,
    search_for_theories(ApperceptionLimits, SequenceAsTrace, TemplateEngine, first_iteration, StartTime, Iteration, Epoch).

search_for_theories(_, _, _, [], _, _, _) :- !.

search_for_theories(ApperceptionLimits, SequenceAsTrace, TemplateEngine, SearchedTemplates, StartTime, Iteration, Epoch) :-
    is_list(SearchedTemplates),
    check_time_expired,
    next_searched_templates(SearchedTemplates, Templates, RemainingSearchedTemplates),
    search_templates(ApperceptionLimits, Templates, SequenceAsTrace, StartTime, Iteration, Epoch),
    !,
    search_for_theories(ApperceptionLimits, SequenceAsTrace, TemplateEngine, RemainingSearchedTemplates, StartTime, Iteration, Epoch).

search_for_theories(_, _, _, _, _, _, _).

% Get N templates where N is the number of CPU cores.
next_templates(TemplateEngine, Templates) :-
    current_prolog_flag(cpu_count, N),
    get_templates(TemplateEngine, N, Templates).

next_searched_templates(SearchedTemplates, Templates, RemainingSearchedTemplates) :-
    current_prolog_flag(cpu_count, N),
    take_many(SearchedTemplates, N, Templates, RemainingSearchedTemplates).

% Take as much as N first elements, leaving the rest
take_many([], _, [], []).
take_many(List, 0, [], List).
take_many([Element | Rest], N, [Element | RestTaken], Remaining) :-
    N > 0,
    N1 is N - 1,
    take_many(Rest, N1, RestTaken, Remaining).

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

% Restart the count of templates if the template was generated from a new region
handle_template_engine_answer(the(Template), Template) :-
    templates_region(Template.region, Template.max_region_templates).    
handle_template_engine_answer(no, _) :- fail.
handle_template_engine_answer(throw(Exception), _) :-
    log(note, apperception_engine, 'Template engine threw ~p', [Exception]),
    fail.

% Let the Template Engine know whether we've obtained the maximum number of templates for the current region.
got_template_from(TemplateEngine) :-
    inc_templates_count ->
        safe_engine_post(TemplateEngine, max_region_templates_reached(false))
        ;
        log(warn, apperception_engine, 'Max templates reached for region'),
        safe_engine_post(TemplateEngine, max_region_templates_reached(true)).

% Ignore permission errors if previous post has not yet been fetched in the engine
safe_engine_post(Engine, Term) :-
    catch(
    (
        engine_post(Engine, Term),
        log(debug, apperception_engine, 'Posted ~p to engine', [Term])
    ),
    Error,
    log(info, apperception_engine, 'Ignoring error ~p posting ~p to engine', [Error, Term])).

search_templates(_, [], _, _, _, _) :- !, fail.

search_templates(ApperceptionLimits, Templates, SequenceAsTrace, StartTime, Iteration, Epoch) :-
    length(Templates, N),
    length(Vars, N),
    pairs_keys_values(Pairs, Templates, Vars),
    findall(find_best_theories_in_template(ApperceptionLimits, Template, SequenceAsTrace, StartTime, Iteration, Epoch, _, Theories), member(Template-Theories, Pairs), Goals),
    concurrent(N, Goals, []),
    !,
    keep_best_theories(Goals).

% Find the best theories in a template within allowed limits
% Called in a concurrent thread
find_best_theories_in_template(ApperceptionLimits, Template, SequenceAsTrace, StartTime, Iteration, Epoch, Status, BestTheories) :-
    create_theory_engine(Template, SequenceAsTrace, Iteration, TheoryEngine),
    get_time(TemplateStartTime),
    best_theories_in_template(ApperceptionLimits, Template, TheoryEngine, SequenceAsTrace, Epoch, StartTime, TemplateStartTime, Status, [], BestTheories),
    log(info, apperception_engine, 'Best theories for template ~p are ~p (~p)', [Template, BestTheories, Status]),
    destroy_engine(TheoryEngine).

best_theories_in_template(ApperceptionLimits, _, _, _, _, StartTime, _, time_expired, BestTheories, BestTheories) :-
    get_time(Now),
    EndTime is StartTime + ApperceptionLimits.time_secs,
    Now > EndTime,
    log(note, apperception_engine, 'Time is up!'),
    !.

best_theories_in_template(ApperceptionLimits, Template, TheoryEngine, SequenceAsTrace, Epoch, StartTime, TemplateStartTime, Status, RatedTheories, BestTheories) :-
    next_theory(TheoryEngine, Theory, Trace, Epoch),
    handle_next_theory(Theory, Trace, ApperceptionLimits, Template, TheoryEngine, SequenceAsTrace, Epoch, StartTime, TemplateStartTime, Status, RatedTheories, BestTheories).

handle_next_theory(no_more, _, _, Template, _, _, _, _, _, template_exhausted, BestTheories, BestTheories) :-
    log(note, apperception_engine, 'Template exhausted ~p', [Template.id]), !.

handle_next_theory(stopped, _, _,  _, _, _, _, _, _, stopped, BestTheories, BestTheories) :- !.
handle_next_theory(Theory, Trace, ApperceptionLimits, Template, TheoryEngine, SequenceAsTrace, Epoch, StartTime, TemplateStartTime, Status, RatedTheories, BestTheories) :-
    % Rate the theory
    % log(note, apperception_engine, 'Max static=~p, Max causal=~p, and causal rules ~p', [Template.limits.max_static_rules, Template.limits.max_causal_rules, Theory.causal_rules]),
    rate_theory(Theory, SequenceAsTrace, Trace, RatedTheory),
    log(info, apperception_engine, 'Found theory ~p', [RatedTheory]),
    maybe_keep_theory(RatedTheory, ApperceptionLimits, Template, Epoch, RatedTheories, KeptRatedTheories),
    !,
    best_theories_in_template(ApperceptionLimits, Template, TheoryEngine, SequenceAsTrace, Epoch, StartTime, TemplateStartTime, Status, KeptRatedTheories, BestTheories).

% A theory was found with coverage that is close enough to the best coverage found so far
acceptable_theory_found([Theory | Others], AcceptableCoverage) :-
    (Coverage-_ = Theory.rating,
    Coverage >= AcceptableCoverage)
    ;
    acceptable_theory_found(Others, AcceptableCoverage).

% Get next theory and its trace from theory engine, unless time is expired.
next_theory(TheoryEngine, Theory, Trace, Epoch) :-
   log(info, apperception_engine, 'Getting next theory from engine'),
   engine_next_reified(TheoryEngine, Response),
   handle_theory_engine_response(Response, Theory, Trace, Epoch).

handle_theory_engine_response(the(Theory-Trace), FoundTheory, Trace, Epoch) :-
    get_time(Now),
    FoundTime is round(Now - Epoch),
    put_dict(found_time, Theory, FoundTime, FoundTheory).

handle_theory_engine_response(no, no_more, _, _) :- 
    log(info, apperception_engine, 'No more theories in this template').

handle_theory_engine_response(exception(error(template_search_time_expired, context(theory_engine, Reason))), stopped, _, _) :-
    log(info, apperception_engine, 'Time expired searching for theories in template: ~p.', [Reason]).

handle_theory_engine_response(exception(error(template_search_truncated, context(theory_engine, Reason))), stopped, _, _) :-
    log(note, apperception_engine, 'Search in template completed but it was truncated: ~p.', [Reason]).

abort_with_best_theories(Cause, BestTheories) :-
    throw(error(Cause, context(apperception_engine, BestTheories))).

keep_best_theories([]).
keep_best_theories([find_best_theories_in_template(_, Template, _, _, _, _, Status, Theories) | Others]) :-
    log(info, apperception_engine, 'Status of template ~p is ~p', [Template.id, Status]),
    maybe_mark_template(Status, Template),
    new_theories(Theories),
    keep_best_theories(Others).

maybe_mark_template(template_exhausted, Template) :-
    template_exhausted(Template.id).
maybe_mark_template(_, _).

% Posit new rated theories. Only the best ones across templates are be retained.
new_theories([]).
new_theories([RatedTheory | Others]) :-
    found_better_theory(RatedTheory),
    new_theories(Others).

found_better_theory(RatedTheory) :-
    log(note, apperception_engine, 'Found better theory with rating ~p in template ~p', [RatedTheory.rating, RatedTheory.template.id]),
    better_theory(RatedTheory, RatedTheory.rating, RatedTheory.template.id),
    trim_theories.

trim_theories :-
    worst_rating(Rating),
    (duplicate_template_id(Id) ->
        trim_theory(Rating, Id)
        ;
        trim_theory(Rating, '')
    ).

duplicate_template_id(Id) :-
    collect_theories([], Theories),
    findall(Id, (member(T, Theories), Id = T.template.id), Ids),
    select(Id, Ids, Rest),
    memberchk(Id, Rest).

collect_theories(Visited, Theories) :-
    collect_theory(Theory, Visited),
    (nonvar(Theory) ->
        collect_theories([Theory | Visited], Theories)
        ;
        Theories = Visited
    ).

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
maybe_keep_theory(RatedTheory, _, _, _, KeptTheories, KeptTheories) :-
    Coverage-_ = RatedTheory.rating,
    Coverage == 0,
    log(info, apperception_engine, 'DUD!'),
    !.
% Throw an exception if a perfect theory is found to halt search
maybe_keep_theory(RatedTheory, ApperceptionLimits, Template, Epoch, _, _) :-
    Coverage-_ = RatedTheory.rating,
    Coverage >= ApperceptionLimits.good_enough_coverage,
    put_dict(template, RatedTheory, Template, RatedTheory1),
    get_time(Now),
    FoundTime is round(Now - Epoch),
    put_dict(found_time, RatedTheory1, FoundTime, RatedTheory2),
    abort_with_best_theories(found_good_enough_theory, [RatedTheory2]).   

maybe_keep_theory(RatedTheory, ApperceptionLimits, Template, _, KeptTheories, [RatedTheoryWithTemplate | KeptTheories]) :-
    length(KeptTheories, L),
    L < ApperceptionLimits.keep_n_theories, 
    put_dict(template, RatedTheory, Template, RatedTheoryWithTemplate),
    !,
    log(warn, apperception_engine, 'Keeping new theory with rating ~p (under quota)', [RatedTheory.rating]).

maybe_keep_theory(RatedTheory, _, Template, _, KeptTheories, UpdatedKeptTheories) :-
    add_if_better(RatedTheory, Template, KeptTheories, UpdatedKeptTheories).

add_if_better(RatedTheory, Template, BestTheoriesSoFar, [RatedTheoryWithTemplate | Others]) :-
    find_worse(BestTheoriesSoFar, RatedTheory.rating, WorseTheory),
    !,
    put_dict(template, RatedTheory, Template, RatedTheoryWithTemplate),
    log(info, apperception_engine, 'Keeping better theory with rating ~p (better one)', [RatedTheory.rating]),
    delete(BestTheoriesSoFar, WorseTheory, Others).

add_if_better(_, _, BestTheoriesSoFar, BestTheoriesSoFar).

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
      % Not '=' since sorting removes duplicates
      Delta = '<'
    ).

better_or_same_rating(Rating1, Rating2) :-
    Rating1 == Rating2
    ;
    % Finds '<' if equal - to prevent duplicate removal when sorting theories on their ratings
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
