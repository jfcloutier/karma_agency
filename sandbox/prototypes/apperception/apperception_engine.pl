:- module(apperception_engine, [apperceive/3]).

:- use_module(global).
:- use_module(template_engine).
:- use_module(theory_engine).

%% An apperception engine.

%% Given a sequence of observations, it searches for theories (a logic program) best explaining the sequence.
%% The search completes after a preset amount of time or when a great theory is found.

%% The apperception engine searches by iterating through theory templates of roughly increasing complexity,
%% each template specifying a region of the search space of theories.
%% For each template, the apperception engine iterates through theories specified by the template and evaluates them.
%% It rejects those that are not unified and retains the highest rated ones found so far.

/*
cd('sandbox/prototypes/apperception').
[leds_observations, sequence, type_signature, domains, template_engine, theory_engine].
sequence(leds_observations, Sequence), 
min_type_signature(Sequence, MinTypeSignature), 
MaxSignatureExtension = max_extension{max_object_types:1, max_objects:1, max_predicate_types:2},
ApperceptionLimits = apperception_limits{max_signature_extension: MaxSignatureExtension, max_theories_per_template: 100, keep_theories: 3, time_secs: 60},
apperceive()
*/

% Given a sequence and some limits, find winning theories.
apperceive(Sequence, ApperceptionLimits, Theories) :-
    init_search(TheoryTemplateEngine, Search),
    min_type_signature(Sequence, MinTypeSignature),
    create_theory_template_engine(MinTypeSignature, ApperceptionLimits.max_signature_extension, TheoryTemplateEngine),
    find_best_theories(TheoryTemplateEngine, ApperceptionLimits, Search, Theories), !,
    engine_destroy(TheoryTemplateEngine),
    % Remove all global state produced during apperception
    delete_global(apperception).

% Iterate over templates, exploring a maximum of theories for each, validating theories found and keeping the best of the valid theories.
find_best_theories(TheoryTemplateEngine, ApperceptionLimits, Search, Theories) :-
    time_expired(ApperceptionLimits, Search),
    !,
    stop_search(TheoryTemplateEngine, Search),
    Theories = Search.theories.

find_best_theories(TheoryTemplateEngine, ApperceptionLimits, Search, Theories) :-   
    find_theory(TheoryTemplateEngine, ApperceptionLimits, Search, Theory, UpdatedSearch),
    make_trace(Theory, Trace),
    (theory_unified(Theory, Trace) -> 
        rate_theory(Sequence, Theory, Trace, Rating),
        maybe_keep_theory(Theory, Rating, ApperceptionLimits, UpdatedSearch, CurrentSearch)
        ; CurrentSearch = UpdatedSearch),
    find_best_theories(TheoryTemplateEngine, ApperceptionLimits, CurrentSearch, Theories).

init_search(TheoryTemplateEngine, Search) :-
    get_time(Now),
    engine_next(TheoryTemplateEngine, Template),
    create_theory_engine(Template, TheoryEngine),
    Search = search{started_at: Now, theories_count: 0, best_theories: [], template: Template, theory_engine: TheoryEngine}.

stop_search(TheoryTemplateEngine, Search) :-
    engine_destroy(TheoryTemplateEngine),
    engine_destroy(Search.theory_engine).

time_expired(ApperceptionLimits, Search) :-
    get_time(Now),
    time_spent is Now - Search.started_at,
    time_spent > ApperceptionLimits.time_secs.

% Find a theory
find_theory(TheoryTemplateEngine, ApperceptionLimits, Search, Theory, CurrentSearch) :-
    maybe_change_template(TheoryTemplateEngine, ApperceptionLimits.max_theories_per_template, Search, UpdatedSearch),
    next_theory(UpdatedSearch, Theory, CurrentSearch),
    !.

maybe_change_template(TheoryTemplateEngine, MaxTheories, Search, UpdatedSearch) :-
    (max_theories_reached(MaxTheories, Search) ->
        next_theory_template(TheoryTemplateEngine, Search, UpdatedSearch); true).

max_theories_reached(MaxTheories, Search) :-
    Search.theories_count >= MaxTheories.

 next_theory_template(TheoryTemplateEngine, Search, UpdatedSearch) :-
    engine_destroy(Search.theory_engine),
    engine_next(TheoryTemplateEngine, Template),
    create_theory_engine(Template, TheoryEngine),
    UpdatedSearch = search{started_at: Search.started_at, theories_count: 0, best_theories: Search.best_theories, template: Template, theory_engine: TheoryEngine}).

next_theory(Search, Theory, UpdatedSearch) :-
    engine_next(Search.theory_engine, Theory),
    Inc is Search.theories_count + 1,
    put_dict(theories_count, Search, Inc, UpdatedSearch).
    
% TODO
make_trace(Theory, Trace).

% TODO
theory_unified(Theory, Trace).

% TODO  
rate_theory(Sequence, Theory, Trace, Rating).

% TODO   
maybe_keep_theory(Theory, Rating, ApperceptionLimits, Search, UpdatedSearch).