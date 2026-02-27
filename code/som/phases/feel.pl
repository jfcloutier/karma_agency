/*
Assign a feeling (from worst to best) to each experience.

Compute an overall feeling for the current timeframe then assign a fraction of it to each experience,
based on how persistent the experience is.

About feelings:

* Feelings are causative: A CA pays attention to the experiences it feels the most and then acts accordingly.
* Feelings compress wellbeing information (now and remembered) into integers between -10 (worst) and 10 (best).
* A feeling is associated to each CA timeframe as a whole and, partially, to each experience within that timeframe.
* A persisting experience (persisting over consecutive timeframes) is felt more strongly than a more recent experience.

A feeling reflects the current and historical wellbeing of a CA:

* Each wellbeing dimension (fullness, integrity, engagement) is felt separately.
* It is felt both in the moment (static) and in how it changed or stopped changing (dynamic).
* A CA feels only the more strongly felt wellbeing dimension at any time.

Computing a timeframe's feeling:

* Dynamic feelings (feelings from changes), if any, override static feelings.
* A wellbeing dimension's up gradient and an interrupted down gradient have positive feeling. A down gradient and an interrupted up gradient have negative feeling.
* The sustained absence of a wellbeing gradient feels like nothing.
* A gradient is felt more strongly than its interruption.
* A long-lived gradient is felt more strongly than a short-lived one.
* The interruption of a long-lived gradient is felt longer than that of a short-lived one.
* A gradient or its interruption is felt more strongly whenever the static feeling is low (heightened sensitivity).

The more sustained an experience is, the more it is felt.
The current feeling of a long-lived experience approaches asymptotically (as a function of its duration) the feeling of the current timeframe.

* Before work
    * Select all observations from static CAs not composing objects in current experiences
    * Add them as experiences with unchanged confidence

*/

:- module(feel, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).

% Elevate as observations from static CAs?
% No work done before units of work
% No work done before units of work
before_work(_, _, [], WellbeingDeltas) :-
    wellbeing:empty_wellbeing(WellbeingDeltas).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(StateDeltas, WellbeingDeltas) or done(StateDeltas, WellbeingDeltas) as last solution. 
unit_of_work(CA, State, done(StateDeltas, WellbeingDeltas)) :-
    timeframe_feeling(CA, State, TimeframeFeeling),
    felt_experiences(CA, State, TimeframeFeeling, FeltExperiences),
    StateDeltas = [feeling=TimeframeFeeling, experiences=FeltExperiences],
    % Computing and assigning feelings does not alter the current wellbeing
    wellbeing:empty_wellbeing(WellbeingDeltas),
    log(info, feel, "Phase feel ended for CA ~w", [CA]).


% Captures as a bounded, scalar integer value the overall feeling of the CA's current timeframe.
% The weightier contribution by a wellbeing dimension gives the overall feeling value.
timeframe_feeling(CA, State, Feeling) :-
    findall(WellbeingFeeling, (wellbeing:dimension(Dimension), wellbeing_dimension_feeling(CA,State, Dimension, WellbeingFeeling)), WellbeingFeelings),
    weighted_abs_max(WellbeingFeelings, Feeling),
    log(info, feel, "The timeframe feeling for ~w is ~w", [CA, Feeling]).

% A negative value weighs more than its absolute value
weighted_abs_max([Val | Rest], Max) :-
    weighted_abs_max(Rest, Val, Max).

weighted_abs_max([], Max, Max).
weighted_abs_max([Val | Rest], Acc, Max) :-
    weighted(Val, Val1),
    weighted(Acc, Acc1),
    abs(Val1) > abs(Acc1) ->
        weighted_abs_max(Rest, Val, Max)
        ;
        weighted_abs_max(Rest, Acc, Max).

% A negative value weighs twice as much as its absolute value
weighted(Val, Val) :-
    Val >= 0.
weighted(Val, Val1) :-
    Val < 0,
    Val1 is Val * 2.

% The contribution of a wellbeing dimension to the timeframe's feeling.
% Dynamic overrides static.
wellbeing_dimension_feeling(CA, State, Dimension, WellbeingDimensionFeeling) :-
    StaticValue = State.get(wellbeing/Dimension),
    static_wellbeing_dimension_feeling(StaticValue, StaticFeeling),
    dynamic_wellbeing_dimension_feeling(CA, State, Dimension, StaticValue, StaticFeeling, DynamicFeeling),
    (DynamicFeeling == 0 ->
      WellbeingDimensionFeeling = StaticFeeling
      ;
      WellbeingDimensionFeeling = DynamicFeeling
    ),
    log(info, feel, "(~w) The feeling of dimension ~w is ~w", [CA, Dimension, WellbeingDimensionFeeling]).

% The feeling of a wellbeing value is an integer between -10 (worst) and 10 (best).
static_wellbeing_dimension_feeling(StaticValue, Feeling) :-
    Deviation is StaticValue - 0.5,
    Feeling is round(Deviation * 20).

% The feeling from the change history of a wellbeing dimension
dynamic_wellbeing_dimension_feeling(CA, State, Dimension, StaticValue, StaticFeeling, DynamicFeeling) :-
    wellbeing_dimension_history(State, Dimension, StaticValue, ValueHistory),
    gradient(CA, ValueHistory, Gradient, Repeats),
    gradient_feeling(Gradient, Repeats, StaticFeeling, DynamicFeeling),
    log(info, feel, "(~w) The dynamic feeling of wellbeing dimension ~w given value history ~p is ~w", [CA, Dimension, ValueHistory, DynamicFeeling]).

% History records for a wellbeing dimension, latest last
wellbeing_dimension_history(State, Dimension, StaticValue, ValueHistory) :-
    get_state(State, timeframes, Timeframes),
    findall(Value, (member(Timeframe, Timeframes), Value = Timeframe.wellbeing.Dimension), Values),
    reverse([StaticValue | Values], ValueHistory).

% Gradient detection and how many times the gradient has repeated    
% Gradient = up, down, up_stopped, down_stopped, none, and for how many consecutive timeframes
gradient(CA, ValueHistory, Gradient, Repeats) :-
    gradient_history(ValueHistory, [], GradientHistory),
    reverse(GradientHistory, ReversedGradientHistory),
    gradient_from_history(ReversedGradientHistory, Gradient, Repeats),
    log(info, feel, "(~w) The gradient is ~w * ~w given gradient history ~p", [CA, Gradient, Repeats, ReversedGradientHistory]).

gradient_history([], GradientHistory, GradientHistory).

gradient_history([Value | Rest], [], GradientHistory) :-
    gradient_history(Rest, [#{value:Value, gradient:none, repeats:0}], GradientHistory).

gradient_history([Value | Rest], Acc, GradientHistory) :-
    [Prior | _] = Acc,
    gradient_from_values(Value, Prior.value, Gradient),
    repeats_from_prior(Gradient, Prior, Repeats),
    gradient_history(Rest, [#{value:Value, gradient:Gradient, repeats:Repeats} | Acc], GradientHistory).

gradient_from_values(Value, PriorValue, up) :-
    Value > PriorValue.

gradient_from_values(Value, PriorValue, down) :-
    Value < PriorValue.

gradient_from_values(Value, PriorValue, none) :-
    Value == PriorValue.

repeats_from_prior(Gradient, Prior, Repeats) :-
    (Gradient == Prior.gradient ->
        Repeats is Prior.repeats + 1
        ;
        Repeats = 0).

% Getting the type of gradient from the history of a wellbeing dimension.
% Gradient history is latest first
% An up duration of N timeframes that is stopped (no change since after going up) is up_stopped for up to N timeframes
gradient_from_history([], none, 0).

gradient_from_history([Latest | _], Gradient, Repeats) :-
    Latest.gradient \= none,
    Gradient = Latest.gradient,
    Repeats = Latest.repeats.

% Has a gradient stopped? Which? How many repeats?
gradient_from_history([Latest | Rest], Gradient, Repeats) :-
    Latest.gradient == none,
    Repeats = Latest.repeats,
    % This gets us to the first record that has a gradient(not none), if there is one
    (nth0(Repeats, Rest, GradientRecord) ->
        #{value:PriorGradient, repeats:GradientRepeats} :< GradientRecord,
        % Has the gradient stop shorter than the gradient run, if so, still *_stopped
        (Repeats =< GradientRepeats ->
            (PriorGradient == up ->  Gradient = up_stopped ; Gradient = down_stopped)
            ;
            Gradient = none)
        ;
        Gradient = none
    ).

% The feeling associated with the current gradient and how long it has lasted.
gradient_feeling(none, _, _, 0) :- !.

% The gradient feeling is the absolute intensity of the timeframe feeling equally modulated by
% * the type of gradient or interruption and its the duration
% * the sensitivity due to the positivity (lesser) or negativity (greater) of the timeframe feeling
gradient_feeling(Gradient, Repeats, StaticFeeling, DynamicFeeling) :-
    up_down_factor(Gradient, UpDownFactor),
    duration_factor(Repeats, DurationFactor),
    static_feeling_factor(StaticFeeling, StaticFactor),
    DynamicFeeling is round(abs(StaticFeeling) * ((UpDownFactor * DurationFactor * 0.5) + (StaticFactor * 0.5))),
    log(info, feel, "The gradient feeling is ~w given static feeling ~w and factors up-down ~w, duration ~w and static ~w", [DynamicFeeling, StaticFeeling, UpDownFactor, DurationFactor, StaticFactor]).

% An up/down gradient is felt twice as much, positively or negatively, as its interruption
up_down_factor(up, 1.0).
up_down_factor(down, -1.0).
up_down_factor(down_stopped, 0.5).
up_down_factor(up_stopped, -0.5).

% Amplify gradient-caused loss or gain if static feeling is negative 
static_feeling_factor(StaticFeeling, StaticFactor) :-
    StaticFeeling < 0 -> StaticFactor = 1.0 ; StaticFactor = 0.75.

% Amplification of a lasting gradient
% Approaches 1.0 with many repeats
duration_factor(Repeats, DurationFactor) :-
    asymptotic(1.0, Repeats, DurationFactor).

asymptotic(MaxY, X, Y) :-
    Y is MaxY * (X / (X + 1)).

% Adds a feeling value to each experience based on the current overall feeling
% and the history of the expeirence if it is persisting over multiple timeframes
felt_experiences(CA, State, Feeling, FeltExperiences) :-
    findall(FeltExperience, (member(Experience, State.experiences), felt_experience(CA, State, Feeling, Experience, FeltExperience)), FeltExperiences).

% Modulate the current feeling by the experience's history, and assign it to the experience.
felt_experience(CA, State, TimeframeFeeling, Experience, FeltExperience) :-
    experienced_feeling(CA, State, TimeframeFeeling, Experience, ExperiencedFeeling),
    FeltExperience = Experience.put(feeling, ExperiencedFeeling).

% The experience that has persisted longer is felt more than a more recent experience.
experienced_feeling(CA, State, TimeframeFeeling, Experience, ExperiencedFeeling) :-
    experience_duration(State.timeframes, Experience, Duration),
    % Asymptotically approaches the timeframe feeling as experience duration increases
    asymptotic(TimeframeFeeling, Duration, Y),
    ExperiencedFeeling is round(Y),
    log(info, feel, "(~w) Feeling of experience ~p with duration ~w is ~w given timeframe feeling ~w", [CA, Experience, Duration, ExperiencedFeeling, TimeframeFeeling]).

experience_duration([Timeframe | Rest], Experience, Duration) :-
    member(Experience1, Timeframe.experiences),
    same_experience(Experience, Experience1),
    !,
    experience_duration(Rest, Experience, Duration1),
    Duration is Duration1 + 1.
experience_duration(_, _, 1).

% Same experience if same origin, kind and value
same_experience(Experience, Other) :-
    experience{origin:Object, kind:Kind, value:Value} :< Experience,
    experience{origin:Object, kind:Kind, value:Value} :< Other.