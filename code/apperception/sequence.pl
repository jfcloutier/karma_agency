:- module(sequence, [time_range/3, state/3, sequence/2]).

time_range(ObservationsModule, StartTime, EndTime) :-
    memberchk(ObservationKind, [sensed, enacted]),
    Goal =.. [ObservationKind, Pred, Args, Time],
    ModuleGoal =.. [:, ObservationsModule, Goal],
    setof(Time, ObservationKind^Pred^Args^ModuleGoal, Times),
    min_list(Times, StartTime),
    max_list(Times, EndTime).

state(ObservationsModule, Time, State) :-
    memberchk(ObservationKind, [sensed, enacted]),
    Goal =.. [ObservationKind, _, _, Time],
    ModuleGoal =.. [:, ObservationsModule, Goal],
    setof(Goal, ModuleGoal, State), !.

state(_, _, []).

sequence(ObservationsModule, Sequence) :-
    time_range(ObservationsModule, StartTime, EndTime),
    findall(State, 
            (between(StartTime, EndTime, Time), state(ObservationsModule, Time, State)), 
            Sequence).

% cd('sandbox/prototypes/apperception').
% [leds_observations, sequence].
% sequence(leds_observations, Sequence).