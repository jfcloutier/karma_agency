# Pengines

See [erl_pengines](https://github.com/Limmen/erl_pengine)

## Starting a SWI-PROLOG Pengine server

    > cd pengines-master
    > swipl
    ?- [load].
    ?- server(3030).

    Point browser to http://localhost:3030/ 
    login at user = "admin", password = "andy" - include the double-quotes!

## Accessing the SWI-PROLOG pengine from Elixir

    # Create a pengine on an application. Keep it alive.
    iex> {{:ok, {pid, id}}, other} = :pengine_master.create_pengine('http://localhost:3030/pengine', %{application: 'genealogist', destroy: false})

    # List of current pengines
    iex> [{pid, id}] = :pengine_master.list_pengines()

    # This will assert safely
    iex> {:success, id, [%{}], false} = :pengine.ask(pid, 'assert_father_child(jf, will)', %{})
    iex> {:success, id, [%{}], false} = :pengine.ask(pid, 'assert_mother_child(liz, will)', %{})

    # Query and get one answer
    iex> {:success, id, answer, more_solutions?} = :pengine.ask(pid, 'ancestor_descendant(X, Y)', %{template: '[X, Y]', chunk: '1'})

    # Get next answer
    iex> {:success, id, answers, more_solutions?} = :pengine.next(pid)

    # Ran out of answers
    iex> {:failure, id} = :pengine.next(pid)

    # Bad query causes an error and stops the pengine
    iex> {:error, id, message_string, error_type_string} = :pengine.ask(pid, 'nonsense(X, [1,2, 3])', %{template: '[X]', chunk: '1'})

    # Terminate the pengine
    {:pengine_destroyed, _message} = :pengine.destroy(pid)

## Calls from Andy

### Assertions

    latest_completed_round(RoundIndex) % Communicated by a GM in Andy at the end of a round
                                       % Inferred round_data is asserted and added to the communicated data for the completed round
    round_data(RoundIndex, Data) % round data as it is being communicated by other GMs (data is a sent_prediction or received_prediction error as string)

Round Index is the index of the round where the belief etc. is held, e.g. `5`
The data is either a belief, a prediction, prediction error or action

### Queries

    current_beliefs(Beliefs) % What are the GM's current beliefs based on current perceptions?
    predictions_to_send(Predictions) % What are the GM's current predictions based on beliefs?
    prediction_errors_to_send(PredictionErrors) % What are the GM's current prediction errors based on current beliefs and received predictions?
    actions_to_take(Actions) % What actions does the theory recommend to achieve sent goal beliefs and test current opinion beliefs?
