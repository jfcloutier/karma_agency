
/*
[load].
['tests/logger.plt'].
run_tests.
*/

:- begin_tests(logger).

:- use_module(code(logger)).

test(logger_defaults) :-
	reset_logging, 
	log(debug, test, 'debug level is hidden'), 
	log(debug, test, 'debug level is hidden with ~p', [1]), 
	log(warn, test, 'warn level is hidden'), 
	log(warn, test, 'warn level is hidden with ~p', [1]), 
	log(note, test, 'note level shows'), 
	log(note, test, 'note level shows with ~p', [1]), 
	logger : level(Level), 
	assertion(Level == note), 
	logger : ignored(AllIgnored), 
	assertion(AllIgnored == []).

test(logger_level) :-
	reset_logging, 
	set_log_level(info), 
	log(debug, test, 'debug level is hidden'), 
	log(info, test, 'info level shows with ~p', [1]), 
	log(warn, test, 'warn level shows'), 
	log(warn, test, 'error level shows with ~p', [1]), 
	log(note, test, 'note level shows with ~p', [1]), 
	logger : level(Level), 
	assertion(Level == info), 
	logger : ignored(AllIgnored), 
	assertion(AllIgnored == []).

test(logger_ignored) :-
	reset_logging, 
	set_log_level(warn), 
	ignore_log_topic(test1), 
	log(warn, test, 'warn topic test shows with ~p', [bla]), 
	log(warn, test1, 'warn topic test1 is hidden'), 
	log(info, test, 'info topic test is hidden with ~p', [bla]), 
	log(info, test1, 'info topic test1 is hidden'), 
	logger : ignored(AllIgnored), 
	assertion(AllIgnored == [test1]).

test(logger_options_param) :-
	reset_logging, 
	set_log_level(info), 
	log(info, test, 'This is a list ~p', [[level(1)]]).

test(logger_rest) :-
	reset_logging, 
	logger : level(Level), 
	assertion(Level == note), 
	logger : ignored(AllIgnored), 
	assertion(AllIgnored == []), 
	set_log_level(warn), 
	ignore_log_topic(test1), 
	logger : level(Level1), 
	assertion(Level1 == warn), 
	logger : ignored(AllIgnored1), 
	assertion(AllIgnored1 == [test1]), reset_logging, 
	logger : level(Level2), 
	assertion(Level2 == note), 
	logger : ignored(AllIgnored2), 
	assertion(AllIgnored2 == []).

:- end_tests(logger).