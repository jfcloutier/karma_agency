:- prolog_load_context(directory, Dir),
   string_concat(Dir, '/code', Code),
   asserta(user:file_search_path(code, Code)),
   asserta(user:file_search_path(actor_model, code('actor_model'))),
   asserta(user:file_search_path(agent, code('agent'))),
   asserta(user:file_search_path(apperception, code('apperception'))),
   string_concat(Dir, '/tests', Tests),
   asserta(user:file_search_path(tests, Tests)).
