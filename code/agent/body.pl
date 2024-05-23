/*
The interface to the robot's effectors and sensors.
*/

:- module(body, []).

:- use_module(code(logger)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).

capabilities(Host, Sensors, Effectors) :-
    log(info, body, 'Getting sensors and efectors'),
    sensors(Host, Sensors),
    effectors(Host, Effectors).

sensors(Host, Sensors) :-
    api_url(Host, sensors, Url),
    http_get(Url, Response, []),
    devices_from_response(Response, sensor, Sensors).

effectors(Host, Effectors) :-
    api_url(Host, effectors, Url),
    http_get(Url, Response, []),
    devices_from_response(Response, effector, Effectors).

sense_value(Url, Value) :-
    http_get(Url, Response, []),
    sense_value_from_response(Response, Value).

api_url(Host, Query, Url) :-
    format(atom(Url), 'http://~w/api/~w', [Host, Query]).

devices_from_response(json([_=Json]), Tag, Devices) :-
    devices_from_json(Json, Tag, Devices).

devices_from_json([], _, []).
devices_from_json([json(JsonDevice) | Others], Tag, [Device | OtherDevices]) :-
    json_to_dict(JsonDevice, Tag, Device),
    devices_from_json(Others, Tag, OtherDevices).

sense_value_from_response(json([_=Value]), Value).

json_to_dict(KVs,Tag, Dict) :-
    convert_json_pairs(KVs, [], Pairs),
    dict_create(Dict, Tag, Pairs).

convert_json_pairs([], Pairs, Pairs).
convert_json_pairs([Tag=json(KVs) | Rest], Acc, Pairs) :-
    !,
    json_to_dict(KVs, Tag, Dict),
    convert_json_pairs(Rest, [Tag=Dict | Acc], Pairs).
convert_json_pairs([KV | Rest], Acc, Pairs) :-
    convert_json_pairs(Rest, [KV | Acc], Pairs).

