/*
The interface to the robot's effectors and sensors.
*/

:- module(body, [get_sensors/2, get_effectors/2]).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).

    
capabilities(Sensors, Effectors) :-
    host(Host),
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

api_url(Host, Query, Url) :-
    format(atom(Url), 'http://~w/api/~w', [Host, Query]).

devices_from_response(json([_=Json]), Tag, Devices) :-
    devices_from_json(Json, Tag, Devices).

devices_from_json([], _, []).
devices_from_json([JsonDevice | Others], Tag, [Device | OtherDevices]) :-
    device_from_json(JsonDevice, Tag, Device),
    devices_from_json(Others, Tag, OtherDevices).

device_from_json(json(KVPairs), Tag, Device) :-
    dict_create(Device, Tag, KVPairs).