-module(mqtt_rest).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	lager:info([{endtype, server}], "Start mqtt_rest app = ~p.~n", [_Args]),	
	mqtt_rest_server:start(mqtt_rest, #{ip=>{127,0,0,1}, port=>8080, net_opts=>[]}).

stop(_State) ->
		ok = ranch:stop_listener(mqtt_rest).
