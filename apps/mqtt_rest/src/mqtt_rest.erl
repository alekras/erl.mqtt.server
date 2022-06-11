%%
%% Copyright (C) 2015-2022 by krasnop@bellsouth.net (Alexei Krasnopolski)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License. 
%%
-module(mqtt_rest).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	lager:info([{endtype, server}], "Start mqtt_rest app = ~p.~n", [_Args]),	
	mqtt_rest_server:start(mqtt_rest, #{ip=>{127,0,0,1}, port=>8080, net_opts=>[]}).

stop(_State) ->
		ok = ranch:stop_listener(mqtt_rest).
