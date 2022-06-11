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
-module(mqtt_rest_server).

-define(DEFAULT_LOGIC_HANDLER, mqtt_rest_default_logic_handler).

-export([start/2]).

-spec start(ID :: any(), #{
	ip			=> inet:ip_address(),
	port		  => inet:port_number(),
	logic_handler => module(),
	net_opts	  => []
}) -> {ok, pid()} | {error, any()}.

start(ID, #{
	ip        := IP,
	port      := Port,
	net_opts  := NetOpts
} = Params) ->
	
	{Transport, TransportOpts} = get_socket_transport(IP, Port, NetOpts),
	LogicHandler = maps:get(logic_handler, Params, ?DEFAULT_LOGIC_HANDLER),
	ExtraOpts = maps:get(cowboy_extra_opts, Params, []),
	CowboyOpts = get_cowboy_config(LogicHandler, ExtraOpts),
	case Transport of
		ssl ->
			cowboy:start_tls(ID, TransportOpts, CowboyOpts);
		tcp ->
			cowboy:start_clear(ID, TransportOpts, CowboyOpts)
	end.

get_socket_transport(IP, Port, Options) ->
	Opts = [
		{ip,   IP},
		{port, Port}
	],
	case mqtt_rest_utils:get_opt(ssl, Options) of
		SslOpts = [_|_] ->
			{ssl, Opts ++ SslOpts};
		undefined ->
			{tcp, Opts}
	end.

get_cowboy_config(LogicHandler, ExtraOpts) ->
	get_cowboy_config(LogicHandler, ExtraOpts, get_default_opts(LogicHandler)).

get_cowboy_config(_LogicHandler, [], Opts) ->
	Opts;
get_cowboy_config(LogicHandler, [{env, Env} | Rest], Opts) ->
	NewEnv = case proplists:get_value(dispatch, Env) of
		undefined -> [get_default_dispatch(LogicHandler) | Env];
		_ -> Env
	end,
	get_cowboy_config(LogicHandler, Rest, store_key(env, NewEnv, Opts));
get_cowboy_config(LogicHandler, [{Key, Value}| Rest], Opts) ->
	get_cowboy_config(LogicHandler, Rest, store_key(Key, Value, Opts)).

get_default_dispatch(LogicHandler) ->
	Paths = mqtt_rest_router:get_paths(LogicHandler),
	#{dispatch => cowboy_router:compile(Paths)}.

get_default_opts(LogicHandler) ->
	#{env => get_default_dispatch(LogicHandler)}.

store_key(Key, Value, Opts) ->
	maps:put(Key, Value, Opts).
