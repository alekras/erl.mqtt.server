%%
%% Copyright (C) 2017 by krasnop@bellsouth.net (Alexei Krasnopolski)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%		 http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License. 
%%

%% @since 2017-01-11
%% @copyright 2017 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc @todo Add description to mqtt_server.


-module(mqtt_server).
-behaviour(application).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	lager:start(),
	case application:get_env(lager, log_root) of
		{ok, _} -> ok;
		undefined ->
			application:set_env(lager, log_root, "logs", [{persistent, true}]),
			application:set_env(lager, crash_log, "logs/crash.log", [{persistent, true}]),
			application:set_env(lager, error_logger_redirect, false, [{persistent, true}]),
			application:set_env(lager, handlers, [{lager_console_backend, debug}], [{persistent, true}]),
			application:stop(lager),
			lager:start()
	end,
	A = application:get_all_env(lager),
	lager:debug("lager config env: ~p",[A]),	

	application:load(sasl),
	Storage =
	case application:get_env(mqtt_server, storage, dets) of
		mysql -> mqtt_mysql_dao;
		dets -> mqtt_dets_dao
	end,
	Storage:start(server),

	ok = application:start(ranch),
	lager:debug("running apps: ~p",[application:which_applications()]),	
	RR = ranch:start_listener(
							mqtt_server, 
							10,
							ranch_tcp, 
							[{port, 18883}], 
							mqtt_server_connection, 
							[{storage, Storage}]
						),
	lager:info([{endtype, server}], "ranch:start_listener for TCP returns ~p~n", [RR]),
	RS = ranch:start_listener(
							mqtt_server_tls, 
							10,
							ranch_ssl, 
							[	{port, 18483},
								{certfile, "tsl/server.crt"},
								{cacertfile, "tsl/ca.crt"},
								{keyfile, "tsl/server.key"},
								{verify, verify_peer}
							], 
							mqtt_server_connection, 
							[{storage, Storage}]
						),
	lager:info([{endtype, server}], "ranch:start_listener for SSL returns ~p~n", [RS]),
	mqtt_server_sup:start_link().

stop(_State) ->
	ok = ranch:stop_listener(mqtt_server),
	ok = ranch:stop_listener(mqtt_server_tls).

%% ====================================================================
%% Internal functions
%% ====================================================================


