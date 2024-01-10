%%
%% Copyright (C) 2017-2023 by krasnop@bellsouth.net (Alexei Krasnopolski)
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
%% @copyright 2017-2023 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc Main module of the mqtt_server application.


-module(mqtt_server).
-behaviour(application).

-include_lib("mqtt_common/include/mqtt.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	start/2,
	stop/1
]).

-define(NUM_ACCEPTORS_IN_POOL, 2).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
%% @private
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
	{ok, Pid :: pid()}
	| {ok, Pid :: pid(), State :: term()}
	| {error, Reason :: term()}.
%% ====================================================================
start(_Type, _Args) ->
	application:ensure_started(mqtt_common),
	application:ensure_started(lager),
	application:ensure_started(sasl),
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

% for debug >
	A = application:get_all_env(lager),
	lager:debug("lager config env: ~p",[A]),	
	lager:debug("mqtt_server config env: ~p",[application:get_all_env(mqtt_server)]),	
%	< for debug

	Storage =
	case application:get_env(mqtt_server, storage, dets) of
		mysql -> mqtt_mysql_storage;
		dets -> mqtt_dets_storage
	end,
	Storage:start(server),
	Storage:cleanup(server), %% TODO is it suitable for sessions?
	Echo = Storage:user(get, <<"echo">>),
	if (Echo =:= undefined) ->
				Storage:user(save, #user{user_id = <<"echo">>, password = <<"echo">>, roles = [<<"USER">>]});
			true -> ok
	end,
	Guest = Storage:user(get, <<"guest">>),
	if (Guest =:= undefined) ->
				Storage:user(save, #user{user_id = <<"guest">>, password = <<"guest">>, roles = [<<"USER">>]});
			true -> ok
	end,
	
	Port = application:get_env(mqtt_server, port, 1883),
	Port_tls = application:get_env(mqtt_server, port_tls, 1884),
	Port_ws = application:get_env(mqtt_server, port_ws, 8080),
	Port_wss = application:get_env(mqtt_server, port_wss, 4443),
	Cert_File = application:get_env(mqtt_server, certfile, "tls/server.crt"),
	CA_Cert_File = application:get_env(mqtt_server, cacertfile, "tls/ca.crt"),
	Key_File = application:get_env(mqtt_server, keyfile, "tls/server.key"),
	Verify_peer = application:get_env(mqtt_server, verify, verify_none),
	lager:info("TLS config files: ~p~n",[{Cert_File, CA_Cert_File, Key_File,Verify_peer}]),	
	case filelib:is_regular(Cert_File) of
		true -> ok;
		false -> 
			lager:error("Certificate file does not exist: ~p.", [Cert_File])
	end,
	case filelib:is_regular(CA_Cert_File) of
		true -> ok;
		false ->
			lager:error("CA Certificate file does not exist: ~p.", [CA_Cert_File])
	end,
	case filelib:is_regular(Key_File) of
		true -> ok;
		false ->
			lager:error("Key file does not exist: ~p.", [Key_File])
	end,
%% 	B0 = application:start(cowlib),
%% 	lager:debug("After Cowlib start: ~p",[B0]),	
%% 	B1 = application:start(cowboy),
%% 	lager:debug("After Cowboy start: ~p",[B1]),

	S = lists:concat([io_lib:format("    ~p~n",[App]) || App <- application:which_applications()]),
	lager:info([{endtype, server}], "running apps: ~n~s",[S]),	
%% 	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Shutdown, Type :: worker | supervisor, Modules},
%% 	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
%% 	RestartPolicy :: permanent
%% 				   | transient
%% 				   | temporary,
%% 	Shutdown :: brutal_kill | timeout(),
%% 	Modules :: [module()] | dynamic.
	RanchSupSpec = {
				ranch_sup, 
				{ranch_sup, start_link, []},
				permanent, 
				5000, 
				supervisor, 
				[ranch_sup]
	},
	CowboyClock = {
				cowboy_clock,
				{cowboy_clock, start_link, []},
				permanent,
				5000, 
				worker, 
				[cowboy_clock]
	},
	TCPListenerSpec = ranch:child_spec(
				mqtt_server, 
				ranch_tcp, 
				#{
					socket_opts => [{port, Port}],
					num_acceptors => ?NUM_ACCEPTORS_IN_POOL
				}, 
				mqtt_server_connection, 
				[{storage, Storage}]
	),
	TLSListenerSpec = ranch:child_spec(
				mqtt_server_tls, 
				ranch_ssl, 
				#{
					num_acceptors => ?NUM_ACCEPTORS_IN_POOL,
					socket_opts => [
						{port, Port_tls},
						{certfile, Cert_File},
						{cacertfile, CA_Cert_File},
						{keyfile, Key_File},
						{depth, 2},
%%					{server_name_indication, disable}, 
						{verify, Verify_peer}
					]
				}, 
				mqtt_server_connection, 
				[{storage, Storage}]
	),
%%	lager:debug("TLSListenerSpec: ~p~n",[TLSListenerSpec]),	

%% Web socket connection
	Dispatch = cowboy_router:compile([
		{'_', [
						{"/", mqtt_ws_handler, []}, 
						{"/:protocol", mqtt_ws_handler, []}
					]
		}
	]),

	WSListener = ranch:child_spec(
				ws_listener, 
				ranch_tcp, 
				#{
					socket_opts => [{port, Port_ws}],
					num_acceptors => ?NUM_ACCEPTORS_IN_POOL,
					connection_type => supervisor
				},
				cowboy_clear, 
				#{env => #{dispatch => Dispatch}, 
					connection_type => supervisor,
					storage => Storage
				}
	),
	
	WSSListener = ranch:child_spec(
				wss_listener, 
				ranch_ssl, 
				#{port => Port_wss,
					num_acceptors => ?NUM_ACCEPTORS_IN_POOL,
					socket_opts => [
						{port, Port_wss},
						{certfile, Cert_File},
						{cacertfile, CA_Cert_File},
						{keyfile, Key_File},
						{verify, Verify_peer}
					],
					connection_type => supervisor,
					next_protocols_advertised => [<<"h2">>, <<"http/1.1">>],
					alpn_preferred_protocols => [<<"h2">>, <<"http/1.1">>]
				}, 
				cowboy_tls, 
				#{env => #{dispatch => Dispatch}, 
					connection_type => supervisor,
					storage => Storage
				}
	),
	
	mqtt_server_sup:start_link([
		RanchSupSpec, 
		CowboyClock, 
		TCPListenerSpec, TLSListenerSpec, 
		WSListener, WSSListener]).

%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
%% @private
stop(_State) ->
	ok = ranch:stop_listener(mqtt_server),
	ok = ranch:stop_listener(mqtt_server_tls),
	ok = ranch:stop_listener(ws_listener),
	ok = ranch:stop_listener(wss_listener).

%% ====================================================================
%% Internal functions
%% ====================================================================
