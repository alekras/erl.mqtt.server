%% @author alexei
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
%	A = application:get_all_env(lager),
	case application:get_env(lager, log_root) of
		{ok, _} -> ok;
		undefined ->
			application:set_env(lager, log_root, "logs", [{persistent, true}]),
			application:set_env(lager, error_logger_redirect, false, [{persistent, true}]),
			application:set_env(lager, handlers, [{lager_console_backend, debug}], [{persistent, true}]),
			application:stop(lager),
			lager:start()
%% 	A = application:get_all_env(lager),
%%   io:format(user, " >>> lager env: ~p~n", [A])
	end,

	application:load(sasl),
%	lager:debug("running apps: ~p",[application:which_applications()]),	
	Storage =
	case application:get_env(mqtt_server, storage, dets) of
		mysql -> mqtt_mysql_dao;
		dets -> mqtt_dets_dao
	end,
	Storage:start(server),

	ok = application:start(ranch),
	RR = ranch:start_listener(
							mqtt_server, 
							10,
							ranch_tcp, 
							[{port, 18883}], 
							mqtt_server_connection, 
							[]
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
							[]
						),
	lager:info([{endtype, server}], "ranch:start_listener for SSL returns ~p~n", [RS]),
	mqtt_server_sup:start_link().

stop(_State) ->
	ok = ranch:stop_listener(mqtt_server),
	ok = ranch:stop_listener(mqtt_server_tls).

%% ====================================================================
%% Internal functions
%% ====================================================================


