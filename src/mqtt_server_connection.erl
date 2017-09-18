%% @author alexei
%% @doc @todo Add description to mqtt_server_protocol.


-module(mqtt_server_connection).

-behaviour(ranch_protocol).

%%
%% Include files
%%
-include_lib("mqtt_common/include/mqtt.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
% ranch_protocol
-export([start_link/4]).

start_link(Ref, Socket, Transport, _Opts) ->
	Storage =
	case application:get_env(mqtt_server, storage, dets) of
		mysql -> mqtt_mysql_dao;
		dets -> mqtt_dets_dao
	end,
	ok = Transport:setopts(Socket, [{active, true}]),
 	State = #connection_state{socket = Socket, transport = Transport, storage = Storage, end_type = server},
	{ok, proc_lib:spawn_link(fun() -> ok = ranch:accept_ack(Ref), mqtt_connection:init(State) end)}.

