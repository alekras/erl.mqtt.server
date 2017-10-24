%% @author alexei
%% @doc @todo Add description to mqtt_ws_handler.


-module(mqtt_ws_handler).
-include_lib("mqtt_common/include/mqtt.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	init/2,
	websocket_init/1,
	websocket_handle/2,
	websocket_info/2,
	terminate/3,
	send/2,
	close/1
]).

init(Req0, State) ->
	lager:debug("web socket init/2: ~p~n~p", [Req0, State]),	
	case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req0) of
		undefined ->
			{cowboy_websocket, Req0, #{}};
		Subprotocols ->
			lager:debug("Subprotokols: ~p", [Subprotocols]),	
			case lists:member(<<"mqttv3.1">>, Subprotocols) of
				true ->
					Req = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>, <<"mqttv3.1">>, Req0),
					{cowboy_websocket, Req, #{}};
				false ->
					Req = cowboy_req:reply(400, Req0),
					{ok, Req, #{}}
			end
	end.

websocket_init(State) ->
	Opts = ranch:get_protocol_options(ws_listener),
	lager:debug("websocket_init/1: ~p~n~p", [State, Opts]),
	Storage = maps:get(storage, Opts, mqtt_dets_dao),
  Conn_State = #connection_state{socket = self(), transport = mqtt_ws_handler, storage = Storage, end_type = server},
 	Conn_Pid = proc_lib:spawn_link(fun() -> mqtt_connection:init(Conn_State) end),
	State1 = maps:put(conn_pid, Conn_Pid, State),
	{ok, State1}.

websocket_handle({binary, Binary} = _Frame, State) ->
	lager:debug("1) websocket_handle/1: ~p~n~p", [_Frame, State]),
	Pid = maps:get(conn_pid, State, undefined),
	Pid ! {tcp, self(), Binary},
	{ok, State};
websocket_handle(_Frame, State) ->
	lager:debug("2) websocket_handle/1: ~p~n~p", [_Frame, State]),
	{ok, State}.

websocket_info({out, Packet} = _Info, State) ->
	lager:debug("websocket_info/1: ~p~n~p", [_Info, State]),	
	{reply, {binary, Packet}, State};
websocket_info({'EXIT', _Pid, normal} = _Info, State) ->
	lager:debug("websocket_info/1: ~p~n~p", [_Info, State]),	
	{stop, State};
websocket_info(_Info, State) ->
	lager:debug("websocket_info/1: ~p~n~p", [_Info, State]),	
	{ok, State}.
%% websocket_info(_Info, State) ->
%% 	{reply, [
%% 			{text, "Hello"},
%% 			{text, <<"world!">>},
%% 			{binary, <<0:8000>>}
%% 	], State}.

terminate(_Reason, _Req, State) ->
	lager:debug("terminate/3: ~p~n~p~n~p", [_Reason, _Req, State]),	
	Pid = maps:get(conn_pid, State, undefined),
	Pid ! {tcp_closed, self()},
	ok.

send(Socket, Packet) ->
	Socket ! {out, Packet},
	ok.
	
close(_Socket) ->
	ok. %% @todo close websocket ???
%% ====================================================================
%% Internal functions
%% ====================================================================


