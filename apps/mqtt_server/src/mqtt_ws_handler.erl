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
	lager:debug([{endtype, server}], "web socket starts req: ~p~n state~p~n", [Req0, State]),	
	case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req0) of
		undefined ->
			{cowboy_websocket, Req0, #{}, #{idle_timeout => 120000}};
		Subprotocols ->
			P1 = lists:member(<<"mqttv3.1.1">>, Subprotocols),
			P2 = lists:member(<<"mqttv3.1">>, Subprotocols),
			P3 = lists:member(<<"mqtt">>, Subprotocols),
			Protocol =
			if P1 -> <<"mqttv3.1.1">>;
				 P2 -> <<"mqttv3.1">>;
				 P3 -> <<"mqtt">>;
				 true -> undefined
			end,
			case Protocol of
				undefined ->
					Req = cowboy_req:reply(400, Req0),
					{ok, Req, #{}};
				_ ->
					Req = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>, Protocol, Req0),
					{cowboy_websocket, Req, #{}, #{idle_timeout => 120000}}
			end
	end.

websocket_init(State) ->
	Opts = ranch:get_protocol_options(ws_listener),
	Storage = maps:get(storage, Opts, mqtt_dets_dao),
  Conn_State = #connection_state{socket = self(), transport = mqtt_ws_handler, storage = Storage, end_type = server},
 	Conn_Pid = proc_lib:spawn_link(fun() -> mqtt_connection:init(Conn_State) end),
	State1 = maps:put(conn_pid, Conn_Pid, State),
	{ok, State1}.

websocket_handle({binary, Binary} = _Frame, State) ->
	Pid = maps:get(conn_pid, State, undefined),
	Pid ! {tcp, self(), Binary},
	{ok, State};
websocket_handle(_Frame, State) ->
	lager:warning([{endtype, server}], "unknown message: ~p~n~p", [_Frame, State]),
	{ok, State}.

websocket_info({out, Packet} = _Info, State) ->
	{reply, {binary, Packet}, State};
websocket_info({'EXIT', Pid, Reason}, State) ->
	lager:info([{endtype, server}], "get EXIT from pid: ~p reason: ~p state: ~p~n", [Pid, Reason, State]),	
	{stop, State};
websocket_info(close_ws, State) ->
	lager:info([{endtype, server}], "get close socket from connection, state: ~p~n", [State]),
	State1 = maps:put(conn_pid, undefined, State),
	{stop, State1};
websocket_info(_Info, State) ->
	lager:warning([{endtype, server}], "unknown message: ~p~n state:~p~n", [_Info, State]),	
	{ok, State}.

terminate(_Reason, _Req, State) ->
	Pid = maps:get(conn_pid, State, undefined),
	if is_pid(Pid) ->	Pid ! {tcp_closed, self()};
		 true -> ok
	end,
	ok.

send(WS_Handler_Pid, Packet) ->
	WS_Handler_Pid ! {out, Packet},
	ok.
	
close(WS_Handler_Pid) ->
	WS_Handler_Pid ! close_ws,
	ok.
%% ====================================================================
%% Internal functions
%% ====================================================================
