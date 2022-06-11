%%
%% Copyright (C) 2017-2022 by krasnop@bellsouth.net (Alexei Krasnopolski)
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
%% @copyright 2017-2022 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc The module implements ranch_protocol behaviour. This is start point to create
%% socket connection process that keeps connection to MQTT client.

-module(mqtt_server_connection).

-behaviour(ranch_protocol).

%%
%% Include files
%%
-include_lib("mqtt_common/include/mqtt.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
% ranch_protocol
-export([start_link/4]).

-spec start_link(Ref :: atom(), Socket :: port(), Transport :: module(), Opts :: list()) -> {ok, Pid :: pid()}.
%% @doc The function starts mqtt_connection process.
start_link(Ref, _Socket, Transport, Opts) ->
	{ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Transport, Opts}])}.

init({Ref, Transport, Opts}) ->
	%% Perform any required state initialization here.
	{ok, Socket} = ranch:handshake(Ref),
	Storage = proplists:get_value(storage, Opts, mqtt_dets_dao),
	ok = Transport:setopts(Socket, [{active, true}]),
 	State = #connection_state{socket = Socket, transport = Transport, storage = Storage, end_type = server},
	mqtt_connection:init(State).
