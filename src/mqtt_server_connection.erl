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
%% @doc @todo Add description to mqtt_server_connection.


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
	Storage = proplists:get_value(storage, _Opts, mqtt_dets_dao),
	ok = Transport:setopts(Socket, [{active, true}]),
 	State = #connection_state{socket = Socket, transport = Transport, storage = Storage, end_type = server},
	{ok, proc_lib:spawn_link(fun() -> ok = ranch:accept_ack(Ref), mqtt_connection:init(State) end)}.

