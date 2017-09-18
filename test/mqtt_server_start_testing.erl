%%
%% Copyright (C) 2015-2017 by krasnop@bellsouth.net (Alexei Krasnopolski)
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

%% @hidden
%% @since 2016-01-23
%% @copyright 2015-2017 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc This module is running unit tests for some modules.

-module(mqtt_server_start_testing).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include_lib("mqtt_common/include/mqtt.hrl").
-include("test.hrl").

%%
%% Import modules
%%
%-import(helper_common, []).

%%
%% Exported Functions
%%
-export([
]).

%%
%% API Functions
%%

unit_test_() ->
	[ 
		{"start", timeout, 10, fun start/0}
	].

start() ->
	S = application:start(mqtt_server),
	C = application:start(mqtt_client),
	P = mqtt_client:connect(
		publisher, 
		#connect{
			client_id = "publisher",
			user_name = "guest",
			password = <<"guest">>,
			will = 0,
			will_message = <<>>,
			will_topic = [],
			clean_session = 1,
			keep_alive = 1000
		}, 
		"localhost", 
		18883, 
		[]
	),
	SC1 = mqtt_client:connect(
		subscriber1, 
		#connect{
			client_id = "subscriber1",
			user_name = "guest",
			password = <<"guest">>,
			will = 0,
			will_message = <<>>,
			will_topic = [],
			clean_session = 1,
			keep_alive = 1000
		}, 
		"localhost", 
		18883, 
		[]
	),
	SC2 = mqtt_client:connect(
		subscriber2, 
		#connect{
			client_id = "subscriber2",
			user_name = "guest",
			password = <<"guest">>,
			will = 0,
			will_message = <<>>,
			will_topic = [],
			clean_session = 1,
			keep_alive = 1000
		}, 
		"localhost", 
		18883, 
		[]
	),
	?debug_Fmt("~n Server app=~p~n Client app=~p~n Connection=~p/~p/~p~n", [S,C,P,SC1,SC2]),
	RS1 = mqtt_client:subscribe(SC1, 
						[{"T1", 0, fun(A)->?debug_Fmt("  +++callbak+++ subscriber1/T1: suscriber1 gets message ~128p~n", [A]) end},
						 {"T2", 1, fun(A)->?debug_Fmt("  +++callbak+++ subscriber1/T2: suscriber1 gets message ~128p~n", [A]) end}]),
	?debug_Fmt("  +++test+++   after subscribe ~p~n", [RS1]),
	RS2 = mqtt_client:subscribe(SC2, 
						[{"T1", 0, fun(A)->?debug_Fmt("  +++callbak+++ subscriber2/T1: suscriber2 gets message ~128p~n", [A]) end},
						 {"T2", 1, fun(A)->?debug_Fmt("  +++callbak+++ subscriber2/T2: suscriber2 gets message ~128p~n", [A]) end}]),
	?debug_Fmt("  +++test+++   after subscribe ~p~n", [RS2]),
	RP = mqtt_client:pingreq(P, fun(A)-> ?debug_Fmt("  +++callbak+++ ping response arrives ~p~n", [A]) end),
	?debug_Fmt("  +++test+++   after ping ~p~n", [RP]),
	RPU0 = mqtt_client:publish(P, #publish{topic = "T1", qos = 0, payload = <<"Message(0) T1(0)">>}),
	?debug_Fmt("  +++test+++   after publish 0 ~p~n", [RPU0]),
	RPU1 = mqtt_client:publish(P, #publish{topic = "T1", qos = 1, payload = <<"Message(1) T1(0)">>}),
	?debug_Fmt("  +++test+++   after publish 1 ~p~n", [RPU1]),
	RPU2 = mqtt_client:publish(P, #publish{topic = "T1", qos = 2, payload = <<"Message(2) T1(0)">>}),
	?debug_Fmt("  +++test+++   after publish 2 ~p~n", [RPU2]),
	RUS1 = mqtt_client:unsubscribe(SC1, ["T1","T2"]),
	?debug_Fmt("  +++test+++   after unsibcribe ~p~n", [RUS1]),
	RUS2 = mqtt_client:unsubscribe(SC2, ["T1","T2"]),
	?debug_Fmt("  +++test+++   after unsibcribe ~p~n", [RUS2]),
	mqtt_client:disconnect(P),
	mqtt_client:disconnect(SC1),
	mqtt_client:disconnect(SC2),
	timer:sleep(1000),
	C2 = application:stop(mqtt_client),
	S1 = application:stop(mqtt_server),
	?debug_Fmt("~nServer app=~p~n Client app=~p~n", [S1,C2]),
	?passed.

