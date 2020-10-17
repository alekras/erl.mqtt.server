%%
%% Copyright (C) 2015-2020 by krasnop@bellsouth.net (Alexei Krasnopolski)
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
%% @since 2017-01-05
%% @copyright 2015-2020 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc This module implements a testing of MQTT retain meaasages.

-module(publish_v5).

%%
%% Include files
%%
%% -include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("mqtt_common/include/mqtt.hrl").
-include("test.hrl").

-export([
  publish_0/2,
  publish_1/2,
	publish_2/2,
	callback/1,
	disconnect_callback/1
]).

-import(testing, [wait_all/1]).
%%
%% API Functions
%%

publish_0({QoS, publish} = _X, [Publisher, Subscriber] = _Conns) -> {"\n*****\npublish with QoS = " ++ integer_to_list(QoS) ++ ".\n*****\n", timeout, 100, fun() ->
	register(test_result, self()),

	F = fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
					 <<QoS_m:1/bytes, _/binary>> = Msg,
					 ?debug_Fmt("::test:: fun callback: ~100p Q=~p",[_Arg, binary_to_list(QoS_m)]),
					 ?assertEqual(QoS, Q),
					 Msq_QoS = list_to_integer(binary_to_list(QoS_m)),
					 Expect_QoS = if QoS > Msq_QoS -> Msq_QoS; true -> QoS end,
					 ?assertEqual(Expect_QoS, _QoS),
					 ?assertEqual("AKTest", Topic),
					 test_result ! done 
			end,

	R2_0 = mqtt_client:subscribe(Subscriber, [{"AKTest", QoS, F}]), 
	?assertEqual({suback,[QoS],[]}, R2_0),
	R3_0 = mqtt_client:publish(Publisher, #publish{topic = "AKTest", qos = 0}, <<"0) Test Payload QoS = 0. annon. function callback. ">>), 
	?assertEqual(ok, R3_0),
	R4_0 = mqtt_client:publish(Publisher, #publish{topic = "AKTest", qos = 1}, <<"1) Test Payload QoS = 1. annon. function callback. ">>), 
	?assertEqual(ok, R4_0),
	R5_0 = mqtt_client:publish(Publisher, #publish{topic = "AKTest", qos = 2}, <<"2) Test Payload QoS = 2. annon. function callback. ">>), 
	?assertEqual(ok, R5_0),

	R2 = mqtt_client:subscribe(Subscriber, [{"AKTest", QoS, {?MODULE, callback}}]), 
	?assertEqual({suback,[QoS],[]}, R2),
	R3 = mqtt_client:publish(Publisher, #publish{topic = "AKTest"}, <<"Test Payload QoS = 0.">>), 
	?assertEqual(ok, R3),
%% errors:
	R4 = mqtt_client:publish(Publisher, #publish{topic = binary_to_list(<<"AK",0,0,0,"Test">>), qos = 2}, <<"Test Payload QoS = 0.">>), 
	?assertEqual(ok, R4), %% Erlang server @todo - have to fail!!!
%%	?assertMatch(#mqtt_client_error{}, R4), %% Mosquitto server

	W = wait_all(4),
	unregister(test_result),
	?assert(W),

	?PASSED
end}.

publish_1({QoS, publish} = _X, [Publisher, Subscriber] = _Conns) -> {"\n*****\npublish with QoS = " ++ integer_to_list(QoS) ++ "\n*****\n", timeout, 100, fun() ->
	register(test_result, self()),
	
	F = fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
					 <<QoS_m:1/bytes, _/binary>> = Msg,
%					 ?debug_Fmt("::test:: fun callback: ~100p Q=~p",[_Arg, binary_to_list(QoS_m)]),
					 ?assertEqual(QoS, Q),
					 Msq_QoS = list_to_integer(binary_to_list(QoS_m)),
					 Expect_QoS = if QoS > Msq_QoS -> Msq_QoS; true -> QoS end,
					 ?assertEqual(Expect_QoS, _QoS),
					 ?assertEqual("AKtest", Topic),
					 test_result ! done 
			end,
	R2_0 = mqtt_client:subscribe(Subscriber, [{"AKtest", QoS, F}]), 
	?assertEqual({suback,[QoS],[]}, R2_0),

	R3_0 = mqtt_client:publish(Publisher, #publish{topic = "AKtest", qos = 0}, <<"0) Test Payload QoS = 0. annon. function callback.">>), 
	?assertEqual(ok, R3_0),
	R4_0 = mqtt_client:publish(Publisher, #publish{topic = "AKtest", qos = 1}, <<"1) Test Payload QoS = 0. annon. function callback.">>), 
	?assertEqual(ok, R4_0),
	R5_0 = mqtt_client:publish(Publisher, #publish{topic = "AKtest", qos = 2}, <<"2) Test Payload QoS = 0. annon. function callback.">>), 
	?assertEqual(ok, R5_0),

	R2 = mqtt_client:subscribe(Subscriber, [{"AKTest", QoS, {?MODULE, callback}}]), 
	?assertEqual({suback,[QoS],[]}, R2),
	R3_0 = mqtt_client:publish(Publisher, #publish{topic = "AKTest", qos=1}, <<"Test Payload QoS = 1.">>), 
	?assertEqual(ok, R3_0),
	R3_1 = mqtt_client:publish(Publisher, #publish{topic = "AKTest", qos=2}, <<"Test Payload QoS = 2.">>), 
	?assertEqual(ok, R3_1),
	R3_2 = mqtt_client:publish(Publisher, #publish{topic = "AKTest", qos=2}, <<"Test Payload QoS = 2.">>), 
	?assertEqual(ok, R3_2),

	W = wait_all(6),
	
	unregister(test_result),
	?assert(W),

	?PASSED
end}.

%% Test Receive Maximum. Moscitto does not support this feature
publish_2({QoS, publish_rec_max} = _X, [Publisher, Subscriber] = _Conns) -> {"\n*****\npublish with Receive Max.\n*****\n", timeout, 100, fun() ->
	register(test_result, self()),

	F = fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
					 <<QoS_m:1/bytes, _/binary>> = Msg,
%					 ?debug_Fmt("::test:: fun callback: ~100p Q=~p",[_Arg, binary_to_list(QoS_m)]),
					 ?assertEqual(QoS, Q),
					 Msq_QoS = list_to_integer(binary_to_list(QoS_m)),
					 Expect_QoS = if QoS > Msq_QoS -> Msq_QoS; true -> QoS end,
					 ?assertEqual(Expect_QoS, _QoS),
					 ?assertEqual("AKtest", Topic),
					 test_result ! done 
			end,
	R2_0 = mqtt_client:subscribe(Subscriber, [{"AKtest", #subscription_options{max_qos=QoS}, F}]), 
	?assertEqual({suback,[QoS],[]}, R2_0),

	gen_server:call(Publisher, {set_test_flag, skip_send_pubrel}),

	gen_server:call(Publisher, {publish, #publish{topic = "AKtest", payload = <<"2) Test Payload QoS = 2. annon. function callback. ">>, qos = 2}}, ?MQTT_GEN_SERVER_TIMEOUT),
	gen_server:call(Publisher, {publish, #publish{topic = "AKtest", payload = <<"2) Test Payload QoS = 2. annon. function callback. ">>, qos = 2}}, ?MQTT_GEN_SERVER_TIMEOUT),
	gen_server:call(Publisher, {publish, #publish{topic = "AKtest", payload = <<"2) Test Payload QoS = 2. annon. function callback. ">>, qos = 2}}, ?MQTT_GEN_SERVER_TIMEOUT),
	gen_server:call(Publisher, {publish, #publish{topic = "AKtest", payload = <<"2) Test Payload QoS = 2. annon. function callback. ">>, qos = 2}}, ?MQTT_GEN_SERVER_TIMEOUT),
%% 	gen_server:call(Publisher, {publish, #publish{topic = "AKtest", payload = <<"2) Test Payload QoS = 2. annon. function callback. ">>, qos = 2}}, ?MQTT_GEN_SERVER_TIMEOUT),
%% 	gen_server:call(Publisher, {publish, #publish{topic = "AKtest", payload = <<"2) Test Payload QoS = 2. annon. function callback. ">>, qos = 2}}, ?MQTT_GEN_SERVER_TIMEOUT),
%% 	gen_server:call(Publisher, {publish, #publish{topic = "AKtest", payload = <<"2) Test Payload QoS = 2. annon. function callback. ">>, qos = 2}}, ?MQTT_GEN_SERVER_TIMEOUT),
%% 	gen_server:call(Publisher, {publish, #publish{topic = "AKtest", payload = <<"2) Test Payload QoS = 2. annon. function callback. ">>, qos = 2}}, ?MQTT_GEN_SERVER_TIMEOUT),
%% 	gen_server:call(Publisher, {publish, #publish{topic = "AKtest", payload = <<"2) Test Payload QoS = 2. annon. function callback. ">>, qos = 2}}, ?MQTT_GEN_SERVER_TIMEOUT),
	R3_0 = mqtt_client:status(Publisher),
	?debug_Fmt("::test:: ~100p~n",[R3_0]),
	?assertMatch([{session_present, 0}, _], R3_0),

	gen_server:call(Publisher, {publish, #publish{topic = "AKtest", payload = <<"2) Test Payload QoS = 2. annon. function callback. ">>, qos = 2}}, ?MQTT_GEN_SERVER_TIMEOUT),
%% 	gen_server:call(Publisher, {publish, #publish{topic = "AKtest", payload = <<"2) Test Payload QoS = 2. annon. function callback. ">>, qos = 2}}, ?MQTT_GEN_SERVER_TIMEOUT),
%% 	gen_server:call(Publisher, {publish, #publish{topic = "AKtest", payload = <<"2) Test Payload QoS = 2. annon. function callback. ">>, qos = 2}}, ?MQTT_GEN_SERVER_TIMEOUT),
%% 	gen_server:call(Publisher, {publish, #publish{topic = "AKtest", payload = <<"2) Test Payload QoS = 2. annon. function callback. ">>, qos = 2}}, ?MQTT_GEN_SERVER_TIMEOUT),
%% 	gen_server:call(Publisher, {publish, #publish{topic = "AKtest", payload = <<"2) Test Payload QoS = 2. annon. function callback. ">>, qos = 2}}, ?MQTT_GEN_SERVER_TIMEOUT),
	timer:sleep(1000),
	R4_0 = mqtt_client:status(Publisher),
	?debug_Fmt("::test:: ~100p~n",[R4_0]),
	?assertMatch(disconnected, R4_0),

	timer:sleep(1000),
	W = wait_all(1),
	
	unregister(test_result),
	?assert(W),

	?PASSED
end}.

callback({TopicQoS, #publish{topic= "AKTest", qos= QoS, payload= <<"Test Payload QoS = 0.">>}} = Arg) ->
	case TopicQoS of
		0 -> ?assertEqual(0, QoS);
		1 -> ?assertEqual(0, QoS);
		2 -> ?assertEqual(0, QoS)
	end,
	?debug_Fmt("::test:: ~p:callback<0>: ~p",[?MODULE, Arg]),
	test_result ! done;
callback({TopicQoS, #publish{topic= "AKTest", qos= QoS, payload= <<"Test Payload QoS = 1.">>}} = Arg) ->
	case TopicQoS of
		0 -> ?assertEqual(0, QoS);
		1 -> ?assertEqual(1, QoS);
		2 -> ?assertEqual(1, QoS)
	end,
	?debug_Fmt("::test:: ~p:callback<1>: ~p",[?MODULE, Arg]),
	test_result ! done;
callback({TopicQoS, #publish{topic= "AKTest", qos= QoS, payload= <<"Test Payload QoS = 2.">>}} = Arg) ->
	case TopicQoS of
		0 -> ?assertEqual(0, QoS);
		1 -> ?assertEqual(1, QoS);
		2 -> ?assertEqual(2, QoS)
	end,
	?debug_Fmt("::test:: ~p:callback<2>: ~p",[?MODULE, Arg]),
	test_result ! done.

disconnect_callback({Q, _} = Arg) -> 
	?debug_Fmt("::test:: fun Callback for disconnect (Receive max): ~100p",[Arg]),
	?assertEqual(147,Q),
	test_result ! done.

