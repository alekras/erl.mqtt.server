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
%% @since 2017-01-05
%% @copyright 2015-2017 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc This module implements a tesing of MQTT retain meaasages.

-module(publish).

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
	publish_2/2
]).

-import(testing, [wait_all/1]).
%%
%% API Functions
%%

publish_0({QoS, publish} = _X, [Publisher, Subscriber] = _Conns) -> {"publish with QoS = " ++ integer_to_list(QoS) ++ ".", timeout, 100, fun() ->
	register(test_result, self()),

	F = fun({{Topic, Q}, _QoS, _Dup, _, Msg} = _Arg) -> 
					 <<QoS_m:1/bytes, _/binary>> = Msg,
%					 ?debug_Fmt("::test:: fun callback: ~100p Q=~p",[_Arg, binary_to_list(QoS_m)]),
					 ?assertEqual(QoS, Q),
					 Msq_QoS = list_to_integer(binary_to_list(QoS_m)),
					 Expect_QoS = if QoS > Msq_QoS -> Msq_QoS; true -> QoS end,
					 ?assertEqual(Expect_QoS, _QoS),
					 ?assertEqual("AKTest", Topic),
					 test_result ! done 
			end,

	R2_0 = mqtt_client:subscribe(Subscriber, [{"AKTest", QoS, F}]), 
	?assertEqual({suback,[QoS]}, R2_0),
	R3_0 = mqtt_client:publish(Publisher, #publish{topic = "AKTest", qos = 0}, <<"0) Test Payload QoS = 0. annon. function callback. ">>), 
	?assertEqual(ok, R3_0),
	R4_0 = mqtt_client:publish(Publisher, #publish{topic = "AKTest", qos = 1}, <<"1) Test Payload QoS = 0. annon. function callback. ">>), 
	?assertEqual(ok, R4_0),
	R5_0 = mqtt_client:publish(Publisher, #publish{topic = "AKTest", qos = 2}, <<"2) Test Payload QoS = 0. annon. function callback. ">>), 
	?assertEqual(ok, R5_0),

	R2 = mqtt_client:subscribe(Subscriber, [{"AKTest", QoS, {mqtt_server_tests, callback}}]), 
	?assertEqual({suback,[QoS]}, R2),
	R3 = mqtt_client:publish(Publisher, #publish{topic = "AKTest"}, <<"Test Payload QoS = 0.">>), 
	?assertEqual(ok, R3),
%% errors:
	R4 = mqtt_client:publish(Publisher, #publish{topic = binary_to_list(<<"AK",0,0,0,"Test">>), qos = 2}, <<"Test Payload QoS = 0.">>), 
	?assertEqual(ok, R4),

	W = wait_all(4),
	unregister(test_result),
	?assert(W),

	?PASSED
end}.

publish_1({_QoS, publish} = _X, [Publisher, Subscriber] = _Conns) -> {"publish with QoS = 1", timeout, 100, fun() ->
	register(test_result, self()),

	R2_0 = mqtt_client:subscribe(Subscriber, [{"AKtest", 1, fun(Arg) -> ?assertMatch({{"AKtest",1},_,_,_,<<"Test Payload QoS = 1. annon. function callback. ">>}, Arg), test_result ! done end}]), 
	?assertEqual({suback,[1]}, R2_0),
	R3_0 = mqtt_client:publish(Publisher, #publish{topic = "AKtest", qos = 0}, <<"Test Payload QoS = 1. annon. function callback. ">>), 
	?assertEqual(ok, R3_0),
	R4_0 = mqtt_client:publish(Publisher, #publish{topic = "AKtest", qos = 1}, <<"Test Payload QoS = 1. annon. function callback. ">>), 
	?assertEqual(ok, R4_0),
	R5_0 = mqtt_client:publish(Publisher, #publish{topic = "AKtest", qos = 2}, <<"Test Payload QoS = 1. annon. function callback. ">>), 
	?assertEqual(ok, R5_0),

	R2 = mqtt_client:subscribe(Subscriber, [{"AKTest", 1, {mqtt_server_tests, callback}}]), 
	?assertEqual({suback,[1]}, R2),
	R3 = mqtt_client:publish(Publisher, #publish{topic = "AKTest"}, <<"Test Payload QoS = 0.">>), 
	?assertEqual(ok, R3),

	wait_all(4),
	
	unregister(test_result),

	?PASSED
end}.

publish_2({_QoS, publish} = _X, [Publisher, Subscriber] = _Conns) -> {"publish with QoS = 2", timeout, 100, fun() ->
	register(test_result, self()),
  
	F = fun({{Topic, Q}, QoS, _Dup, _, Msg} = _Arg) -> 
					 <<QoS_m:1/bytes, _/binary>> = Msg,
%					 ?debug_Fmt("::test:: fun callback: ~100p Q=~p",[_Arg, binary_to_list(QoS_m)]),
					 ?assertEqual(2, Q),
					 ?assertEqual(list_to_integer(binary_to_list(QoS_m)), QoS),
					 ?assertEqual("AKtest", Topic),
					 test_result ! done 
			end,
	R2_0 = mqtt_client:subscribe(Subscriber, [{"AKtest", 2, F}]), 
	?assertEqual({suback,[2]}, R2_0),
	R3_0 = mqtt_client:publish(Publisher, #publish{topic = "AKtest", qos = 0}, <<"0) Test Payload QoS = 2. annon. function callback. ">>), 
	?assertEqual(ok, R3_0),
	R4_0 = mqtt_client:publish(Publisher, #publish{topic = "AKtest", qos = 1}, <<"1) Test Payload QoS = 2. annon. function callback. ">>), 
	?assertEqual(ok, R4_0),
	R5_0 = mqtt_client:publish(Publisher, #publish{topic = "AKtest", qos = 2}, <<"2) Test Payload QoS = 2. annon. function callback. ">>), 
	?assertEqual(ok, R5_0),

	R2 = mqtt_client:subscribe(Subscriber, [{"AKTest", 2, {mqtt_server_tests, callback}}]), 
	?assertEqual({suback,[2]}, R2),
	R3 = mqtt_client:publish(Publisher, #publish{topic = "AKTest"}, <<"Test Payload QoS = 0.">>), 
	?assertEqual(ok, R3),

	wait_all(4),
	
	unregister(test_result),

	?PASSED
end}.
