%%
%% Copyright (C) 2015-2023 by krasnop@bellsouth.net (Alexei Krasnopolski)
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
%% @copyright 2015-2023 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc This module implements a testing of MQTT retain meaasages.

-module(topic_alias).

%%
%% Include files
%%
%% -include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("mqtt_common/include/mqtt.hrl").
-include_lib("mqtt_common/include/mqtt_property.hrl").
-include("test.hrl").

-export([
  publish_0/2,
  publish_1/2,
  publish_2/2,
	publish_3/2
]).

-import(testing_v5, [wait_all/1]).
%%
%% API Functions
%%

publish_0({QoS, publish} = _X, [Publisher, Subscriber] = _Conns) -> {"publish to Topic Alias with QoS = " ++ integer_to_list(QoS) ++ ".", timeout, 100, fun() ->
	register(test_result, self()),

	F = fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
					 <<QoS_m:1/bytes, _/binary>> = Msg,
					 ?debug_Fmt("::test:: F fun callback: ~100p Q=~p",[_Arg, binary_to_list(QoS_m)]),
					 ?assertEqual(QoS, Q#subscription_options.max_qos),
					 Msq_QoS = list_to_integer(binary_to_list(QoS_m)),
					 Expect_QoS = if QoS > Msq_QoS -> Msq_QoS; true -> QoS end,
					 ?assertEqual(Expect_QoS, _QoS),
					 ?assertEqual("/AKTest", Topic),
					 test_result ! done 
			end,

	R2_0 = mqtt_client:subscribe(Subscriber, [{"/AKTest", #subscription_options{max_qos = QoS}, F}]), 
	?assertEqual({suback,[QoS],[]}, R2_0),
	R3_0 = mqtt_client:publish(Publisher, #publish{topic = "/AKTest", qos = 0, properties = [{?Topic_Alias, 2}]}, <<"0) Test Payload QoS = 0. annon. function callback. ">>), 
	?assertEqual(ok, R3_0),
	R4_0 = mqtt_client:publish(Publisher, #publish{topic = "", qos = 1, properties = [{?Topic_Alias, 2}]}, <<"1) Test Payload QoS = 1. annon. function callback. ">>), 
	?assertEqual(ok, R4_0),
	R5_0 = mqtt_client:publish(Publisher, #publish{topic = "/AKTest", qos = 2, properties = []}, <<"2) Test Payload QoS = 2. annon. function callback. ">>), 
	?assertEqual(ok, R5_0),

	W = wait_all(3),
	unregister(test_result),
	?assert(W),

	?PASSED
end}.

publish_1({QoS, publish} = _X, [Publisher, Subscriber] = _Conns) -> {"publish to Topic Alias with QoS = " ++ integer_to_list(QoS) ++ ".\n", timeout, 100, fun() ->
	register(test_result, self()),

	F = fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
					 <<QoS_m:1/bytes, _/binary>> = Msg,
					 ?debug_Fmt("::test:: F fun callback: ~100p Q=~p",[_Arg, binary_to_list(QoS_m)]),
					 ?assertEqual(QoS, Q#subscription_options.max_qos),
					 Msq_QoS = list_to_integer(binary_to_list(QoS_m)),
					 Expect_QoS = if QoS > Msq_QoS -> Msq_QoS; true -> QoS end,
					 ?assertEqual(Expect_QoS, _QoS),
					 ?assertEqual("/AKTest", Topic),
					 test_result ! done 
			end,

	R2_0 = mqtt_client:subscribe(Subscriber, [{"/AKTest", #subscription_options{max_qos = QoS}, F}]), 
	?assertEqual({suback,[QoS],[]}, R2_0),
	R2_1 = mqtt_client:publish(Subscriber, #publish{topic = "/AKTest", qos = 0, properties = [{?Topic_Alias, 3}]}, <<"0) Subscriber self publish Payload QoS = 0. annon. function callback. ">>), 
	?assertEqual(ok, R2_1),
	R2_2 = mqtt_client:publish(Subscriber, #publish{topic = "", qos = 0, properties = [{?Topic_Alias, 3}]}, <<"0) Subscriber self publish Payload QoS = 0. annon. function callback. ">>), 
	?assertEqual(ok, R2_2),
	R3_0 = mqtt_client:publish(Publisher, #publish{topic = "/AKTest", qos = 0, properties = [{?Topic_Alias, 2}]}, <<"0) Test Payload QoS = 0. annon. function callback. ">>), 
	?assertEqual(ok, R3_0),
	R4_0 = mqtt_client:publish(Publisher, #publish{topic = "", qos = 1, properties = [{?Topic_Alias, 2}]}, <<"1) Test Payload QoS = 1. annon. function callback. ">>), 
	?assertEqual(ok, R4_0),
	R5_0 = mqtt_client:publish(Publisher, #publish{topic = "/AKTest", qos = 2, properties = []}, <<"2) Test Payload QoS = 2. annon. function callback. ">>), 
	?assertEqual(ok, R5_0),

	W = wait_all(5),
	unregister(test_result),
	?assert(W),

	?PASSED
end}.

publish_2({QoS, publish} = _X, [Publisher, Subscriber] = _Conns) -> {"publish to Topic Alias with QoS = " ++ integer_to_list(QoS) ++ ".\n", timeout, 100, fun() ->
	register(test_result, self()),

	F = fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
					 <<QoS_m:1/bytes, _/binary>> = Msg,
					 ?debug_Fmt("::test:: F fun callback: ~100p Q=~p",[_Arg, binary_to_list(QoS_m)]),
					 ?assertEqual(QoS, Q#subscription_options.max_qos),
					 Msq_QoS = list_to_integer(binary_to_list(QoS_m)),
					 Expect_QoS = if QoS > Msq_QoS -> Msq_QoS; true -> QoS end,
					 ?assertEqual(Expect_QoS, _QoS),
					 ?assertEqual("/AKTest", Topic),
					 test_result ! done 
			end,

	R2_0 = mqtt_client:subscribe(Subscriber, [{"/AKTest", #subscription_options{max_qos = QoS}, F}]), 
	?assertEqual({suback,[QoS],[]}, R2_0),
	R2_1 = mqtt_client:publish(Subscriber, #publish{topic = "/AKTest", qos = 2, properties = [{?Topic_Alias, 1}]}, <<"2) Subscriber self publish Payload QoS = 0. annon. function callback. ">>), 
	?assertEqual(ok, R2_1),
	R2_2 = mqtt_client:publish(Subscriber, #publish{topic = "", qos = 2, properties = [{?Topic_Alias, 1}]}, <<"2) Subscriber self publish Payload QoS = 0. annon. function callback. ">>), 
	?assertEqual(ok, R2_2),

	gen_server:call(Subscriber, {set_test_flag, skip_alias_max_check}),
	mqtt_client:publish(Subscriber, #publish{topic = "/AKTest/2", qos = 2, properties = [{?Topic_Alias, 2}]}, <<"2) Subscriber self publish Payload QoS = 2. annon. function callback. ">>), 
	mqtt_client:publish(Subscriber, #publish{topic = "/AKTest/3", qos = 2, properties = [{?Topic_Alias, 3}]}, <<"2) Subscriber self publish Payload QoS = 2. annon. function callback. ">>), 
	mqtt_client:publish(Subscriber, #publish{topic = "/AKTest/4", qos = 2, properties = [{?Topic_Alias, 4}]}, <<"2) Subscriber self publish Payload QoS = 2. annon. function callback. ">>), 
	mqtt_client:publish(Subscriber, #publish{topic = "/AKTest/5", qos = 2, properties = [{?Topic_Alias, 5}]}, <<"2) Subscriber self publish Payload QoS = 2. annon. function callback. ">>), 
	mqtt_client:publish(Subscriber, #publish{topic = "/AKTest/6", qos = 2, properties = [{?Topic_Alias, 6}]}, <<"2) Subscriber self publish Payload QoS = 2. annon. function callback. ">>), 
	mqtt_client:publish(Subscriber, #publish{topic = "/AKTest/7", qos = 2, properties = [{?Topic_Alias, 7}]}, <<"2) Subscriber self publish Payload QoS = 2. annon. function callback. ">>), 
	R2_3 = mqtt_client:publish(Subscriber, #publish{topic = "/AKTest/8", qos = 2, properties = [{?Topic_Alias, 8}]}, <<"2) Subscriber self publish Payload QoS = 2. annon. function callback. ">>), 
	?debug_Fmt("::test:: R2_3= ~100p",[R2_3]),
	%%	?assertMatch(#mqtt_client_error{}, R2_3),

	?assertEqual(disconnected, mqtt_client:status(Subscriber)),

	W = wait_all(2),
	unregister(test_result),
	?assert(W),

	?PASSED
end}.

publish_3({QoS, publish} = _X, [Publisher, Subscriber] = _Conns) -> {"publish to empty topic without Topic Alias with QoS = " ++ integer_to_list(QoS) ++ ".\n", timeout, 100, fun() ->
	register(test_result, self()),

	F = fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
					 <<QoS_m:1/bytes, _/binary>> = Msg,
					 ?debug_Fmt("::test:: F fun callback: ~100p Q=~p",[_Arg, binary_to_list(QoS_m)]),
					 ?assertEqual(QoS, Q#subscription_options.max_qos),
					 Msq_QoS = list_to_integer(binary_to_list(QoS_m)),
					 Expect_QoS = if QoS > Msq_QoS -> Msq_QoS; true -> QoS end,
					 ?assertEqual(Expect_QoS, _QoS),
					 ?assertEqual("/AKTest", Topic),
					 test_result ! done 
			end,

	R2_0 = mqtt_client:subscribe(Subscriber, [{"/AKTest", #subscription_options{max_qos = QoS}, F}]), 
	?assertEqual({suback,[QoS],[]}, R2_0),
	R2_1 = mqtt_client:publish(Publisher, #publish{topic = "/AKTest", qos = 0, properties = []}, <<"0) Subscriber self publish Payload QoS = 0. annon. function callback. ">>), 
	?assertEqual(ok, R2_1),
	R2_2 = mqtt_client:publish(Publisher, #publish{topic = "", qos = 0, properties = []}, <<"0) Subscriber self publish Payload QoS = 0. annon. function callback. ">>), 
	?assertMatch(#mqtt_client_error{type=protocol,errno=130}, R2_2),
	
	gen_server:call(Publisher, {set_test_flag, skip_alias_max_check}),
	R2_3 = mqtt_client:publish(Publisher, #publish{topic = "", qos = 0, properties = []}, <<"0) Subscriber self publish Payload QoS = 0. annon. function callback. ">>), 
	?assertMatch(ok, R2_3),
	timer:sleep(1000),
	?assertEqual(disconnected, mqtt_client:status(Publisher)),

	W = wait_all(1),
	unregister(test_result),
	?assert(W),

	?PASSED
end}.
