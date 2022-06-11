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

%% @hidden
%% @since 2017-01-05
%% @copyright 2015-2022 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc This module implements a testing of MQTT retain meaasages.

-module(share).

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
	callback/1
]).

-import(testing_v5, [wait_all/1]).
%%
%% API Functions
%%

publish_0({QoS, share} = _X, [Publisher, Subscriber1, Subscriber2, _, _] = _Conns) -> {"publish to shared Topic with QoS = " ++ integer_to_list(QoS) ++ ".", timeout, 100, fun() ->
	register(test_result, self()),

	F = fun(_SubN) ->
			fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
					 <<QoS_m:1/bytes, _/binary>> = Msg,
					 ?debug_Fmt("::test:: Subscriber ~p callback: ~100p Q=~p",[_SubN, _Arg, binary_to_list(QoS_m)]),
					 ?assertEqual(QoS, Q#subscription_options.max_qos),
					 Msq_QoS = list_to_integer(binary_to_list(QoS_m)),
					 Expect_QoS = if QoS > Msq_QoS -> Msq_QoS; true -> QoS end,
					 ?assertEqual(Expect_QoS, _QoS),
					 ?assertEqual("AKTest", Topic),
					 test_result ! done 
			end
			end,

	R2_0 = mqtt_client:subscribe(Subscriber1, [{"$share/A/AKTest", #subscription_options{max_qos = QoS}, F(1)}]), 
	?assertEqual({suback,[QoS],[]}, R2_0),
	R2_1 = mqtt_client:subscribe(Subscriber2, [{"$share/A/AKTest", #subscription_options{max_qos = QoS}, F(2)}]), 
	?assertEqual({suback,[QoS],[]}, R2_1),
	R3_0 = mqtt_client:publish(Publisher, #publish{topic = "AKTest", qos = 0}, <<"0) Test Payload QoS = 0. annon. function callback. ">>), 
	?assertEqual(ok, R3_0),
	R4_0 = mqtt_client:publish(Publisher, #publish{topic = "AKTest", qos = 1}, <<"1) Test Payload QoS = 1. annon. function callback. ">>), 
	?assertEqual(ok, R4_0),
	R5_0 = mqtt_client:publish(Publisher, #publish{topic = "AKTest", qos = 2}, <<"2) Test Payload QoS = 2. annon. function callback. ">>), 
	?assertEqual(ok, R5_0),

	W = wait_all(3),
	unregister(test_result),
	?assert(W),

	?PASSED
end}.

publish_1({QoS, share} = _X, [Publisher, Subscriber1, Subscriber2, Subscriber3, Subscriber4] = _Conns) -> {"publish with QoS = 1", timeout, 100, fun() ->
	register(test_result, self()),

	F = fun(SubN) ->
			fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
					<<QoS_m:1/bytes, _/binary>> = Msg,
					?debug_Fmt("::test:: Subscriber ~p callback: ~100p Q=~p",[SubN, _Arg, binary_to_list(QoS_m)]),
					?assertEqual(QoS, Q#subscription_options.max_qos),
					Msq_QoS = list_to_integer(binary_to_list(QoS_m)),
					Expect_QoS = if QoS > Msq_QoS -> Msq_QoS; true -> QoS end,
					?assertEqual(Expect_QoS, _QoS),
					?assertEqual("AKTest", Topic),
					test_result ! {SubN, done} 
			end
			end,

	R2_0 = mqtt_client:subscribe(Subscriber1, [{"$share/A/AKTest", #subscription_options{max_qos = QoS}, F(1)}]), 
	?assertEqual({suback,[QoS],[]}, R2_0),
	R2_1 = mqtt_client:subscribe(Subscriber2, [{"$share/A/AKTest", #subscription_options{max_qos = QoS}, F(2)}]), 
	?assertEqual({suback,[QoS],[]}, R2_1),
	R2_2 = mqtt_client:subscribe(Subscriber3, [{"$share/B/AKTest", #subscription_options{max_qos = QoS}, F(3)}]), 
	?assertEqual({suback,[QoS],[]}, R2_2),
	R2_3 = mqtt_client:subscribe(Subscriber4, [{"$share/B/AKTest", #subscription_options{max_qos = QoS}, F(4)}]), 
	?assertEqual({suback,[QoS],[]}, R2_3),
	
	FT = fun(Q) ->
		fun(_) ->
			R = mqtt_client:publish(Publisher,
															#publish{topic = "AKTest", qos = Q},
															<<(integer_to_binary(Q))/binary, ") Test Payload QoS = 0. annon. function callback. ">>), 
			?assertEqual(ok, R)
		end
	end,
	lists:foreach(FT(0), lists:seq(1, 50)),
	lists:foreach(FT(1), lists:seq(1, 50)),
	lists:foreach(FT(2), lists:seq(1, 50)),

	W = wait_all(300),
	unregister(test_result),
	?assert(W),
	?debug_Fmt("::test:: Subscriber1:~p  Subscriber2:~p",[get(1),get(2)]),
	?debug_Fmt("::test:: Subscriber3:~p  Subscriber4:~p",[get(3),get(4)]),

	?PASSED
end}.

publish_2({QoS, publish} = _X, [Publisher, Subscriber] = _Conns) -> {"publish with QoS = 2", timeout, 100, fun() ->
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
	R3_0 = mqtt_client:publish(Publisher, #publish{topic = "AKtest", qos = 0}, <<"0) Test Payload QoS = 2. annon. function callback. ">>), 
	?assertEqual(ok, R3_0),
	R4_0 = mqtt_client:publish(Publisher, #publish{topic = "AKtest", qos = 1}, <<"1) Test Payload QoS = 2. annon. function callback. ">>), 
	?assertEqual(ok, R4_0),
	R5_0 = mqtt_client:publish(Publisher, #publish{topic = "AKtest", qos = 2}, <<"2) Test Payload QoS = 2. annon. function callback. ">>), 
	?assertEqual(ok, R5_0),

	R2 = mqtt_client:subscribe(Subscriber, [{"AKTest", QoS, {?MODULE, callback}}]), 
	?assertEqual({suback,[QoS],[]}, R2),
	R3 = mqtt_client:publish(Publisher, #publish{topic = "AKTest", qos=2}, <<"Test Payload QoS = 2.">>), 
	?assertEqual(ok, R3),

	W = wait_all(4),
	
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
%% callback({_, #publish{qos= QoS}} = Arg) ->
%% 	case QoS of
%% 		0 -> ?assertMatch({2, #publish{topic= "AKtest", qos= 0, payload= <<"Test Payload QoS = 0.">>}}, Arg);
%% 		1 -> ?assertMatch({2, #publish{topic= "AKtest", qos= 1, payload= <<"Test Payload QoS = 1.">>}}, Arg);
%% 		2 -> ?assertMatch({2, #publish{topic= "AKtest", qos= 2, payload= <<"Test Payload QoS = 2.">>}}, Arg)
%% 	end,
%% 	?debug_Fmt("::test:: ~p:callback<_>: ~p",[?MODULE, Arg]),
%% 	test_result ! done.
