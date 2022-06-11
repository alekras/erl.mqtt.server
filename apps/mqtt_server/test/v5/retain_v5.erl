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
%% @since 2016-12-19
%% @copyright 2015-2022 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc This module implements a tesing of MQTT retain meaasages.

-module(retain_v5).

%%
%% Include files
%%
%% -include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("mqtt_common/include/mqtt.hrl").
-include_lib("mqtt_common/include/mqtt_property.hrl").
-include("test.hrl").

-export([
	retain_0/2,
	retain_1/2,
	retain_2/2,
	retain_3/2,
	subscription_option/2,
	subscription_id/2
]).
-import(testing_v5, [wait_all/1]).
%%
%% API Functions
%%

%% .
retain_0({QoS, retain} = _X, [Publisher, Subscriber1, Subscriber2] = _Conns) -> {"retain QoS=" ++ integer_to_list(QoS) ++ ".", timeout, 100, fun() ->
	register(test_result, self()),

	F = fun(_SubN) ->
			fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
%					 ?debug_Fmt("::test:: Subs:~p fun callback: ~100p",[_SubN, _Arg]),
					 ?assertEqual(QoS, Q#subscription_options.max_qos),
					 ?assertEqual("AK_retain_test", Topic),
					 ?assertEqual(<<"Test 0 retain message QoS=", (list_to_binary((integer_to_list(QoS))))/binary>>, Msg),
					 test_result ! done 
			end
			end,
	R1_0 = mqtt_client:subscribe(Subscriber1, [{"AK_retain_test", #subscription_options{max_qos = QoS}, F(1)}]), 
	?assertEqual({suback,[QoS],[]}, R1_0),

	R2_0 = mqtt_client:publish(Publisher, #publish{topic = "AK_retain_test", qos = QoS, retain = 1}, <<"Test 0 retain message QoS=", (list_to_binary((integer_to_list(QoS))))/binary>>),
	?assertEqual(ok, R2_0),

	timer:sleep(100),
	
	R1_1 = mqtt_client:subscribe(Subscriber2, [{"AK_retain_test", #subscription_options{max_qos = QoS}, F(2)}]), 
	?assertEqual({suback,[QoS],[]}, R1_1),

	W = wait_all(2),

	unregister(test_result),
	?assert(W),
	?PASSED
end}.

%% .
retain_1({QoS, retain} = _X, [Publisher, Subscriber1, Subscriber2] = _Conns) -> {"retain QoS=" ++ integer_to_list(QoS) ++ ".", timeout, 100, fun() ->
	register(test_result, self()),
  
	F = fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
%					 ?debug_Fmt("::test:: fun callback: ~100p",[_Arg]),
					 ?assertEqual(QoS, Q#subscription_options.max_qos),
					 ?assertEqual("AK_retain_test", Topic),
					 ?assertEqual(<<"Test 1 retain message QoS=", (list_to_binary((integer_to_list(QoS))))/binary>>, Msg),
					 test_result ! done 
			end,
	R1_0 = mqtt_client:publish(Publisher, #publish{topic = "AK_retain_test", qos = QoS, retain = 1}, <<"Test 1 retain message QoS=", (list_to_binary((integer_to_list(QoS))))/binary>>),
	?assertEqual(ok, R1_0),

	timer:sleep(100),

	R2_0 = mqtt_client:subscribe(Subscriber1, [{"AK_retain_test", #subscription_options{max_qos = QoS}, F}]), 
	?assertEqual({suback,[QoS],[]}, R2_0),

	R2_1 = mqtt_client:disconnect(Publisher),
	?assertEqual(ok, R2_1),

	R2_2 = mqtt_client:subscribe(Subscriber2, [{"AK_retain_test", #subscription_options{max_qos = QoS}, F}]), 
	?assertEqual({suback,[QoS],[]}, R2_2),

	W = wait_all(2),

	unregister(test_result),
	?assert(W),
	?PASSED
end}.

retain_2({QoS, retain} = _X, [Publisher, Subscriber1, Subscriber2] = _Conns) -> {"retain QoS=" ++ integer_to_list(QoS) ++ ".", timeout, 100, fun() ->
	register(test_result, self()),
  
	F = fun(_SubN) ->
			fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
					 ?debug_Fmt("::test:: Subscriber ~p callback: ~100p",[_SubN, _Arg]),
					 ?assertEqual(QoS, Q#subscription_options.max_qos),
					 ?assertEqual("AK_retain_test", Topic),
					 ?assertEqual(<<"Test 1 retain message QoS=", (list_to_binary((integer_to_list(QoS))))/binary>>, Msg),
					 test_result ! done 
			end
			end,
	R1_0 = mqtt_client:publish(Publisher, #publish{topic = "AK_retain_test", qos = QoS, retain = 1}, <<"Test 1 retain message QoS=", (list_to_binary((integer_to_list(QoS))))/binary>>),
	?assertEqual(ok, R1_0),

	timer:sleep(100),

	R2_0 = mqtt_client:subscribe(Subscriber1, [{"AK_retain_test", #subscription_options{max_qos = QoS,retain_handling=1}, F(1)}]), %% send for new subs 
	?assertEqual({suback,[QoS],[]}, R2_0),

%% 	R2_1 = mqtt_client:disconnect(Publisher),
%% 	?assertEqual(ok, R2_1),

	R2_2 = mqtt_client:subscribe(Subscriber2, [{"AK_retain_test", #subscription_options{max_qos = QoS,retain_handling=1}, F(2)}]),  %% send for new subs
	?assertEqual({suback,[QoS],[]}, R2_2),
	
	R2_3 = mqtt_client:subscribe(Subscriber2, [{"AK_retain_test", #subscription_options{max_qos = QoS,retain_handling=1}, F(3)}]),  %% NOT send for exist subs
	?assertEqual({suback,[QoS],[]}, R2_3),

	R2_4 = mqtt_client:subscribe(Subscriber2, [{"AK_retain_test", #subscription_options{max_qos = QoS,retain_handling=0}, F(4)}]), %% send anyway
	?assertEqual({suback,[QoS],[]}, R2_4),

	R2_5 = mqtt_client:subscribe(Subscriber2, [{"AK_retain_test", #subscription_options{max_qos = QoS,retain_handling=2}, F(5)}]), %% DO NOT send
	?assertEqual({suback,[QoS],[]}, R2_5),

	R2_7 = mqtt_client:unsubscribe(Subscriber1, ["AK_retain_test"]), 
	?assertEqual({unsuback,[0],[]}, R2_7),
	R2_8 = mqtt_client:unsubscribe(Subscriber2, ["AK_retain_test"]), 
	?assertEqual({unsuback,[0],[]}, R2_8),

	R2_9 = mqtt_client:publish(Publisher, #publish{topic = "AK_retain_test", qos = QoS, retain = 1}, <<>>), %% clean retain msg
	?assertEqual(ok, R2_9),

	R2_6 = mqtt_client:subscribe(Subscriber2, [{"AK_retain_test", #subscription_options{max_qos = QoS,retain_handling=0}, F(6)}]), %% DO NOT SEND 
	?assertEqual({suback,[QoS],[]}, R2_6),

	W = wait_all(3),

	unregister(test_result),
	?assert(W),
	?PASSED
end}.

retain_3({QoS, retain} = _X, [Publisher, Subscriber1, Subscriber2] = _Conns) -> {"retain QoS=" ++ integer_to_list(QoS) ++ ".", timeout, 100, fun() ->
	register(test_result, self()),
  
	F = fun(_SubN) ->
			fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg} = PubRec} = _Arg) ->
					 <<"Test 1 retain message RetainFlag=", MsgRetFl:1/bytes>> = Msg,
					 MsgRetainFlag = list_to_integer(binary_to_list(MsgRetFl)),
					 RetainFlag = PubRec#publish.retain,
					 ?debug_Fmt("::test:: Subscriber ~p~n callback: ~128p; retain=~p/~p~n",[_SubN, _Arg, MsgRetainFlag, RetainFlag]),
					 ?assertEqual(QoS, Q#subscription_options.max_qos),
					 ?assertEqual("AK_retain_test", Topic),
					 case _SubN of
							1 -> if (MsgRetainFlag == 2) -> ?assertEqual(1, RetainFlag); true -> ?assertEqual(0, RetainFlag) end;
							2 -> if (MsgRetainFlag == 2) -> ?assertEqual(1, RetainFlag); true -> ?assertEqual(MsgRetainFlag, RetainFlag) end
					 end,
					 test_result ! done 
			end
			end,
	R1_0 = mqtt_client:publish(Publisher, #publish{topic = "AK_retain_test", qos = QoS, retain = 1}, <<"Test 1 retain message RetainFlag=2">>),
	?assertEqual(ok, R1_0),

	timer:sleep(100),

	R2_0 = mqtt_client:subscribe(Subscriber1, [{"AK_retain_test", #subscription_options{max_qos = QoS,retain_handling=0, retain_as_published=0}, F(1)}]), 
	?assertEqual({suback,[QoS],[]}, R2_0),

	R2_2 = mqtt_client:subscribe(Subscriber2, [{"AK_retain_test", #subscription_options{max_qos = QoS,retain_handling=0, retain_as_published=1}, F(2)}]), 
	?assertEqual({suback,[QoS],[]}, R2_2),
	
	R1_1 = mqtt_client:publish(Publisher, #publish{topic = "AK_retain_test", qos = QoS, retain = 1}, <<"Test 1 retain message RetainFlag=1">>),
	?assertEqual(ok, R1_1),

	R1_2 = mqtt_client:publish(Publisher, #publish{topic = "AK_retain_test", qos = QoS, retain = 0}, <<"Test 1 retain message RetainFlag=0">>),
	?assertEqual(ok, R1_2),

	W = wait_all(6),

	unregister(test_result),
	?assert(W),
	?PASSED
end}.

subscription_option({QoS, retain} = _X, [_Publisher, Subscriber1, Subscriber2] = _Conns) -> {"retain QoS=" ++ integer_to_list(QoS) ++ ".", timeout, 100, fun() ->
	register(test_result, self()),

	F = fun(_SubN) ->
			fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
					 ?debug_Fmt("::test:: Subscriber ~p callback: ~100p",[_SubN, _Arg]),
					 ?assertEqual(QoS, Q#subscription_options.max_qos),
					 ?assertEqual("AK_retain_test", Topic),
					 ?assertEqual(<<"Test 1 retain message QoS=", (list_to_binary((integer_to_list(QoS))))/binary>>, Msg),
					 test_result ! done 
			end
		end,

	R2_0 = mqtt_client:subscribe(Subscriber1, [{"AK_retain_test", #subscription_options{max_qos = QoS, nolocal=0}, F(1)}]), 
	?assertEqual({suback,[QoS],[]}, R2_0),

	R2_2 = mqtt_client:subscribe(Subscriber2, [{"AK_retain_test", #subscription_options{max_qos = QoS, nolocal=1}, F(2)}]), 
	?assertEqual({suback,[QoS],[]}, R2_2),
	
	R1_1 = mqtt_client:publish(Subscriber1, #publish{topic = "AK_retain_test", qos = QoS}, <<"Test 1 retain message QoS=", (list_to_binary((integer_to_list(QoS))))/binary>>),
	?assertEqual(ok, R1_1),

	R1_2 = mqtt_client:publish(Subscriber2, #publish{topic = "AK_retain_test", qos = QoS}, <<"Test 1 retain message QoS=", (list_to_binary((integer_to_list(QoS))))/binary>>),
	?assertEqual(ok, R1_2),

	W = wait_all(3),

	unregister(test_result),
	?assert(W),
	?PASSED
end}.


subscription_id({QoS, retain} = _X, [Publisher, Subscriber1, Subscriber2] = _Conns) -> {"retain QoS=" ++ integer_to_list(QoS) ++ ".", timeout, 100, fun() ->
	register(test_result, self()),

	F = fun(_SubN) ->
			fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg, properties=Props}} = _Arg) -> 
					 ?debug_Fmt("::test:: Subscriber ~p callback: ~100p",[_SubN, _Arg]),
					 ?assertEqual(QoS, Q#subscription_options.max_qos),
					 if _SubN == 1 -> ?assertEqual(37,proplists:get_value(?Subscription_Identifier, Props, 0));
							_SubN == 2 -> ?assertEqual(38,proplists:get_value(?Subscription_Identifier, Props, 0))
					 end,
					 ?assertEqual("AK_retain_test", Topic),
					 ?assertEqual(<<"Test 1 retain message QoS=", (list_to_binary((integer_to_list(QoS))))/binary>>, Msg),
					 test_result ! done 
			end
		end,

	R2_0 = mqtt_client:subscribe(Subscriber1, [{"AK_retain_test", #subscription_options{max_qos = QoS}, F(1)}], [{?Subscription_Identifier, 37}]), 
	?assertEqual({suback,[QoS],[]}, R2_0),

	R2_2 = mqtt_client:subscribe(Subscriber2, [{"AK_retain_test", #subscription_options{max_qos = QoS}, F(2)}], [{?Subscription_Identifier, 38}]), 
	?assertEqual({suback,[QoS],[]}, R2_2),

	R1_1 = mqtt_client:publish(Publisher, #publish{topic = "AK_retain_test", qos = QoS}, <<"Test 1 retain message QoS=", (list_to_binary((integer_to_list(QoS))))/binary>>),
	?assertEqual(ok, R1_1),

 	R1_2 = mqtt_client:publish(Subscriber2, #publish{topic = "AK_retain_test", qos = QoS}, <<"Test 1 retain message QoS=", (list_to_binary((integer_to_list(QoS))))/binary>>),
 	?assertEqual(ok, R1_2),

	W = wait_all(4),

	unregister(test_result),
	?assert(W),
	?PASSED
end}.

