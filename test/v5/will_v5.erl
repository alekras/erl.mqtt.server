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
%% @since 2016-09-29
%% @copyright 2015-2020 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc This module implements a tesing of MQTT will.

-module(will_v5).

%%
%% Include files
%%
%% -include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("mqtt_common/include/mqtt.hrl").
-include("test.hrl").

-export([
  will_a/2,
  will_0/2,
	will_delay/2,
	will_retain/2
]).
-import(testing_v5, [wait_all/1]).
%%
%% API Functions
%%

will_a({0, will} = _X, [Publisher, Subscriber] = _Conns) -> {"will QoS=0.", timeout, 100, fun() ->
	register(test_result, self()),

	F = fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
%					 ?debug_Fmt("::test:: fun callback: ~100p",[_Arg]),
					 ?assertEqual(0, Q#subscription_options.max_qos),
					 ?assertEqual("AK_will_test", Topic),
					 ?assertEqual(<<"Test will message">>, Msg),
					 test_result ! done 
			end,
	R1_0 = mqtt_client:subscribe(Subscriber, [{"AK_will_test", #subscription_options{max_qos=0}, F}]), 
	?assertEqual({suback,[0],[]}, R1_0),
%% generate connection close:
	R2 = mqtt_client:disconnect(Publisher),
%	?debug_Fmt("::test:: after stop publisher: ~100p",[R2]),
	?assertEqual(ok, R2),

	W = wait_all(0),

	unregister(test_result),
	?assert(W),
	?PASSED
end}.

will_0({QoS, will} = _X, [Publisher, Subscriber] = _Conns) -> {"will QoS=" ++ integer_to_list(QoS) ++ ".", timeout, 100, fun() ->
	register(test_result, self()),
	
	F = fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
%					 ?debug_Fmt("::test:: fun callback: ~100p~n",[_Arg]),
					 ?assertEqual(QoS, Q#subscription_options.max_qos),
					 ?assertEqual("AK_will_test", Topic),
					 ?assertEqual(<<"Test will message">>, Msg),
					 test_result ! done 
			end,
	R1_0 = mqtt_client:subscribe(Subscriber, [{"AK_will_test", #subscription_options{max_qos=QoS}, F}]), 
	?assertEqual({suback,[QoS],[]}, R1_0),
%% generate connection lost:
	gen_server:call(Publisher, {set_test_flag, break_connection}),
	try
		mqtt_client:publish(Publisher, #publish{topic = "AKtest", qos = 0}, <<"Test Payload QoS = 0. annon. function callback. ">>)
	catch
		_:_ -> ok
	end,

  W = wait_all(1),

	unregister(test_result),
	?assert(W),
	?PASSED
end}.

will_delay({QoS, will_delay} = _X, [Publisher, Subscriber] = _Conns) -> {"will QoS=" ++ integer_to_list(QoS) ++ ".", timeout, 100, fun() ->
	register(test_result, self()),
	T1 = erlang:timestamp(),
	F = fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) ->
					 T2 = timer:now_diff(erlang:timestamp(), T1) div 1000000,
					 ?debug_Fmt("::test:: fun callback: ~100p, elapsed time = ~p~n",[_Arg, T2]),
					 ?assertEqual(QoS, Q#subscription_options.max_qos),
					 ?assertEqual("AK_will_test", Topic),
					 ?assertEqual(<<"Test will message">>, Msg),
					 ?assert(T2 >= 5),
					 test_result ! done 
			end,
	R1_0 = mqtt_client:subscribe(Subscriber, [{"AK_will_test", #subscription_options{max_qos=QoS}, F}]), 
	?assertEqual({suback,[QoS],[]}, R1_0),
%% generate connection lost:
	gen_server:call(Publisher, {set_test_flag, break_connection}),
	try
		mqtt_client:publish(Publisher, #publish{topic = "AKtest", qos = 0}, <<"Test Payload QoS = 0.">>)
	catch
		_:_ -> ok
	end,
	timer:sleep(6000),
  W = wait_all(1),

	unregister(test_result),
	?assert(W),
	?PASSED
end}.

will_retain({QoS, will_retain} = _X, [Publisher, Subscriber] = _Conns) -> {"will with retain QoS=" ++ integer_to_list(QoS) ++ ".", timeout, 100, fun() ->
	register(test_result, self()),

	F = fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
%					 ?debug_Fmt("::test:: fun F callback: ~100p~n",[_Arg]),
					 ?assertEqual(QoS, Q),
					 ?assertEqual("AK_will_retain_test", Topic),
					 ?assertEqual(<<"Test will retain message">>, Msg),
					 test_result ! done 
			end,
	F1 = fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
%					 ?debug_Fmt("::test:: fun F1 callback: ~100p~n",[_Arg]),
					 ?assertEqual(QoS, Q),
					 ?assertEqual("AK_will_retain_test", Topic),
					 ?assertEqual(<<"Test will retain message">>, Msg),
					 test_result ! done 
			end,
	R1_0 = mqtt_client:subscribe(Subscriber, [{"AK_will_retain_test", QoS, F}]), 
	?assertEqual({suback,[QoS],[]}, R1_0),
%% generate connection lost:
	gen_server:call(Publisher, {set_test_flag, break_connection}),
	try
		mqtt_client:publish(Publisher, #publish{topic = "AKtest", qos = QoS}, <<"Test Payload QoS = 2. annon. function callback. ">>)
	catch
		_:_ -> ok
	end,

	Subscriber_2 = mqtt_client:connect(
		subscriber_2, 
		#connect{
			client_id = "subscriber_2",
			user_name = "guest", password = <<"guest">>,
			clean_session = 1,
			keep_alive = 60000,
			version = ?TEST_PROTOCOL
		}, 
		?TEST_SERVER_HOST_NAME, ?TEST_SERVER_PORT,
		[?TEST_CONN_TYPE]
	),
	?assert(is_pid(Subscriber_2)),
	R2_0 = mqtt_client:subscribe(Subscriber_2, [{"AK_will_retain_test", QoS, F1}]), 
	?assertEqual({suback,[QoS],[]}, R2_0),

  W = wait_all(2),

	unregister(test_result),
	mqtt_client:disconnect(Subscriber_2),
	?assert(W),
	?PASSED
end}.
