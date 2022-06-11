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

-module(retain).

%%
%% Include files
%%
%% -include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("mqtt_common/include/mqtt.hrl").
-include("test.hrl").

-export([
  retain_0/2,
  retain_1/2
]).
-import(testing, [wait_all/1]).
%%
%% API Functions
%%

%% .
retain_0({QoS, retain} = _X, [Publisher, Subscriber1, Subscriber2] = _Conns) -> {"retain QoS=" ++ integer_to_list(QoS) ++ ".", timeout, 100, fun() ->
	register(test_result, self()),

	F1 = fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, retain=_Ret, payload= Msg}} = _Arg) -> 
					 ?debug_Fmt("::test:: fun 1 callback: ~100p",[_Arg]),
					 ?assertEqual(QoS, Q),
					 ?assertEqual("AK_retain_test", Topic),
					 ?assertEqual(<<"Test retain message qos=", (integer_to_binary(QoS))/binary>>, Msg),
					 test_result ! done 
			end,
	F2 = fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, retain=_Ret, payload= Msg}} = _Arg) -> 
					 ?debug_Fmt("::test:: fun 2 callback: ~100p",[_Arg]),
					 ?assertEqual(QoS, Q),
					 ?assertEqual("AK_retain_test", Topic),
					 ?assertEqual(<<"Test retain message qos=", (integer_to_binary(QoS))/binary>>, Msg),
					 test_result ! done 
			end,
	R1_0 = mqtt_client:subscribe(Subscriber1, [{"AK_retain_test", QoS, F1}]), 
	?assertEqual({suback,[QoS],[]}, R1_0),
timer:sleep(200),
	R2_0 = mqtt_client:publish(Publisher, #publish{topic = "AK_retain_test", qos = QoS, retain = 1}, <<"Test retain message qos=", (integer_to_binary(QoS))/binary>>),
	?assertEqual(ok, R2_0),
timer:sleep(200),
	R1_1 = mqtt_client:subscribe(Subscriber2, [{"AK_retain_test", QoS, F2}]), 
	?assertEqual({suback,[QoS],[]}, R1_1),

	W = wait_all(2),

	unregister(test_result),
	?assert(W),
	?PASSED
end}.

%% .
retain_1({QoS, retain} = _X, [Publisher, Subscriber1, Subscriber2] = _Conns) -> {"retain QoS=" ++ integer_to_list(QoS) ++ ".", timeout, 100, fun() ->
	register(test_result, self()),

	F1 = fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
					 ?debug_Fmt("::test:: fun 1 callback: ~100p",[_Arg]),
					 ?assertEqual(QoS, Q),
					 ?assertEqual("AK_retain_test", Topic),
					 ?assertEqual(<<"Test retain message QoS=", (integer_to_binary(QoS))/binary>>, Msg),
					 test_result ! done 
			end,
	F2 = fun({Q, #publish{topic= Topic, qos=_QoS, dup=_Dup, payload= Msg}} = _Arg) -> 
					 ?debug_Fmt("::test:: fun 2 callback: ~100p",[_Arg]),
					 ?assertEqual(QoS, Q),
					 ?assertEqual("AK_retain_test", Topic),
					 ?assertEqual(<<"Test retain message QoS=", (integer_to_binary(QoS))/binary>>, Msg),
					 test_result ! done 
			end,
	R1_0 = mqtt_client:publish(Publisher, #publish{topic = "AK_retain_test", qos = QoS, retain = 1}, <<"Test retain message QoS=", (integer_to_binary(QoS))/binary>>),
	?assertEqual(ok, R1_0),
timer:sleep(200),

	R2_0 = mqtt_client:subscribe(Subscriber1, [{"AK_retain_test", QoS, F1}]), 
	?assertEqual({suback,[QoS],[]}, R2_0),

	R2_1 = mqtt_client:disconnect(Publisher),
	?assertEqual(ok, R2_1),

	R2_2 = mqtt_client:subscribe(Subscriber2, [{"AK_retain_test", QoS, F2}]), 
	?assertEqual({suback,[QoS],[]}, R2_2),

	W = wait_all(2),

	unregister(test_result),
	?assert(W),
	?PASSED
end}.
