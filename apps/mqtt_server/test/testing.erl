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

%% @since 2015-12-25
%% @copyright 2015-2023 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc @todo Add description to testing.


-module(testing).
-include_lib("eunit/include/eunit.hrl").
-include_lib("mqtt_common/include/mqtt.hrl").
-include("test.hrl").

-define(
	CONN_REC, 
	(#connect{
		user_name = ?TEST_USER,
		password = ?TEST_PASSWORD,
		host = ?TEST_SERVER_HOST_NAME,
		port = ?TEST_SERVER_PORT,
		conn_type = ?TEST_CONN_TYPE,
		keep_alive = 60000,
		version = ?TEST_PROTOCOL
	})
).

%%
%% API functions
%%
-export([
	do_setup/1, 
	do_cleanup/2, 
	do_start/0, 
	do_stop/1,
	get_connect_rec/0,
	get_storage/1, 
	wait_all/1]).

do_start() ->
	S = application:ensure_all_started(mqtt_server),
	?assertMatch({ok,_}, S),

	(get_storage(server)):user(save, #user{user_id = <<"guest">>, password = <<"guest">>}),
	(get_storage(server)):user(save, #user{user_id = <<"admin">>, password = <<"admin">>}),

	C = application:start(mqtt_client),
	?assertEqual(ok, C).

do_stop(_R) ->
	C = application:stop(mqtt_client),
	?assertEqual(ok, C),
	S = application:stop(mqtt_server),
	?assertEqual(ok, S).

connect(Name) when is_atom(Name) ->
	mqtt_client:connect(
		Name, 
		?CONN_REC#connect{client_id = atom_to_list(Name)}, 
		undefined
	);	
connect(Name) when is_list(Name) ->
	mqtt_client:connect(
		list_to_atom(Name), 
		?CONN_REC#connect{client_id = Name}, 
		undefined
	).	

do_setup({_, publish} = _X) ->
%  ?debug_Fmt("~n::test:: setup before: ~p",[_X]),
	[connect(publisher), connect(subscriber)];
do_setup({_, session} = _X) ->
%  ?debug_Fmt("~n::test:: setup before: ~p",[_X]),
	P1 = mqtt_client:connect(
		publisher, 
		?CONN_REC#connect{client_id = "publisher", clean_session = 0}, 
		?TEST_SERVER_HOST_NAME, ?TEST_SERVER_PORT, 
		[?TEST_CONN_TYPE]
	),
	S1 = mqtt_client:connect(
		subscriber, 
		?CONN_REC#connect{client_id = "subscriber", clean_session = 0}, 
		?TEST_SERVER_HOST_NAME, ?TEST_SERVER_PORT, 
		[?TEST_CONN_TYPE]
	),
	[P1, S1];
do_setup({QoS, will} = _X) ->
%  ?debug_Fmt("~n::test:: setup before: ~p",[_X]),
	P = mqtt_client:connect(
		publisher, 
		?CONN_REC#connect{
			client_id = "publisher",
			will_publish = #publish{topic= "AK_will_test", qos= QoS, payload= <<"Test will message">>}
		}, 
		?TEST_SERVER_HOST_NAME, ?TEST_SERVER_PORT, 
		[?TEST_CONN_TYPE]
	),
	S = connect(subscriber),
	[P,S];
do_setup({QoS, will_retain} = _X) ->
%  ?debug_Fmt("~n::test:: setup before: ~p",[_X]),
	P = mqtt_client:connect(
		publisher, 
		?CONN_REC#connect{
			client_id = "publisher",
			will_publish = #publish{topic= "AK_will_retain_test", qos= QoS, retain= 1, payload= <<"Test will retain message">>}
		}, 
		?TEST_SERVER_HOST_NAME, 
		?TEST_SERVER_PORT, 
		[?TEST_CONN_TYPE]
	),
	S = connect(subscriber),
	[P,S];
do_setup({_QoS, retain} = _X) ->
%  ?debug_Fmt("~n::test:: setup before: ~p",[_X]),
	[connect(publisher), connect(subscriber01), connect(subscriber02)];
do_setup({_, keep_alive}) ->
%  ?debug_Fmt("~n::test:: setup before: ~p",[_X]),
	mqtt_client:connect(
		publisher, 
		?CONN_REC#connect{client_id = "publisher", keep_alive = 5}, 
		?TEST_SERVER_HOST_NAME, 
		?TEST_SERVER_PORT, 
		[?TEST_CONN_TYPE]
	);
do_setup(_X) ->
%  ?debug_Fmt("~n::test:: setup before: ~p",[_X]),
	connect(publisher).

do_cleanup({_, publish} = _X, [P, S] = _Pids) ->
	R1 = mqtt_client:disconnect(P),
	R2 = mqtt_client:disconnect(S),
	(get_storage(client)):cleanup(client),
	(get_storage(server)):cleanup(server),
	?assertEqual(ok, R1),
	?assertEqual(ok, R2);
%  ?debug_Fmt("::test:: teardown after: ~p  pids=~p  disconnect returns=~150p",[_X, _Pids, {R1, R2}]);
do_cleanup({_, session} = _X, [P1, S1] = _Pids) ->
	R1 = mqtt_client:disconnect(P1),
	R2 = mqtt_client:disconnect(S1),
	(get_storage(client)):cleanup(client),
	(get_storage(server)):cleanup(server),
	?assertEqual(ok, R1),
	?assertEqual(ok, R2);
%  ?debug_Fmt("::test:: teardown after: ~p  pids=~p  disconnect returns=~150p",[_X, _Pids, {R1, R2}]);
do_cleanup({_QoS, will} = _X, [P, S] = _Pids) ->
	R1 = mqtt_client:disconnect(P),
	
%% 	P1 = mqtt_client:connect(
%% 		publisher, 
%% 		?CONN_REC#connect{
%% 			client_id = "publisher",
%% 			will = 1,
%% 			will_qos = QoS,
%% 			will_message = <<"Test will message">>,
%% 			will_topic = "AK_will_test"
%% 		}, 
%% 		?TEST_SERVER_HOST_NAME,
%% 		?TEST_SERVER_PORT, 
%% 		[?TEST_CONN_TYPE]
%% 	),
%% 
%% 	R1_1 = mqtt_client:disconnect(P1),

	R2 = mqtt_client:disconnect(S),

	(get_storage(client)):cleanup(client),
	(get_storage(server)):cleanup(server),
	?assertEqual(ok, R1),
%	?assertEqual(ok, R1_1),
	?assertEqual(ok, R2);
%  ?debug_Fmt("::test:: teardown after: ~p  pids=~p  disconnect returns=~150p",[_X, _Pids, {R1, R2}]);
do_cleanup({QoS, will_retain} = _X, [P, S] = _Pids) ->
	R1 = mqtt_client:disconnect(P),
	R2 = mqtt_client:disconnect(S),

	P1 = mqtt_client:connect(
		publisher, 
		?CONN_REC#connect{
			client_id = "publisher",
			clean_session = 1
%% 			will = 1,
%% 			will_retain = 1,
%% 			will_qos = QoS,
%% 			will_message = <<"Test will retain message">>,
%% 			will_topic = "AK_will_retain_test"
		}, 
		?TEST_SERVER_HOST_NAME, ?TEST_SERVER_PORT, 
		[?TEST_CONN_TYPE]
	),
 	R1_0 = mqtt_client:publish(P1, #publish{topic = "AK_will_retain_test", retain = 1, qos = QoS}, <<>>), 
	?assertEqual(ok, R1_0),
% 	mqtt_client:publish(P1, #publish{topic = "AK_will_retain_test", retain = 1, qos = QoS}, <<>>), 
% 	mqtt_client:publish(P1, #publish{topic = "AK_will_retain_test", retain = 1, qos = QoS}, <<>>), 
% 	mqtt_client:publish(P1, #publish{topic = "AK_will_retain_test", retain = 1, qos = QoS}, <<>>), 
%	R1_1 = mqtt_client:publish(P1, #publish{topic = "AK_will_test", retain = 1, qos = QoS}, <<>>), 
%	?assertEqual(ok, R1_1),
	R3 = mqtt_client:disconnect(P1),

	(get_storage(client)):cleanup(client),
	(get_storage(server)):cleanup(server),
	?assertEqual(ok, R1),
	?assertEqual(ok, R2),
	?assertEqual(ok, R3);
%  ?debug_Fmt("::test:: teardown after: ~p  pids=~p  disconnect returns=~150p",[_X, _Pids, {R1, R2}]);
do_cleanup({QoS, retain} = _X, [P1, S1, S2] = _Pids) ->
	Rs1 = mqtt_client:disconnect(S1),
	Rs2 = mqtt_client:disconnect(S2),
	R1 = case mqtt_client:status(P1) of
				disconnected ->
					P2 = mqtt_client:connect(
						publisher, 
						?CONN_REC#connect{client_id = "publisher", clean_session = 0}, 
						?TEST_SERVER_HOST_NAME,
						?TEST_SERVER_PORT, 
						[?TEST_CONN_TYPE]
					),
					mqtt_client:publish(P2, #publish{topic = "AK_retain_test", retain = 1, qos = QoS}, <<>>), 
					mqtt_client:disconnect(P2);
				_ ->
					mqtt_client:publish(P1, #publish{topic = "AK_retain_test", retain = 1, qos = QoS}, <<>>), 
					mqtt_client:disconnect(P1)
			 end,
	(get_storage(client)):cleanup(client),
	(get_storage(server)):cleanup(server),
	?assertEqual(ok, R1),
	?assertEqual(ok, Rs1),
	?assertEqual(ok, Rs2);
%  ?debug_Fmt("::test:: teardown after: ~p  pids=~p  disconnect returns=~150p",[_X, _Pids, {R1, R2}]);
do_cleanup(_X, _Pids) ->
	R = mqtt_client:disconnect(publisher),
	(get_storage(client)):cleanup(client),
	(get_storage(server)):cleanup(server),
	?assertEqual(ok, R).

get_connect_rec() ->
	?CONN_REC#connect{client_id = "test0Client"}.

get_storage(server) ->
	case application:get_env(mqtt_server, storage, dets) of
		mysql -> mqtt_mysql_storage;
		dets -> mqtt_dets_storage
	end;
get_storage(client) ->
	case application:get_env(mqtt_client, storage, dets) of
		mysql -> mqtt_mysql_storage;
		dets -> mqtt_dets_storage
	end.
	
wait_all(N) ->
	case wait_all(N, 0) of
		{ok, _M} -> 
%			?debug_Fmt("::test:: all ~p done received.", [_M]),
			true;
		{fail, _T} -> 
			?debug_Fmt("::test:: ~p done have not received.", [N - _T]), 
			false
%			?assert(true)
	end
	and
	case wait_all(100, 0) of
		{fail, 0} -> 
%			?debug_Fmt("::test:: ~p unexpected done received.", [0]),
			true;
		{fail, _Z} -> 
			?debug_Fmt("::test:: ~p unexpected done received.", [_Z]),
			false;
		{ok, _R} -> 
			?debug_Fmt("::test:: ~p unexpected done received.", [_R]), 
			false
%			?assert(true)
	end.

wait_all(0, M) -> {ok, M};
wait_all(N, M) ->
	receive
		done -> wait_all(N - 1, M + 1)
	after 200 -> {fail, M}
	end.
