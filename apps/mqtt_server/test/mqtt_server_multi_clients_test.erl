%% @author alexei
%% @doc @todo Add description to mqtt_server_multi_clients.


-module(mqtt_server_multi_clients_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("mqtt_common/include/mqtt.hrl").
-include("test.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	start_task/2,
	publisher_process/5,
	subscriber_process/5
]).

mqtt_multi_client_test_() ->
	[ 
		{ setup, 
			fun do_start/0, 
			fun do_stop/1, 
				{ foreachx, 
					fun do_setup/1, 
					fun do_cleanup/2, 
					[
						{{1, start_test}, fun start_task/2}
					]}
		}
	].

start_task(_, []) -> {"start task", timeout, 150, fun() ->
	N_Pub = 500,
	Sub_List = [
		{subscriber01, [{"Winter/Jan/+", 0}, {"Spring/Apr/+", 1}], 4 * N_Pub},
		{subscriber02, [{"Summer/Jun/15", 0}, {"Fall/Nov/15", 2}], 5 * N_Pub},
		{subscriber03, [{"Winter/#", 0}, {"Summer/+/15", 0}], 5 * N_Pub},
		{subscriber04, [{"Fall/Nov/15", 2}, {"Winter/Feb/15", 1}], 4 * N_Pub},
		{subscriber05, [{"Spring/Apr/15", 1}, {"Summer/Jun/15", 0}], 3 * N_Pub}
	],
	Pub_List = [
		{publisher01, [{"Winter/Jan/15", 1}, {"Summer/Jun/15", 0}], N_Pub},
		{publisher02, [{"Summer/Jun/15", 0}, {"Fall/Nov/15", 2}],   N_Pub},
		{publisher03, [{"Winter/Feb/15", 1}, {"Spring/Apr/30", 1}], N_Pub},
		{publisher04, [{"Fall/Nov/15", 2}, {"Winter/Jan/15", 0}],   N_Pub},
		{publisher05, [{"Spring/Apr/15", 1}, {"Fall/Nov/15", 2}],   N_Pub}
	],
	[start_subscriber_process(Name, Topics, N, self()) || {Name, Topics, N} <- Sub_List],
	timer:sleep(1000),
	[start_publisher_process(Name, Topics, N, self()) || {Name, Topics, N} <- Pub_List],
	counter(10),
	?PASSED
end}.

counter(0) -> ok;
counter(M) ->
	receive
		stop -> 
			?assert(true),
			counter(M - 1);
		error -> 
			?assert(false), 
			counter(M - 1)
	end.
%% ====================================================================
%% Internal functions
%% ====================================================================

do_start() ->
	C = application:start(mqtt_client),
	?assertEqual(ok, C).

do_stop(_R) ->
	C = application:stop(mqtt_client),
	?assertEqual(ok, C).

do_setup({_, start_test} = _X) ->
  ?debug_Fmt("~n::test:: setup before: ~p",[_X]),
	[].

do_cleanup({_, _} = _X, []) ->
  ?debug_Fmt("~n::test:: clean up after: ~p",[_X]).

start_publisher_process(Name, Topics, N, Parent_Pid) ->
	Conn_config = (testing:get_connect_rec())#connect{version= '5.0'},
	Pub_Pid = mqtt_client:connect(
		Name, 
		Conn_config#connect{client_id = atom_to_list(Name)}, 
		?TEST_SERVER_HOST_NAME, 
		?TEST_SERVER_PORT, 
		[?TEST_CONN_TYPE]),
	spawn_link(?MODULE, publisher_process, [Pub_Pid, Name, Topics, Parent_Pid, N]).

publisher_process(Pid, _Name, _Topics, Parent_Pid, 0) ->
	mqtt_client:disconnect(Pid),
	Parent_Pid ! stop;
publisher_process(Pid, Name, Topics, Parent_Pid, N) ->
	[mqtt_client:publish(Pid, #publish{topic = Topic, qos = QoS}, gen_payload(N, Name)) || {Topic, QoS} <- Topics],
%	timer:sleep(50), %% need for Mosquitto
	publisher_process(Pid, Name, Topics, Parent_Pid, N-1).

gen_payload(N, Name) ->
	term_to_binary([{name, Name}, {number, N}, {message, "Test message."}]).

start_subscriber_process(Name, Topics, N, Parent_Pid) ->
	Conn_config = (testing:get_connect_rec())#connect{version= '5.0'},
	Subs_Pid = mqtt_client:connect(
		Name, 
		Conn_config#connect{client_id = atom_to_list(Name)}, 
		?TEST_SERVER_HOST_NAME, 
		?TEST_SERVER_PORT, 
		[?TEST_CONN_TYPE]),
	Pid = erlang:spawn_link(?MODULE, subscriber_process, [Subs_Pid, Name, Topics, Parent_Pid, N + 1]),

	CallBack = fun(A) -> process_message(A, Pid) end,
	T2 = [{T, Q, CallBack} || {T, Q} <- Topics],
	mqtt_client:subscribe(Subs_Pid, T2).

subscriber_process(Pid, Name, Topics, Parent_Pid, 0) ->
	?debug_Fmt("::test:: subscriber '~p' receive +1 message.",[Name]),
	mqtt_client:unsubscribe(Pid, [T || {T, _} <- Topics]),
	mqtt_client:disconnect(Pid),
	Parent_Pid ! error;
subscriber_process(Pid, Name, Topics, Parent_Pid, N) ->
	receive
		{Pub_Name, Mess_Number, Message} -> 
%			?debug_Fmt("::test:: subscriber '~p' processed message[~p]: ~128p",[Name, N, {Pub_Name, Mess_Number, Message}]),
			subscriber_process(Pid, Name, Topics, Parent_Pid, N - 1)
	after 10000 -> 
			?debug_Fmt("::test:: subscriber '~p' catched timeout while waiting message[~p]",[Name, N]),
			mqtt_client:unsubscribe(Pid, [T || {T, _} <- Topics]),
			mqtt_client:disconnect(Pid),
			case N of
				1 -> Parent_Pid ! stop;
				_ -> Parent_Pid ! error
			end
	end.	

process_message({_Q, #publish{payload= Msg}} = _A, Dest_Pid) -> 
%  ?debug_Fmt("~n::test:: process message: ~128p",[A]),
	Payload = binary_to_term(Msg),
	Name = proplists:get_value(name, Payload),
	N = proplists:get_value(number, Payload),
	M = proplists:get_value(message, Payload),
	Dest_Pid ! {Name, N, M}.
