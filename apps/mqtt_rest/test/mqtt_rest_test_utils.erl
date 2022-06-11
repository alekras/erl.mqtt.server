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

%% @since 2022-06-01
%% @copyright 2015-2017 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc @todo Add description to testing.


-module(mqtt_rest_test_utils).
-include_lib("eunit/include/eunit.hrl").
-include_lib("mqtt_common/include/mqtt.hrl").
-include("test_rest.hrl").

-define(CONN_REC, (#connect{user_name = ?TEST_USER, password = ?TEST_PASSWORD, keep_alive = 60000, version = ?TEST_PROTOCOL}) ).

%%
%% API functions
%%
-export([
	do_setup/1, 
	do_cleanup/2, 
	do_start/0, 
	do_stop/1
]).

do_start() ->
	?debug_Fmt(">>> do start >>> ~n", []),
	S = application:ensure_all_started(mqtt_rest),
	Storage =
	case application:get_env(mqtt_server, storage, dets) of
		mysql -> mqtt_mysql_dao;
		dets -> mqtt_dets_dao
	end,
	Storage:start(server),
%	[ ?debug_Fmt(" ### ~p", [T]) || T <- application:which_applications()],
	?assertMatch({ok,_}, S).

do_stop(_R) ->
%	[ ?debug_Fmt(" --- ~p", [T]) || T <- application:which_applications()],
	S = application:stop(cowboy),
	?debug_Fmt("<<< did stop <<<", []),
	?assertEqual(ok, S).

do_setup({_, _} = _X) ->
	ok;
do_setup(_X) ->
	ok.

do_cleanup({_, publish} = _X, [P, S] = _Pids) ->
	ok;
do_cleanup(_X, _Pids) ->
	ok.

get_storage(server) ->
	case application:get_env(mqtt_server, storage, dets) of
		mysql -> mqtt_mysql_dao;
		dets -> mqtt_dets_dao
	end;
get_storage(client) ->
	case application:get_env(mqtt_client, storage, dets) of
		mysql -> mqtt_mysql_dao;
		dets -> mqtt_dets_dao
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
