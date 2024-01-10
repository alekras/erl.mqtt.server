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
%% @since 2017-11-16
%% @copyright 2015-2023 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc This module implements a tesing of MQTT restful service.

-module(restful).

%%
%% Include files
%%
%% -include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("mqtt_common/include/mqtt.hrl").
-include("test_rest.hrl").

-export([
	post/0,
	get_user/0,
	get_status/0,
	get_all_statuses/0,
	delete/0
]).

-import(testing, [wait_all/1]).
%%
%% API Functions
%%

post() ->
	Req0 = {
		?TEST_REST_SERVER_URL ++ "/rest/user/Alexei",
		headers(),
		"application/json",
		"{\"password\":\"aaaaaaa\", \"roles\":[\"ADMIN\",\"USER\"]}"
	},
	Response0 = httpc:request(post, Req0, [{timeout, 1000}], []),
	?debug_Fmt(" >>> Response #0: ~p~n", [Response0]),
	{ok, {{_Pr, Status, _}, _Headers, Body}} = Response0,
	?assertEqual(201, Status),
	?assertEqual("{}", Body),

	?PASSED.

get_user() ->
	Req0 = {
		?TEST_REST_SERVER_URL ++ "/rest/user/Alexei",
		headers()
	},
	Response0 = httpc:request(get, Req0, [], []),
	?debug_Fmt("Response #0: ~p~n", [Response0]),
	{ok, {{_Pr, Status, _}, _Headers, Body}} = Response0,
	?assertEqual(200, Status),
	?assertEqual("{\"password\":\"5d793fc5b00a2348c3fb9ab59e5ca98a\",\"roles\":[\"ADMIN\",\"USER\"]}", Body),

	Req1 = {
		?TEST_REST_SERVER_URL ++ "/rest/user/Alexi",
		headers()
	},
	Response1 = httpc:request(get, Req1, [], []),
	?debug_Fmt("Response #1: ~p~n", [Response1]),
	{ok, {{_Pr1, Status1, _}, _Headers1, Body1}} = Response1,
	?assertEqual(404, Status1),
	?assertEqual("{\"code\":\"404\",\"message\":\"User does not found.\"}", Body1),

	?PASSED.

get_status() ->
	Req0 = {
		?TEST_REST_SERVER_URL ++ "/rest/user/Alexei/status",
		headers()
	},
	Response0 = httpc:request(get, Req0, [], []),
	?debug_Fmt("Response #0: ~p~n", [Response0]),
	{ok, {{_Pr, Status, _}, _Headers, Body}} = Response0,
	?assertEqual(200, Status),
	?assertEqual("{\"id\":\"Alexei\",\"status\":\"off\"}", Body),

	Req1 = {
		?TEST_REST_SERVER_URL ++ "/rest/user/Alexi/status",
		headers()
	},
	Response1 = httpc:request(get, Req1, [], []),
	?debug_Fmt("Response #1: ~p~n", [Response1]),
	{ok, {{_Pr1, Status1, _}, _Headers1, Body1}} = Response1,
	?assertEqual(404, Status1),
	?assertEqual("{\"code\":\"404\",\"message\":\"User does not found.\"}", Body1),

	?PASSED.

get_all_statuses() ->
	Req0 = {
		?TEST_REST_SERVER_URL ++ "/rest/user/status" ++ "?users=Alexei,sam,john",
		headers()
	},
	Response0 = httpc:request(get, Req0, [], []),
	?debug_Fmt("Response #0: ~p~n", [Response0]),
	{ok, {{_Pr, Status, _}, _Headers, Body}} = Response0,
	?assertEqual(200, Status),
	?assertEqual("[{\"id\":\"Alexei\",\"status\":\"off\"},{\"id\":\"sam\",\"status\":\"notFound\"},{\"id\":\"john\",\"status\":\"notFound\"}]", Body),

	?PASSED.

delete() ->
	Req0 = {
		?TEST_REST_SERVER_URL ++ "/rest/user/Alexei",
		headers()
	},
	Response0 = httpc:request(delete, Req0, [], []),
	?debug_Fmt("Response #0: ~p~n", [Response0]),
	{ok, {{_Pr, Status, _}, _Headers, Body}} = Response0,
	?assertEqual(200, Status),
	?assertEqual("{}", Body),

	Response1 = httpc:request(get, Req0, [], []),
	?debug_Fmt("Response #1: ~p~n", [Response1]),
	{ok, {{_Pr1, Status1, _}, _Headers1, Body1}} = Response1,
	?assertEqual(404, Status1),
	?assertEqual("{\"code\":\"404\",\"message\":\"User does not found.\"}", Body1),

	?PASSED.

headers() ->
[
 {"X-Forwarded-For", "localhost"},
 {"Accept", "application/json"},
 {"X-API-Key", "mqtt-rest-api"}
].
