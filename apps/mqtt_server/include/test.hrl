%%
%% Copyright (C) 2016-2016 by krasnop@bellsouth.net (Alexei Krasnopolski)
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

%-define(TEST_SERVER_HOST_NAME, "localhost").
%-define(TEST_SERVER_HOST_NAME, {127,0,0,1}).
-define(TEST_SERVER_HOST_NAME, "MacBook-Pro").
-define(TEST_USER, "guest").
-define(TEST_PASSWORD, <<"guest">>).
-define(TEST_PROTOCOL, '3.1.1').

-define(CONN_TYPE, wss). %% clear | ssl | ws | wss

-if(?CONN_TYPE == clear).
%%%%%%%%%%%%% Clear socket test %%%%%%%%%%%%%%%%% 
	-define(TEST_CONN_TYPE, {conn_type, clear}). %% Clear tcp for client
	-define(TEST_SERVER_PORT, 18883). %% Erlang
	%-define(TEST_SERVER_PORT, 2883). %% Mosquitto
	%-define(TEST_SERVER_PORT, 1883). %% RabbitMQ
	-define(TEST_REST_SERVER_URL, "http://" ++ ?TEST_SERVER_HOST_NAME ++ ":8880").
-elif(?CONN_TYPE == ssl).
%%%%%%%%%%%%% SSL/TSL socket test %%%%%%%%%%%%%%%%% 
	-define(TEST_CONN_TYPE, {conn_type, ssl}). %% TSL/SSL for client
	-define(TEST_SERVER_PORT, 18483). %% Erlang TSL
	%-define(TEST_SERVER_PORT, 2884). %% Mosquitto TSL
	-define(TEST_REST_SERVER_URL, "http://" ++ ?TEST_SERVER_HOST_NAME ++ ":8880").
-elif(?CONN_TYPE == ws).
%%%%%%%%%%%%% Clear WEB socket test %%%%%%%%%%%%%%%%% 
	-define(TEST_CONN_TYPE, {conn_type, web_socket}). %% Web socket connection for client
	-define(TEST_SERVER_PORT, 8880). %% Erlang WEBSocket
	-define(TEST_REST_SERVER_URL, "http://" ++ ?TEST_SERVER_HOST_NAME ++ ":8880").
-elif(?CONN_TYPE == wss).
%%%%%%%%%%%%% SSL/TLS WEB socket test %%%%%%%%%%%%%%%%% 
	-define(TEST_CONN_TYPE, {conn_type, web_sec_socket}). %% Web socket connection for client
	-define(TEST_SERVER_PORT, 4443). %% Erlang WEBSocket
	-define(TEST_REST_SERVER_URL, "http://" ++ ?TEST_SERVER_HOST_NAME ++ ":8880").
-endif.

-define(debug_Msg(S),
	(begin
		timer:sleep(1),
		io:fwrite(user, <<"~n~s">>, [S])
%		io:fwrite(<<"~s\n">>, [S])
	 end)).
-define(debug_Fmt(S, As), (?debug_Msg(io_lib:format((S), (As))))).
-define(PASSED, (?debug_Msg("    +++ Passed "))).
-define(passed, (?debug_Msg("    +++ Passed with "))).
