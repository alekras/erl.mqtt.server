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
%% @since 2022-06-01
%% @copyright 2015-2023 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc This module is running erlang unit tests.

-module(mqtt_rest_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("test_rest.hrl").

%%
%% API Functions
%%
mqtt_server_test_() ->
	[ 
		{ setup, 
			fun mqtt_rest_test_utils:do_start/0, 
			fun mqtt_rest_test_utils:do_stop/1, 
			{inorder, [
				{"rest service", timeout, 15, fun restful:post/0},
				{"rest service", fun restful:get_user/0},
				{"rest service", fun restful:get_status/0},
				{"rest service", fun restful:get_all_statuses/0},
				{"rest service", fun restful:delete/0},
				{foreachx, 
					fun mqtt_rest_test_utils:do_setup/1, 
					fun mqtt_rest_test_utils:do_cleanup/2, 
					[
						{{1, keep_alive}, fun keep_alive/2}
					]
				}
			]}
		}
	].

keep_alive(_, _Conn) -> {"keep alive test", timeout, 15, fun() ->
	
	?PASSED
end}.
