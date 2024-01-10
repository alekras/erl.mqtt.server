%%
%% Copyright (C) 2017-2023 by krasnop@bellsouth.net (Alexei Krasnopolski)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%		 http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License. 
%%

%% @since 2017-01-11
%% @copyright 2017-2023 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc Root supervisor of mqtt_server application.

-module(mqtt_server_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).

-spec start_link(Children :: list()) -> {ok, pid()}.
start_link(Children) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Children).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Shutdown, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Shutdown :: brutal_kill | timeout(),
	Modules :: [module()] | dynamic.
%% ====================================================================
init(Children) ->
	lager:debug("Children amount: ~p~n",[length(Children)]),
	Pred = fun({_,{M,_,_},_,_,_,_}) ->
		case whereis(M) of
			undefined -> true;
			Pid -> not is_process_alive(Pid)
		end
	end,
	FilteredChildren = lists:filter(Pred, Children),
	lager:debug("Children amount: ~p~n",[length(FilteredChildren)]),
	{ok, {{one_for_one, 10, 10}, FilteredChildren}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
