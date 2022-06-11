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
-module(mqtt_rest_default_logic_handler).

-behaviour(mqtt_rest_logic_handler).

-export([handle_request/3]).
-export([authorize_api_key/2]).

-include_lib("mqtt_common/include/mqtt.hrl").

-spec authorize_api_key(OperationID :: mqtt_rest_api:operation_id(), ApiKey :: binary()) -> {true, #{}}.
authorize_api_key(_, _) -> {true, #{}}.

-spec handle_request(
	OperationID :: mqtt_rest_api:operation_id(),
	Req :: cowboy_req:req(),
	Context :: #{}
) ->
	{Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: jsx:json_term()}.
handle_request('CreateNewUser', 
							 Req, 
							 Context = #{storage := Storage,
													 user_name := User,
													 'User' := UserData}) ->
	lager:debug([{endtype, server}], "OperationID: 'CreateNewUser';~nrequest:~p;~ncontext:~p.~n", [Req, Context]),
%%	#{<<"password">> := Password, <<"roles">> := Roles} = UserData
	Password = maps:get(<<"password">>, UserData, <<"">>),
	Roles = maps:get(<<"roles">>, UserData, []),
	case Storage:save(server, #user{user_id = User, password = Password, roles = Roles}) of
		false ->
			lager:info([{endtype, server}], "Cannot create new user, context: ~p~n", [Context]),
			{400, #{}, #{code => <<"400">>, message => <<"Cannot create new user.">>}};
		true ->
			lager:info([{endtype, server}], "new user created, context: ~p~n", [Context]),
			{201, #{}, #{}}
	end;
handle_request('GetUserInfo', 
							 Req, 
							 Context = #{storage := Storage,user_name := User}) ->
	lager:debug([{endtype, server}], "OperationID: 'GetUserInfo';~nrequest:~p;~ncontext:~p.~n", [Req, Context]),
	case Storage:get(server, {user_id, User}) of
		undefined ->
			lager:info([{endtype, server}], "USER DOES NOT EXIST context: ~p~n", [Context]),
			{404, #{}, #{code => <<"404">>, message => <<"User does not found.">>}};
		#{password := Password, roles := Roles} ->
			lager:info([{endtype, server}], "USER EXISTS context: ~p~n", [Context]),
			{200, #{}, #{password => Password, roles => Roles}}
	end;
handle_request('DeleteUser', 
							 Req, 
							 Context = #{storage := Storage,user_name := User}) ->
	lager:debug([{endtype, server}], "OperationID: 'DeleteUser';~nrequest:~p;~ncontext:~p.~n", [Req, Context]),
	case Storage:get(server, {user_id, User}) of
		undefined ->
			lager:info([{endtype, server}], "USER DOES NOT EXIST context: ~p~n", [Context]),
			{404, #{}, #{code => <<"404">>, message => <<"User does not found.">>}};
		_ ->
			lager:info([{endtype, server}], "USER EXISTS context: ~p~n", [Context]),
			case Storage:remove(server, {user_id, User}) of
				false ->
					{201, #{}, #{code => <<"201">>, message => <<"user already deleted">>}};
				true -> {200, #{}, #{}}
			end
	end;
handle_request('GetStatus', 
							 Req, 
							 Context = #{storage := Storage,user_name := User}) ->
	lager:debug([{endtype, server}], "OperationID: 'GetStatus';~nrequest:~p;~ncontext:~p.~n", [Req, Context]),
	case Storage:get(server, {user_id, User}) of
		undefined ->
			lager:info([{endtype, server}], "USER DOES NOT EXIST context: ~p~n", [Context]),
			{404, #{}, #{code => <<"404">>, message => <<"User does not found.">>}};
		_ ->
			case Storage:get(server, {client_id, User}) of
				P when is_pid(P) ->
					Status = <<"on">>;
				_ ->
					Status = <<"off">>
			end,
			{200, #{}, #{id => User, status => Status}}
	end;
handle_request('GetAllStatuses', 
							 Req, 
							 Context = #{storage := Storage,users := Users}) ->
	lager:debug([{endtype, server}], "OperationID: 'GetAllStatuses';~nrequest:~p;~ncontext:~p.~nUsers:~p.~n", [Req, Context, Users]),
	GetStatus = fun(U) -> 
		case Storage:get(server, {user_id, U}) of
			undefined -> <<"notFound">>;
			_ ->
				case Storage:get(server, {client_id, U}) of
					P when is_pid(P) -> <<"on">>;
					_ -> <<"off">>
				end
		end
	end,
	L = [#{id => U, status => GetStatus(U)} || U <- string:split(Users, ",", all)],
	{200, #{}, L};
handle_request(OperationID, Req, Context) ->
	error_logger:error_msg(
		"Got not implemented request to process: ~p~n",
		[{OperationID, Req, Context}]
	),
	{501, #{}, #{}}.
