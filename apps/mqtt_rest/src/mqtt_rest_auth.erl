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
-module(mqtt_rest_auth).

-export([authorize_api_key/5]).

-spec authorize_api_key(
	LogicHandler :: atom(),
	OperationID :: mqtt_rest_api:operation_id(),
	From :: header | qs_val,
	KeyParam :: iodata() | atom(),
	Req ::cowboy_req:req()
)-> {true, Context :: #{binary() => any()}, Req ::cowboy_req:req()} |
	{false, AuthHeader :: binary(), Req ::cowboy_req:req()}.
authorize_api_key(LogicHandler, OperationID, From, KeyParam, Req0) ->
	{ApiKey, Req} = get_api_key(From, KeyParam, Req0),
	case ApiKey of
		undefined ->
			AuthHeader = <<"">>,
			{false, AuthHeader, Req};
		_ ->
			Result = mqtt_rest_logic_handler:authorize_api_key(
				LogicHandler,
				OperationID,
				ApiKey
			),
			case Result of
				{true, Context}  ->
					{true, Context, Req};
				false ->
					AuthHeader = <<"">>,
					{false, AuthHeader, Req}
			end
	end.

get_api_key(header, KeyParam, Req) ->
	Headers = cowboy_req:headers(Req),
	{
		maps:get(
			mqtt_rest_utils:to_header(KeyParam),
			Headers,
			undefined
		),
		Req
	};
get_api_key(qs_val, KeyParam, Req) ->
	QS = cowboy_req:parse_qs(Req),
	{mqtt_rest_utils:get_opt(KeyParam, QS), Req}.
