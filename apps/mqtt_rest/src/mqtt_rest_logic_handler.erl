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
-module(mqtt_rest_logic_handler).

-export([handle_request/4]).
-export([authorize_api_key/3]).
-type context() :: #{binary() => any()}.
-type handler_response() ::{
	Status :: cowboy:http_status(),
	Headers :: cowboy:http_headers(),
	Body :: jsx:json_term()}.

-export_type([handler_response/0]).

-callback authorize_api_key(
	OperationID :: mqtt_rest_api:operation_id(),
	ApiKey :: binary()
) ->
	Result :: boolean() | {boolean(), context()}.


-callback handle_request(OperationID :: mqtt_rest_api:operation_id(), cowboy_req:req(), Context :: context()) ->
	handler_response().

-spec handle_request(
	Handler :: atom(),
	OperationID :: mqtt_rest_api:operation_id(),
	Request :: cowboy_req:req(),
	Context :: context()
) ->
	handler_response().
handle_request(Handler, OperationID, Req, Context) ->
	Handler:handle_request(OperationID, Req, Context).

-spec authorize_api_key(Handler :: atom(), OperationID :: mqtt_rest_api:operation_id(), ApiKey :: binary()) ->
	Result :: false | {true, context()}.
authorize_api_key(Handler, OperationID, ApiKey) ->
	Handler:authorize_api_key(OperationID, ApiKey).
