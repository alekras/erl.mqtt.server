-module(mqtt_rest_user_handler).
-moduledoc """
Exposes the following operation IDs:

- `POST` to `/user/:user_name`, OperationId: `createNewUser`:
Add a new user to the database.


- `DELETE` to `/user/:user_name`, OperationId: `deleteUser`:
Delete user in the database.


- `GET` to `/user/status`, OperationId: `getAllStatuses`:
Get user connection statuses.
Returns a list of user&#39;s statuses

- `GET` to `/user/:user_name/status`, OperationId: `getStatus`:
Get user connection status.
Returns a user connection status

- `GET` to `/user/:user_name`, OperationId: `getUserInfo`:
Get user&#39;s information.
Returns a user&#39;s password in md5 format and list of roles

- `GET` to `/user/list`, OperationId: `getUserList`:
Get list of users.
Returns a list of users roles and connection statuses

- `POST` to `/user/login/:user_name`, OperationId: `loginUser`:
User is trying to log in to server.


""".

-behaviour(cowboy_rest).

-include_lib("kernel/include/logger.hrl").

%% Cowboy REST callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_resource/2]).
-export([is_authorized/2]).
-export([valid_content_headers/2]).
-export([resource_exists/2, allow_missing_post/2]).
-export([handle_type_accepted/2, handle_type_provided/2]).

-ignore_xref([handle_type_accepted/2, handle_type_provided/2]).

-export_type([class/0, operation_id/0]).

-type class() :: 'user'.

-type operation_id() ::
    'createNewUser' %% Add a new user to the database
    | 'deleteUser' %% Delete user in the database
    | 'getAllStatuses' %% Get user connection statuses
    | 'getStatus' %% Get user connection status
    | 'getUserInfo' %% Get user&#39;s information
    | 'getUserList' %% Get list of users
    | 'loginUser'. %% User is trying to log in to server

-record(state,
        {operation_id :: operation_id(),
         accept_callback :: mqtt_rest_logic_handler:accept_callback(),
         provide_callback :: mqtt_rest_logic_handler:provide_callback(),
         api_key_callback :: mqtt_rest_logic_handler:api_key_callback(),
         context = #{} :: mqtt_rest_logic_handler:context()}).

-type state() :: #state{}.
-export_type([state/0]).

-spec init(cowboy_req:req(), mqtt_rest_router:init_opts()) ->
    {cowboy_rest, cowboy_req:req(), state()}.
init(Req, {Operations, Module}) ->
	Method = cowboy_req:method(Req),
	OperationID = maps:get(Method, Operations, undefined),
	Storage =
	case application:get_env(mqtt_common, storage, dets) of
		mysql -> mqtt_mysql_storage;
		dets -> mqtt_dets_storage;
		mnesia -> mqtt_mnesia_storage
	end,
    lager:info([{endtype, server}], "Attempt to process operation ~p~n", [#{what => "Attempt to process operation",
                method => Method,
                operation_id => OperationID}]),
    State = #state{operation_id = OperationID,
                   accept_callback = fun Module:accept_callback/4,
                   provide_callback = fun Module:provide_callback/4,
                   api_key_callback = fun Module:api_key_callback/2,
                   context = #{storage => Storage}},
    {cowboy_rest, Req, State}.

-spec allowed_methods(cowboy_req:req(), state()) ->
    {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, #state{operation_id = 'createNewUser'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'deleteUser'} = State) ->
    {[<<"DELETE">>], Req, State};
allowed_methods(Req, #state{operation_id = 'getAllStatuses'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'getStatus'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'getUserInfo'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'getUserList'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'loginUser'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, State) ->
    {[], Req, State}.

-spec is_authorized(cowboy_req:req(), state()) ->
    {true | {false, iodata()}, cowboy_req:req(), state()}.
is_authorized(Req0,
              #state{operation_id = 'createNewUser' = OperationID,
                     api_key_callback = Handler} = State) ->
    case mqtt_rest_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context0, Req} ->
            Context1 = maps:merge(Context0, State#state.context),
            {true, Req, State#state{context = Context1}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'deleteUser' = OperationID,
                     api_key_callback = Handler} = State) ->
    case mqtt_rest_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context0, Req} ->
            Context1 = maps:merge(Context0, State#state.context),
            {true, Req, State#state{context = Context1}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'getAllStatuses' = OperationID,
                     api_key_callback = Handler} = State) ->
    case mqtt_rest_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context0, Req} ->
            Context1 = maps:merge(Context0, State#state.context),
            {true, Req, State#state{context = Context1}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'getStatus' = OperationID,
                     api_key_callback = Handler} = State) ->
    case mqtt_rest_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context0, Req} ->
            Context1 = maps:merge(Context0, State#state.context),
            {true, Req, State#state{context = Context1}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'getUserInfo' = OperationID,
                     api_key_callback = Handler} = State) ->
    case mqtt_rest_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context0, Req} ->
            Context1 = maps:merge(Context0, State#state.context),
            {true, Req, State#state{context = Context1}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'getUserList' = OperationID,
                     api_key_callback = Handler} = State) ->
    case mqtt_rest_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context0, Req} ->
            Context1 = maps:merge(Context0, State#state.context),
            {true, Req, State#state{context = Context1}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'loginUser' = OperationID,
                     api_key_callback = Handler} = State) ->
    case mqtt_rest_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context0, Req} ->
            Context1 = maps:merge(Context0, State#state.context),
            {true, Req, State#state{context = Context1}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req, State) ->
    {true, Req, State}.

-spec allow_missing_post(Req, State) -> 
    {boolean(), Req, State}.
allow_missing_post(Req, State) ->
    {false, Req, State}.

-spec resource_exists(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State) ->
    mqtt_rest_logic_handler:resource_exist(State#state.operation_id, Req, State).

-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, #state{operation_id = 'createNewUser'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'deleteUser'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'getAllStatuses'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'getStatus'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'getUserInfo'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'getUserList'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'loginUser'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, State) ->
    {[], Req, State}.

-spec valid_content_headers(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
valid_content_headers(Req, #state{operation_id = 'createNewUser'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'deleteUser'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'getAllStatuses'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'getStatus'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'getUserInfo'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'getUserList'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'loginUser'} = State) ->
    {true, Req, State};
valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, #state{operation_id = 'createNewUser'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'deleteUser'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'getAllStatuses'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'getStatus'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'getUserInfo'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'getUserList'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'loginUser'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, State) ->
    {[], Req, State}.

-spec delete_resource(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
delete_resource(Req, State) ->
    {Res, Req1, State1} = handle_type_accepted(Req, State),
    {true =:= Res, Req1, State1}.

-spec handle_type_accepted(cowboy_req:req(), state()) ->
    { mqtt_rest_logic_handler:accept_callback_return(), cowboy_req:req(), state()}.
handle_type_accepted(Req, #state{operation_id = OperationID,
                                 accept_callback = Handler,
                                 context = Context} = State) ->
    {Res, Req1, Context1} = Handler(user, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.

-spec handle_type_provided(cowboy_req:req(), state()) ->
    { mqtt_rest_logic_handler:provide_callback_return(), cowboy_req:req(), state()}.
handle_type_provided(Req, #state{operation_id = OperationID,
                                 provide_callback = Handler,
                                 context = Context} = State) ->
    {Res, Req1, Context1} = Handler(user, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.
