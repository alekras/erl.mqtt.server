%% @author alexei
%% @doc @todo Add description to mqtt_rest_handler.


-module(mqtt_rest_handler).

-include_lib("mqtt_common/include/mqtt.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	init/2,
	allowed_methods/2,
	known_methods/2,
	is_authorized/2,
	malformed_request/2,
	content_types_provided/2,
	content_types_accepted/2,
	resource_exists/2,
	delete_resource/2,

	get_user/2,
	add_user/2
]).

init(Req0, State) ->
	lager:info([{endtype, server}], "web starts req: ~p~n state: ~p~n", [Req0, State]),	
	OrigHost = cowboy_req:header(<<"x-forwarded-for">>, Req0),
	lager:info([{endtype, server}], "host: ~p~n", [OrigHost]),

	Storage =
	case application:get_env(mqtt_server, storage, dets) of
		mysql -> mqtt_mysql_dao;
		dets -> mqtt_dets_dao
	end,

	{cowboy_rest, Req0, #{storage => Storage}}.

%% ====================================================================
%% RESTfull callbacks
%% ====================================================================
allowed_methods(Req, State) -> {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

known_methods(Req, State) -> {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

is_authorized(Req, State) -> {true, Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, get_user}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, []}, add_user}],
		Req, State}.

malformed_request(Req, State) ->
	User = binary_to_list(cowboy_req:binding(user_name, Req)),
	Password = cowboy_req:binding(password, Req),
	New_State0 =
	case cowboy_req:method(Req) of
		<<"POST">> -> maps:put(password, Password, State);
		_ -> State
	end,
	New_State1 = maps:put(user, User, New_State0),
	case User of
		undefined ->
			{true, Req, New_State1};
		User ->
			lager:info([{endtype, server}], "well formed request, state: ~p~n", [New_State1]),
			{false, Req, New_State1}
	end.

resource_exists(Req, #{storage := Storage, user := User} = State) ->
	case Storage:get(server, {user_id, User}) of
		undefined ->
			lager:info([{endtype, server}], "USER DOES NOT EXIST state: ~p~n", [State]),
			{false, Req, maps:put(exist, false, State)};
		Password ->
			lager:info([{endtype, server}], "USER EXISTS state: ~p~n", [State]),
			{true, Req, State#{exist => true, db_password => Password}}
	end.

delete_resource(Req, #{storage := Storage, user := User} = State) -> 
	lager:info([{endtype, server}], "DELETE req: ~p~n state: ~p~n", [Req, State]),
	{Storage:remove(server, {user_id, User}), Req, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================

get_user(Req, #{exist := true, db_password := Password} = State) -> 
	lager:info([{endtype, server}], "GET USER req: ~p~n state: ~p~n", [Req, State]),
	{<<Password/binary, "\n">>, Req, State}.

add_user(Req, #{storage := Storage, user := User, password := Password} = State) -> 
	lager:info([{endtype, server}], "ADD USER req: ~p~n state: ~p~n", [Req, State]),
	{Storage:save(server, #user{user_id = User, password = Password}), Req, State}.
