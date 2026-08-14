-module(mqtt_rest_logic_handler).

-include_lib("kernel/include/logger.hrl").
-include_lib("mqtt_common/include/mqtt.hrl").

-type accept_callback_return() ::
        stop
        | boolean()
        | {true, iodata()}
        | {created, iodata()}
        | {see_other, iodata()}.
-type provide_callback_return() ::
        stop
        | cowboy_req:resp_body().
-type api_key_callback() ::
    fun((mqtt_rest_api:operation_id(), binary()) -> {true, context()} | {false, iodata()}).
-type accept_callback() ::
    fun((mqtt_rest_api:class(), mqtt_rest_api:operation_id(), cowboy_req:req(), context()) ->
            {accept_callback_return(), cowboy_req:req(), context()}).
-type provide_callback() ::
    fun((mqtt_rest_api:class(), mqtt_rest_api:operation_id(), cowboy_req:req(), context()) ->
            {cowboy_req:resp_body(), cowboy_req:req(), context()}).
-type context() :: #{_ := _}.

-export_type([context/0, api_key_callback/0,
              accept_callback_return/0, provide_callback_return/0,
              accept_callback/0, provide_callback/0]).

-optional_callbacks([api_key_callback/2]).

-callback api_key_callback(mqtt_rest_api:operation_id(), binary()) ->
    {true, context()} | {false, iodata()}.

-callback accept_callback(mqtt_rest_api:class(), mqtt_rest_api:operation_id(), cowboy_req:req(), context()) ->
    {accept_callback_return(), cowboy_req:req(), context()}.

-callback provide_callback(mqtt_rest_api:class(), mqtt_rest_api:operation_id(), cowboy_req:req(), context()) ->
    {provide_callback_return(), cowboy_req:req(), context()}.

-export([api_key_callback/2, accept_callback/4, provide_callback/4, resource_exist/3]).
-ignore_xref([api_key_callback/2, accept_callback/4, provide_callback/4]).

-record(state,
        {operation_id,
         accept_callback :: accept_callback(),
         provide_callback :: provide_callback(),
         api_key_callback :: api_key_callback(),
         context = #{} :: context()}).

-spec api_key_callback(mqtt_rest_api:operation_id(), binary()) -> {true, #{}}.
api_key_callback(OperationID, ApiKey) ->
    lager:info([{endtype, server}], "api_key_callback Operation: ~p ApiKey: ~p~n", [OperationID, ApiKey]),
    case ApiKey of
        <<"mqtt">> -> {true, #{}};
        _ -> {false, #{}}
    end.

resource_exist(OperationID, Req0, #state{context = Context0} = State) when 
		OperationID == 'loginUser'; 
		OperationID == 'getUserInfo';
		OperationID == 'deleteUser';
		OperationID == 'getStatus' ->
	ValidatorState = mqtt_rest_api:prepare_validator(),
	case mqtt_rest_api:populate_request(OperationID, Req0, ValidatorState) of
		{ok, Model, Req1} ->
			Context1 = maps:merge(Context0, Model),
			case find_user(Context1) of
				undefined ->
					Context2 = Context1#{user_record => undefined},
					Binary_resp = json:encode(#{code => 404, message => <<"User does not found">>}),
					Req2 = cowboy_req:set_resp_body(Binary_resp, Req1),
					{false, Req2, State#state{context = Context2}};
				#{} = User_rec ->
					Context2 = Context1#{user_record => User_rec},
					{true, Req1, State#state{context = Context2}}
			end;
		{error, _Reason, Req1} ->
			lager:debug([{endtype, server}], "Error: ~p~n", [_Reason]),
			Binary_resp = json:encode(#{code => 400, message => <<"Invalid request">>}),
			Req2 = cowboy_req:set_resp_body(Binary_resp, Req1),
			{false, Req2, Context0}
	end;
resource_exist(_OperationID, Req, State) ->
	{true, Req, State}.

find_user(#{storage := Storage, user_name := User}) ->
	case Storage:user(get, User) of
		undefined ->
			lager:debug([{endtype, server}], "USER DOES NOT EXIST: ~p~n", [User]),
			undefined;
		User_record ->
			lager:debug([{endtype, server}], "USER EXISTS User: ~p User record: ~p~n", [User, User_record]),
			User_record
	end.

-spec accept_callback(mqtt_rest_api:class(), mqtt_rest_api:operation_id(), cowboy_req:req(), context()) ->
	{accept_callback_return(), cowboy_req:req(), context()}.
%-type accept_callback_return() ::
%        stop
%        | boolean()
%        | {true, iodata()}
%        | {created, iodata()}
%        | {see_other, iodata()}.
accept_callback(_Class, 'loginUser' = _OperationID, Req0, Context0) ->
	#{'User' := #{<<"password">> := Password}, user_record := User_record} = Context0,
	Response_map =
		case User_record of
			undefined ->
				#{success => false, roles => [<<"NO USER">>]};
			#{password := Password_From_DB, roles := Roles} ->
				Enc_Password = list_to_binary(binary_to_hex(crypto:hash(md5, Password))),
				lager:info("Passwords: ~p/~p~n", [Enc_Password, Password_From_DB]),
				if Enc_Password =:= Password_From_DB ->
					lager:info("Login success~n", []),
					#{success => true, roles => Roles};
				true ->
					lager:info("Login failed~n", []),
					#{success => false, roles => []}
				end
		end,
	Req1 = cowboy_req:set_resp_body(json:encode(Response_map), Req0),
	{true, Req1, Context0};
accept_callback(_Class, 'createNewUser' = OperationID, Req0, Context0) ->
	ValidatorState = mqtt_rest_api:prepare_validator(),
	case mqtt_rest_api:populate_request(OperationID, Req0, ValidatorState) of
		{ok, Model, Req1} ->
			Context1 = maps:merge(Context0, Model),
			#{storage := Storage, user_name := User, 'User' := #{<<"password">> := Password, <<"roles">> := Roles}} = Context1,
			{R, Response_map} =
			case find_user(Context1) of
				undefined ->
					case Storage:user(save, #user{user_id = User, password = Password, roles = Roles}) of
						false ->
							lager:info([{endtype, server}], "Cannot create new user, context: ~p~n", [Context1]),
							{false, #{code => 400, message => <<"Cannot create new user.">>}};
						true ->
							lager:info([{endtype, server}], "new user created, context: ~p~n", [Context1]),
							{{created, <<"">>}, #{}}
					end;
				#{} ->
					lager:info([{endtype, server}], "The user already exists: ~p~n", [Context1]),
					{false, #{code => 400, message => <<"Already exists.">>}}
			end,
			Binary_resp = json:encode(Response_map),
			Req2 = cowboy_req:set_resp_body(Binary_resp, Req1),
			{R, Req2, Context1};
		{error, _Reason, Req1} ->
			lager:error([{endtype, server}], "Invalid request:: reason: ~p~n ~p~n", [_Reason, Req1]),
			Binary_resp = json:encode(#{code => 400, message => <<"Invalid request">>}),
			Req2 = cowboy_req:set_resp_body(Binary_resp, Req1),
			{false, Req2, Context0}
	end;
accept_callback(_Class, 'deleteUser' = _OperationID, Req0, Context0) ->
	#{storage := Storage, user_name := User} = Context0,
	{R, Response_map} =
		case Storage:user(remove, User) of
			false ->
				{false, #{code => <<"204">>, message => <<"user already deleted">>}};
			true ->
				{true, #{}}
		end,
	Req1 = cowboy_req:set_resp_body(json:encode(Response_map), Req0),
	{R, Req1, Context0};
accept_callback(Class, OperationID, Req, Context) ->
    lager:info([{endtype, server}], "accept_callback::~n  class: ~p~n  OperationId: ~p~n  Request: ~p~n  Context: ~p~n",
     [Class, OperationID, Req, Context]),
    {true, Req, Context}.

-spec provide_callback(mqtt_rest_api:class(), mqtt_rest_api:operation_id(), cowboy_req:req(), context()) ->
    {cowboy_req:resp_body(), cowboy_req:req(), context()}.
provide_callback(_Class, 'getUserInfo' = _OperationID, Req0, Context0) ->
	#{user_record := User_record} = Context0,
	Binary_resp = json:encode(User_record),
	{Binary_resp, Req0, Context0};
provide_callback(_Class, 'getStatus' = _OperationID, Req0, Context0) ->
	#{storage := Storage, user_name := User} = Context0,
	Status =
		case Storage:connect_pid(get, User, server) of
			P when is_pid(P) ->
				<<"on">>;
			_ ->
				<<"off">>
		end,
	Binary_resp = json:encode(#{id => User, status => Status}),
	{Binary_resp, Req0, Context0};
provide_callback(_Class, 'getAllStatuses' = OperationID, Req0, Context0) ->
	ValidatorState = mqtt_rest_api:prepare_validator(),
	case mqtt_rest_api:populate_request(OperationID, Req0, ValidatorState) of
		{ok, Model, Req1} ->
			Context1 = maps:merge(Context0, Model),
			#{storage := Storage, users := Users} = Context1,
			GetStatus = fun(U) -> 
				case Storage:user(get, U) of
					undefined -> <<"notFound">>;
					_ ->
						case Storage:connect_pid(get, U, server) of
							P when is_pid(P) -> <<"on">>;
							_ -> <<"off">>
						end
				end
			end,
			L = [#{id => U, status => GetStatus(U)} || U <- string:split(Users, ",", all)],
			{json:encode(L), Req1, Context1};
		{error, _Reason, Req1} ->
			Binary_resp = json:encode(#{code => 400, message => <<"Invalid request">>}),
			{Binary_resp, Req1, Context0}
	end;
provide_callback(_Class, 'getUserList' = OperationID, Req0, Context0) ->
	ValidatorState = mqtt_rest_api:prepare_validator(),
	case mqtt_rest_api:populate_request(OperationID, Req0, ValidatorState) of
		{ok, Model, Req1} ->
			Context1 = maps:merge(Context0, Model),
			#{storage := Storage, indexes := Indexes} = Context1,
			lager:debug([{endtype, server}], "Indexes: ~p~n", [Indexes]),
			List_indexes = string:split(Indexes, ",", all),
			Inds = [ binary_to_integer(I) || I <- List_indexes],
			lager:debug([{endtype, server}], "Inds: ~p~n", [Inds]),
			Users = Storage:user(get_all, server),
			UserStatus = fun(#user{user_id = User_name, password = _Pswd, roles = Roles} = U) -> 
				S = case Storage:connect_pid(get, U, server) of
					P when is_pid(P) -> <<"on">>;
					_ -> <<"off">>
				end,
				#{user_name => User_name, roles => Roles, status => S}
			end,
			L = [UserStatus(U) || U <- Users],
			{json:encode(L), Req1, Context1};
		{error, _Reason, Req1} ->
			Binary_resp = json:encode(#{code => 400, message => <<"Invalid request">>}),
			{Binary_resp, Req1, Context0}
	end;
provide_callback(_Class, 'getConfig' = OperationID, Req0, Context0) ->
	ValidatorState = mqtt_rest_api:prepare_validator(),
	case mqtt_rest_api:populate_request(OperationID, Req0, ValidatorState) of
		{ok, Model, Req1} ->
			Context1 = maps:merge(Context0, Model),
			#{app := Apps} = Context1,
			App_list = string:split(Apps, ",", all),
			lager:debug([{endtype, server}], "Apps: ~p~nApp_list: ~p~n", [Apps, App_list]),

			Resp_map = [
				begin
					Env = application:get_all_env(binary_to_atom(App)),
					lager:debug([{endtype, server}], "App: ~p~nEnv: ~p~n", [App, Env]),
					Fun = fun(_K, V) when is_list(V) -> list_to_binary(V);
						(_K, V) -> V
						end,
					Env_map = maps:map(Fun, maps:from_list(Env)),
					lager:debug([{endtype, server}], "Env_map: ~p~n", [Env_map]),
					#{app => App, config => Env_map}
				end || App <- App_list],
			lager:debug([{endtype, server}], "Resp_map: ~p~n", [Resp_map]),
			Binary_resp = json:encode(Resp_map),
			
			{Binary_resp, Req1, Context1};
		{error, _Reason, Req1} ->
			Binary_resp = json:encode(#{code => 400, message => <<"Invalid request">>}),
			{Binary_resp, Req1, Context0}
	end;
provide_callback(Class, OperationID, Req, Context) ->
	lager:error([{endtype, server}], "provide_callback::~n  class: ~p~n  OperationId: ~p~n  Request: ~p~n  Context: ~p~n",
		[Class, OperationID, Req, Context]),
		{<<"{}">>, Req, Context}.

binary_to_hex(Binary) -> [conv(N) || <<N:4>> <= Binary].

conv(N) when N < 10 -> N + 48; 
conv(N) -> N + 87. 
