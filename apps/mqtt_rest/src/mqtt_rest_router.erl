-module(mqtt_rest_router).

-export([get_paths/1, get_validator_state/0]).

-type operations() :: #{
	Method :: binary() => mqtt_rest_api:operation_id()
}.

-type init_opts()  :: {
	Operations :: operations(),
	LogicHandler :: atom(),
	ValidatorMod :: module()
}.

-export_type([init_opts/0]).

-spec get_paths(LogicHandler :: atom()) ->  [{'_',[{
	Path :: string(),
	Handler :: atom(),
	InitOpts :: init_opts()
}]}].
get_paths(LogicHandler) ->
	ValidatorState = prepare_validator(),
	PreparedPaths = maps:fold(
		fun(Path, #{operations := Operations, handler := Handler}, Acc) ->
			[{Path, Handler, Operations} | Acc]
		end,
		[],
		group_paths()
	),
	[
		{'_',
			[{P, H, {O, LogicHandler, ValidatorState}} || {P, H, O} <- PreparedPaths]
		}
	].

group_paths() ->
	maps:fold(
		fun(OperationID, #{path := Path, method := Method, handler := Handler}, Acc) ->
			case maps:find(Path, Acc) of
				{ok, PathInfo0 = #{operations := Operations0}} ->
					Operations = Operations0#{Method => OperationID},
					PathInfo = PathInfo0#{operations => Operations},
					Acc#{Path => PathInfo};
				error ->
					Operations = #{Method => OperationID},
					PathInfo = #{handler => Handler, operations => Operations},
					Acc#{Path => PathInfo}
			end
		end,
		#{},
		get_operations()
	).

get_operations() ->
	#{ 
		'CreateNewUser' => #{
			path => "/rest/user/:user_name",
			method => <<"POST">>,
			handler => 'mqtt_rest_user_handler'
		},
		'DeleteUser' => #{
			path => "/rest/user/:user_name",
			method => <<"DELETE">>,
			handler => 'mqtt_rest_user_handler'
		},
		'GetAllStatuses' => #{
			path => "/rest/user/status",
			method => <<"GET">>,
			handler => 'mqtt_rest_user_handler'
		},
		'GetStatus' => #{
			path => "/rest/user/:user_name/status",
			method => <<"GET">>,
			handler => 'mqtt_rest_user_handler'
		},
		'GetUserInfo' => #{
			path => "/rest/user/:user_name",
			method => <<"GET">>,
			handler => 'mqtt_rest_user_handler'
		}
	}.

get_validator_state() ->
	persistent_term:get({?MODULE, validator_state}).

prepare_validator() ->
	R = jsx:decode(element(2, file:read_file(get_openapi_path()))),
	JesseState = jesse_state:new(R, [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>}]),
	persistent_term:put({?MODULE, validator_state}, JesseState),
	?MODULE.

get_openapi_path() ->
	{ok, AppName} = application:get_application(?MODULE),
	filename:join(mqtt_rest_utils:priv_dir(AppName), "openapi.json").
