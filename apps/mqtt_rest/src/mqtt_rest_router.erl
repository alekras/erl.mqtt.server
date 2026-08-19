-module(mqtt_rest_router).

-export([get_paths/1]).

-type method() :: binary().
-type operations() :: #{method() => mqtt_rest_api:operation_id()}.
-type init_opts()  :: {operations(), module()}.

-export_type([init_opts/0]).

-spec get_paths(LogicHandler :: module()) -> cowboy_router:routes().
get_paths(LogicHandler) ->
    PreparedPaths = maps:fold(
                      fun(Path, #{operations := Operations, handler := Handler}, Acc) ->
                              [{Path, Handler, Operations} | Acc]
                      end, [], group_paths()
                     ),
    [{'_', 
			lists:append(
				[
					{"/mqtt", cowboy_static, {priv_file, mqtt_rest, "www/index.html"}},
					{"/mqtt/js/[...]", cowboy_static, {priv_dir, mqtt_rest, "www/js", [{mimetypes, cow_mimetypes, all}]}},
					{"/mqtt/css/[...]", cowboy_static, {priv_dir, mqtt_rest, "www/css", [{mimetypes, cow_mimetypes, all}]}},
					{"/mqtt/img/[...]", cowboy_static, {priv_dir, mqtt_rest, "www/img", [{mimetypes, cow_mimetypes, all}]}},
					{"/mqtt/login", mqtt_web_handler_log, []},

					{"/rest/v3/swagger-ui", cowboy_static, {priv_file, mqtt_rest, "dist/index.html"}},
					{"/rest/v3/swagger-spec", cowboy_static, {priv_file, mqtt_rest, "openapi.json"}},
					{"/rest/v3/[...]", cowboy_static, {priv_dir, mqtt_rest, "dist", [{mimetypes, cow_mimetypes, all}]}}
				],
    
    [{P, H, {O, LogicHandler}} || {P, H, O} <- PreparedPaths]
    )
    }].

group_paths() ->
    maps:fold(
      fun(OperationID, #{servers := Servers, base_path := BasePath, path := Path,
                         method := Method, handler := Handler}, Acc) ->
              FullPaths = build_full_paths(Servers, BasePath, Path),
              merge_paths(FullPaths, OperationID, Method, Handler, Acc)
      end, #{}, get_operations()).

build_full_paths([], BasePath, Path) ->
    [lists:append([BasePath, Path])];
build_full_paths(Servers, _BasePath, Path) ->
    [lists:append([Server, Path]) || Server <- Servers ].

merge_paths(FullPaths, OperationID, Method, Handler, Acc) ->
    lists:foldl(
      fun(Path, Acc0) ->
              case maps:find(Path, Acc0) of
                  {ok, PathInfo0 = #{operations := Operations0}} ->
                      Operations = Operations0#{Method => OperationID},
                      PathInfo = PathInfo0#{operations => Operations},
                      Acc0#{Path => PathInfo};
                  error ->
                      Operations = #{Method => OperationID},
                      PathInfo = #{handler => Handler, operations => Operations},
                      Acc0#{Path => PathInfo}
              end
      end, Acc, FullPaths).

get_operations() ->
    #{ 
       'getConfig' => #{
            servers => [],
            base_path => "/rest",
            path => "/server/config",
            method => <<"GET">>,
            handler => 'mqtt_rest_server_handler'
        },
       'getSession' => #{
            servers => [],
            base_path => "/rest",
            path => "/server/checksession",
            method => <<"GET">>,
            handler => 'mqtt_rest_server_handler'
        },
       'createNewUser' => #{
            servers => [],
            base_path => "/rest",
            path => "/user/:user_name",
            method => <<"POST">>,
            handler => 'mqtt_rest_user_handler'
        },
       'deleteUser' => #{
            servers => [],
            base_path => "/rest",
            path => "/user/:user_name",
            method => <<"DELETE">>,
            handler => 'mqtt_rest_user_handler'
        },
       'getAllStatuses' => #{
            servers => [],
            base_path => "/rest",
            path => "/user/status",
            method => <<"GET">>,
            handler => 'mqtt_rest_user_handler'
        },
       'getStatus' => #{
            servers => [],
            base_path => "/rest",
            path => "/user/:user_name/status",
            method => <<"GET">>,
            handler => 'mqtt_rest_user_handler'
        },
       'getUserInfo' => #{
            servers => [],
            base_path => "/rest",
            path => "/user/:user_name",
            method => <<"GET">>,
            handler => 'mqtt_rest_user_handler'
        },
       'getUserList' => #{
            servers => [],
            base_path => "/rest",
            path => "/user/list",
            method => <<"GET">>,
            handler => 'mqtt_rest_user_handler'
        },
       'loginUser' => #{
            servers => [],
            base_path => "/rest",
            path => "/user/login/:user_name",
            method => <<"POST">>,
            handler => 'mqtt_rest_user_handler'
        },
       'updateUser' => #{
            servers => [],
            base_path => "/rest",
            path => "/user/:user_name",
            method => <<"PUT">>,
            handler => 'mqtt_rest_user_handler'
        }
    }.
