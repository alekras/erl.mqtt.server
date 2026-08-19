
-record(session, {
		id :: string() | binary(),
		created = 0 :: integer(),
		userId :: string() | binary(),
		roles :: list() %% ["ADMIN", "USER"]
	}
).

%%-define(URL, "http://192.168.1.75:8880").
%%-define(URL, "http://MacBook-Pro:8080"). % @todo move to config
%%-define(URL, "http://lucky3p.com:8880"). % @todo move to config
%%rd(session, {id,created,userId,password}).