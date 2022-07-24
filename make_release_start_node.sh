#!/bin/sh

export PATH="$PATH:/usr/local/bin:/usr/local/Cellar/erlang/23.0/lib/erlang/bin"
echo "argument: $1"
REBAR3="/opt/local/bin/rebar3"
$REBAR3 do version
$REBAR3 do upgrade

case "$1" in
	dev)
		$REBAR3 release -n mqtt_server_dev
		cd _build/default/rel/mqtt_server_dev/bin
		./mqtt_server_dev start
		./mqtt_server_dev pid
		;;
	prod)
		$REBAR3 release -n mqtt_server
		cd _build/default/rel/mqtt_server/bin
		./mqtt_server foreground
		./mqtt_server pid
		;;
	*)
		echo "Usage: $0 [dev|prod]"
		;;
esac

exit 0
