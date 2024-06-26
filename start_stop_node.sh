#!/bin/sh

export PATH="$PATH:/usr/local/bin:/usr/local/opt/erlang@24/lib/erlang/bin"
echo "arguments: $1 $2"
REBAR3="/opt/local/bin/rebar3"
$REBAR3 do version


case "$1" in
	dev)
		cd _build/default/rel/mqtt_server_dev
		SCRIPT_NAME="./bin/mqtt_server_dev"
		;;
	prod)
		cd _build/default/rel/mqtt_server
		SCRIPT_NAME="./bin/mqtt_server"
		;;
	*)
		echo "Usage: $0 [dev|prod] [start|stop|console]"
		;;
esac

case "$2" in
	start)
		$SCRIPT_NAME start
		sleep 2
		$SCRIPT_NAME pid
		;;
	stop)
		$SCRIPT_NAME stop
		;;
	console)
		$SCRIPT_NAME console
		;;
	*)
		echo "Usage: $0 [dev|prod] [start|stop|console]"
		;;
esac

exit 0
