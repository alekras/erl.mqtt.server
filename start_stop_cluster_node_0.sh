#!/bin/sh

export PATH="$PATH:/usr/bin:/usr/local/bin:/usr/local/Cellar/erlang/28.0.2_1/bin"
export RELX_REPLACE_OS_VARS=true
export NODE_NAME=mqtt_server_0
export PORT_REST=8080 \
export PORT_CLEAR=18883 \
export PORT_TLS=18483 \
export PORT_WS=8880 \
export PORT_WSS=4443 \
export CLUSTER_NODES="'mqtt_server_0@MacBook-Pro','mqtt_server_1@MacBook-Pro'" \
export MNESIA_MASTER=false \
export MNESIA_DIR="'/opt/mqtt/cluster/node_0/mnesia'" 
echo "Script to start/stop node $NODE_NAME of cluster"
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
		$SCRIPT_NAME daemon
		sleep 2
		$SCRIPT_NAME pid
		;;
	stop)
		$SCRIPT_NAME stop
		$SCRIPT_NAME status
		;;
	console)
		$SCRIPT_NAME console
		;;
	*)
		echo "Usage: $0 [dev|prod] [start|stop|console]"
		;;
esac

exit 0
