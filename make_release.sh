#!/bin/sh

export PATH="$PATH:/usr/bin:/usr/local/bin:/usr/local/Cellar/erlang/28.0.2_1/bin"
echo "argument: $1"
REBAR3="/opt/local/bin/rebar3"
$REBAR3 do version
$REBAR3 do clean --all
$REBAR3 do unlock --all
$REBAR3 do upgrade --all

case "$1" in
	dev)
		REL_NAME="mqtt_server_dev"
		;;
	prod)
		REL_NAME="mqtt_server"
		;;
	*)
		echo "Usage: $0 [dev|prod]"
		;;
esac

$REBAR3 release -n $REL_NAME

exit 0
