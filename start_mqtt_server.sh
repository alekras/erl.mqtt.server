#!/bin/sh

/opt/local/bin/rebar3 do version,compile

erl \
 -pa _build/default/lib/*/ebin \
 -boot start_sasl \
 -config mqtt \
 -sname mqtt-server@localhost \
 -s ssl \
 -eval "application:ensure_all_started(mqtt_server)" \
 -setcookie 'mqtt'

# -detached \
# -s application start mqtt_server \
