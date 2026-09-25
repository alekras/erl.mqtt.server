#!/bin/sh

/opt/local/bin/rebar3 do version,compile

erl \
 -pa _build/default/lib/*/ebin \
 -boot start_sasl \
 -config mqtt \
 -sname mqtt-rest@localhost \
 -s ssl \
 -eval "application:ensure_all_started(mqtt_rest)" \
 -setcookie 'mqtt'

# -detached \
# -s application start mqtt_server \
