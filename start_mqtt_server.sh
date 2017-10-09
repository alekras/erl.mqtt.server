#!/bin/sh

erl \
 -pa _build/default/lib/*/ebin \
 -boot start_sasl \
 -config mqtt \
 -sname mqtt-server@localhost \
 -s ssl \
 -eval "application:start(mqtt_server)" \
 -setcookie 'mqtt'

# -detached \
# -s application start mqtt_server \
