#!/bin/sh

C:\Users\axk456\Applications\erlang\bin\werl ^
-pa ^
 _build\default\lib\cowboy\ebin ^
 _build\default\lib\cowlib\ebin ^
 _build\default\lib\goldrush\ebin ^
 _build\default\lib\lager\ebin ^
 _build\default\lib\mysql_client\ebin ^
 _build\default\lib\ranch\ebin ^
 _build\default\lib\rsrc_pool\ebin ^
 _build\default\lib\mqtt_common\ebin ^
 _build\default\lib\mqtt_server\ebin ^
-boot start_sasl ^
-config mqtt ^
-sname mqtt-server@localhost ^
-s ssl ^
-eval "application:start(mqtt_server)" ^
-setcookie 'mqtt'

