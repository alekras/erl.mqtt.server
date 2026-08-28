export MNESIA_DIR=/opt/mqtt/server/mnesia
export PORT_CLEAR=18883
export PORT_REST=8080
export PORT_WS=8880

docker run -it \
 -p "$PORT_CLEAR":"$PORT_CLEAR"/tcp \
 -p "$PORT_REST":"$PORT_REST"/tcp \
 -p "$PORT_WS":"$PORT_WS"/tcp \
 --name docker_container \
 --hostname localhost \
 --net mqtt_net \
 --rm \
 -e PORT_REST="$PORT_REST" \
 -e PORT_CLEAR="$PORT_CLEAR" \
 -e PORT_TLS=18483 \
 -e PORT_WS="$PORT_WS" \
 -e PORT_WSS=4443 \
 -e NODE_NAME=mqtt_server \
 -e CLUSTER_NODES="" \
 -e MNESIA_MASTER=true \
 -e MNESIA_DIR="'$MNESIA_DIR'" \
 --mount type=bind,src="/opt/mqtt/server/mnesia",dst="$MNESIA_DIR" \
 mqtt_server_dev
