export MNESIA_DIR=/opt/mqtt/server/mnesia
docker run -it \
 -p 18883:18883/tcp \
 --name docker_container \
 --hostname docker_container \
 --net mqtt_net \
 --rm \
 -e PORT_REST=8080 \
 -e PORT_CLEAR=18883 \
 -e PORT_TLS=18483 \
 -e PORT_WS=8880 \
 -e PORT_WSS=4443 \
 -e NODE_NAME=mqtt_server \
 -e CLUSTER_NODES="" \
 -e MNESIA_MASTER=true \
 -e MNESIA_DIR="'$MNESIA_DIR'" \
 --mount type=bind,src="$HOME"/MyData/mqtt/server/mnesia,dst="$MNESIA_DIR" \
 mqtt_server_dev
