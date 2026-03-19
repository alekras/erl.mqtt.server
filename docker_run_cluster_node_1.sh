export MNESIA_DIR=/opt/mqtt/server/mnesia
docker run -it \
 -p 28883:18883 \
 --name docker_container_1 \
 --hostname docker_container_1 \
 --net mqtt_net \
 --rm \
 -e PORT_REST=8080 \
 -e PORT_CLEAR=18883 \
 -e PORT_TLS=18483 \
 -e PORT_WS=8880 \
 -e PORT_WSS=4443 \
 -e NODE_NAME=mqtt_server \
 -e CLUSTER_NODES="'mqtt_server@docker_container_0','mqtt_server@docker_container_1'" \
 -e MNESIA_MASTER=false \
 -e MNESIA_DIR="'$MNESIA_DIR'" \
 --mount type=bind,src="$HOME"/MyData/mqtt/cluster/node_1/mnesia,dst="$MNESIA_DIR" \
 mqtt_server_dev
