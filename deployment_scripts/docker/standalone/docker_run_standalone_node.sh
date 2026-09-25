export MNESIA_DIR=/opt/mqtt/server/mnesia
export PORT_CLEAR=18883
export PORT_REST=8080
export PORT_WS=8880

echo "arguments: $1 (dev | prod)"
case "$1" in
	dev)
		RELEASE_NAME="mqtt_server_dev"
		export CERT_FILE="tls_cnfg/server/cert.pem"
		export CA_CERT_FILE="tls_cnfg/server/cacerts.pem"
		export KEY_FILE="tls_cnfg/server/key.pem"
		;;
	prod)
		RELEASE_NAME="mqtt_server"
		export CERT_FILE="/home/alexei/.ssh/lucky3p.com/certificate.crt"
		export CA_CERT_FILE="/home/alexei/.ssh/lucky3p.com/ca_bundle.crt"
		export KEY_FILE="/home/alexei/.ssh/lucky3p.com/private.key"
		;;
	*)
		echo "Usage: $0 [dev|prod]"
		exit 1
		;;
esac

docker run -it \
 -p "$PORT_CLEAR":"$PORT_CLEAR"/tcp \
 -p "$PORT_REST":"$PORT_REST"/tcp \
 -p "$PORT_WS":"$PORT_WS"/tcp \
 --name docker_container_mqtt \
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
 -e MNESIA_DIR="$MNESIA_DIR" \
 -e CERT_FILE="$CERT_FILE" \
 -e CA_CERT_FILE="$CA_CERT_FILE" \
 -e KEY_FILE="$KEY_FILE" \
 --mount type=bind,src="/opt/mqtt/server/mnesia",dst="$MNESIA_DIR" \
 $RELEASE_NAME
