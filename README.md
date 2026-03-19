# MQTT server for Erlang.
## Introduction
The server implements MQTT messaging protocol version 3.1, 3.1.1, 5.0 and allows communicate with MQTT clients to send/receive messages.
The server is written in Erlang. 
The server was tested with following clients:

1. Mosquitto command line tools [subscriber](https://mosquitto.org/man/mosquitto_sub-1.html) and [publisher](https://mosquitto.org/man/mosquitto_pub-1.html)
2. Javascript web-socket MQTT client [HiveMQ](http://www.hivemq.com/demos/websocket-client/)
3. [Erlang MQTT client](https://github.com/alekras/mqtt_client/)

## Messenger implementation based on the MQTT server.
Simple Instant messenger [SIM](https://github.com/alekras/erl.web.sim) was created to test functionality of the MQTT server and prove good performance of the code.
Live demo of SIM is running [here](https://lucky3p.com/sim).

## Architecture
The server consist of two OTP applications: core MQTT server and restful HTTP server for managing users DB. The both apps are combined in one release and are working closely.

### Core MQTT server
Core MQTT server is an OTP application that implements MQTT protocol versions 3.1, 3.1.1 and 5.0. It depends on other Erlang applications: 

1. ```lager``` for logging service,
2. ```cowboy``` for tcp, tls and web-socket (ws and wss) connections,
3. ```msql_client``` for connection to MySQL server [see](https://github.com/alekras/mysql_client),
4. ```mqtt_common``` that is library keeping [code](https://github.com/alekras/erl.mqtt.common) that is common for MQTT client and server implementation.

### Resful Http server
Http server implements Restful API described in OpenAPI configuration file 
[mqtt_rest_v3.yaml](https://github.com/alekras/erl.mqtt.server/blob/master/mqtt_rest_v3.yaml).
There is [swagger](https://lucky3p.com/rest/v3/swagger-ui) page of running instance of MQTT Rest Http server.

### Storage service: backend database implementation
Storage service is using to keep information about client's sessions, subscriptions of a clients, messages during client-server communication with different QoS, users and retain messages.

The MQTT server has a three options for storage service:
1. **DETS** implementation is using for standalone MQTT server and running in the same Erlang VM. It has maximum performance and minimum memory consuming.
2. **MySQL** implementation is using for standalone variant or for distributed solution but all MQTT servers from cluster are connected to only separated MySQL server.
3. **Mnesia** implementation is using for distributed MQTT servers in cluster. Mnesia DB is running on each node inside Erlang VM but connected to other nodes. This options is work fine with standalone configuration.

### Connection types
Server can establish connection using different network protocols:
1. clear it/tcp
2. tls/ssl
3. web socket
4. secure web socket

## Getting started
### Installation
To start with the server you have to complete two steps below:

1. Install [Erlang](http://www.erlang.org/download.html).
2. Install [Rebar3](https://www.rebar3.org/).

### Building and deployment server instance
#### Download or clone the project from SourceForge GIT repository or from GitHub

Download/clone source code to local host from GitHub. Type command

```bash
$ git clone https://github.com/alekras/erl.mqtt.server.git erl.mqtt.server
```
or download/cloan code from SourceForge

```bash
$ git clone https://git.code.sf.net/p/mqtt-server/code erl.mqtt.server
```
Now you have a few options how build/start server:
 - compile and start from bash terminal. It is simple way and convenient for debugging and testing;
 - create release and start it according to OTP rules. You can run server as single standalone mode or as cluster distributed nodes;
 - build docker image and start it in standalone or cluster mode.

#### Compiling and starting from bash terminal
After you have got source code of the server then change directory to the erl.mqtt.server:

```bash
$ cd erl.mqtt.server
```
Run rebar3 for this project. You have to add path to rebar3 to OS PATH variable or just use the whole path:

```bash
$ /opt/local/bin/rebar3 compile
```
Rebar will fetch code of all dependencies and compile source files of main project and all dependencies.

To start server run bash script:

```bash
$ cd erl.mqtt.server
$ ./deployment_scripts/start_mqtt_server.sh
```
Erlang shell will open and log statements are appearing in console.

#### Using relx
To make release of the application for development run command:

```bash
$ /opt/local/bin/rebar3 release -n mqtt_server_dev
```
for production:

```bash
$ /opt/local/bin/rebar3 release -n mqtt_server
```

Example of script to make release and start server is [here](https://github.com/alekras/erl.mqtt.server/blob/master/make_release_start_node.sh)

#### Build Docker image
First at all you need to download and install Docker or Docker Desktop. After that you can build docker image of MQTT server:

```bash
$ cd erl.mqtt.server
$ docker build -t mqtt_server_dev --file Dockerfile .
```

#### Build Docker compose cluster

 *TO DO*

### Deployment

#### Start standalone server from terminal

This kind of server run is useful for testing and debugging:

```bash
$ cd erl.mqtt.server
$ ./deployment_scripts/start_mqtt_server.sh
```
#### Start standalone release

When you build release ([see "Using relx"](#using-relx)), now execute commands to start server in dev mode:

```bash
$ cd erl.mqtt.server
$ ./deployment_scripts/start_stop_node.sh dev console
```

#### Start distributed release

Let start cluster of two nodes of MQTT servers. Distributed mode of the server can run only with Mnesia
storage service. When you start cluster first time Mnesia will initiate databases on different nodes and create tables. Clients can connect to any server nodes but messages will transport between clients connected to different nodes the same way as in standalone mode.

You need to do a few steps to deploy cluster:
 - Open first terminal window and run command

```bash
$ cd erl.mqtt.server
$ ./start_stop_cluster_node_0.sh dev console
```
 - Open second terminal window and run command

```bash
$ cd erl.mqtt.server
$ ./start_stop_cluster_node_1.sh dev console
```
Now you can connect a few MQTT clients to different ports (18883 or 28883) to test the cluster.

#### Start standalone docker container
 *TO DO*
#### Start distributed docker containers
 *TO DO*
#### Start docker-compose 
 *TO DO*
## Configuration file sys.config
To set up ports for TCP and TLS socket connection go to config[-dev]/sys.config. This is OTP application configuration file contained startup data for
lager, ranch and mqtt server backend type and connection details.

 *TO DO*

## Testing

You can test the server with any MQTT client complained with protocol version 3.1.1 or 5.0.
I recommend to try Erlang MQTT [client](https://sourceforge.net/projects/mqtt-client/) or [client](https://github.com/alekras/mqtt_client.git).

### Testing with Mosquitto tools
To test with Mosquitto tools you need to open two terminal windows. One for subscribing and other for publishing.
Open the first terminal windows and change directory to folder where Mosquitto is installed:

```bash
$ cd /usr/local/Cellar/mosquitto/1.4.10/
```
Now subscribe to "test/c" topic:

```bash
$ bin/mosquitto_sub -t test/c -p 18883 -i test -u guest -P guest -V mqttv5
```
Open the second terminal windows and change directory to Mosquitto installation:

```bash
$ cd /usr/local/Cellar/mosquitto/1.4.10/
```
Publish some message to "test/c" topic:

```bash
$ bin/mosquitto_pub -t test/c -p 18883 -i test1 -u guest -P guest -m "Test message from mosquitto tools QoS=2" -q 2 -V mqttv5
```
In first terminal you will see incoming message:

```bash
$ Test message from mosquitto tools QoS=2
```

### Testing with other MQTT clients
The server was tested with other clients:
1. Websocket MQTT client from HiveMQ [http://www.hivemq.com/demos/websocket-client/].</li>
2. MQTT Erlang client [https://github.com/alekras/mqtt_client.git].</li>

### Add/Remove users

Rest HTTP server allows to manage users table on backend DB. If you start server on local environment
you can reach swagger page as http://localhost:8080/rest/v3/swagger-ui.

## References

1. [https://mosquitto.org/] - Mosquitto MQTT server.
2. [https://www.rabbitmq.com/] - RabbitMQ server with MQTT plugin.
3. [https://sourceforge.net/projects/mqtt-client/] - Erlang MQTT client.
4. [http://www.hivemq.com/demos/websocket-client/] - MQTT websocket client.

