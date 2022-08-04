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

### Backend database implementation
Session state data is storing in database (DETS or MySQL in current version). Developing of Mnesia support is in progress.

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

### Building
#### Download or clone from SourceForge GIT repository or from GitHub

Download source code to local host. Type command
```bash
$ git clone https://git.code.sf.net/p/mqtt-server/code erl.mqtt.server
```
or
```bash
$ git clone https://github.com/alekras/erl.mqtt.server.git erl.mqtt.server
```

#### Compiling
After you have got source code of the server then change directory to the erl.mqtt.server:
```bash
$ cd erl.mqtt.server
```
Run rebar3 for this project. You have to add path to rebar3 to OS PATH variable or just use the whole path:
```bash
$ /opt/local/bin/rebar3 compile
```
Rebar will fetch code of all dependencies and compile source files of main project and all dependencies.

#### Starting

To start server run bash script:
```bash
$ ./start_mqtt_server.sh
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

Go to folder:
```bash
$ cd _build/prod/rel/mqtt_server_dev
```
and run command to start server:
```bash
$ bin/mqtt_server_dev console
```
Example of script to make release and start server is [here](https://github.com/alekras/erl.mqtt.server/blob/master/make_release_start_node.sh)

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
2. MQTT.fx client [http://www.mqttfx.org/].</li>

## Configuration

To set up ports for TCP and TLS socket connection go to config[-dev]/sys.config. This is OTP application configuration file contained startup data for
lager, ranch and mqtt server backend type and connection details.

### Add/Remove users

Rest HTTP server allows to manage users table on backend DB. If you start server on local environment
you can reach swagger page as http://localhost:8080/rest/v3/swagger-ui.

## References

1. [https://mosquitto.org/] - Mosquitto MQTT server.
2. [https://www.rabbitmq.com/] - RabbitMQ server with MQTT plugin.
3. [https://sourceforge.net/projects/mqtt-client/] - Erlang MQTT client.
4. [http://www.hivemq.com/demos/websocket-client/] - MQTT websocket client.

