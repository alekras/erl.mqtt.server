# MQTT server for Erlang.
## Introduction
The server implements MQTT messaging protocol version 3.1, 3.1.1, 5.0 and allows communicate with MQTT clients to send/receive messages.
The server is written in Erlang. 
The server was tested with following clients

1. Mosquitto command line tools [subscriber](https://mosquitto.org/man/mosquitto_sub-1.html) and [publisher](https://mosquitto.org/man/mosquitto_pub-1.html)
2. [MQTT.fx client](http://www.mqttfx.org/)
3. [Erlang MQTT client](https://sourceforge.net/projects/mqtt-client/)

## Messenger implementation based on the MQTT server.
Simple Instant messenger [SIM](https://github.com/alekras/erl.web.sim) was created to test functionality of the MQQT server and prove good performance of the code.
Live demo of SIM is running [here](http://lucky3p.com/sim).

## Architecture
MQTT server is an OTP application. Application depends on other Erlang applications: 

1. ```lager``` for logging services,
2. ```ranch``` for tcp and tsl connections,
3. ```msql_client``` for connection to MySQL server,
4. ```mqtt_common``` that is library keeping code that is common for client and server implementation

Session state data is storing in database (DETS or MySQL in current version). Server can establish connection using different network protocols:
1. clear it/tcp
2. tls/ssl
3. web socket

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

### Starting

To start server run bash script:
```bash
$ ./start_mqtt_server.sh
```
Erlang shell will open and log statements are appearing in console.

### Using relx
To make release of the application run command:
```bash
$ /opt/local/bin/rebar3 as prod release
```
Go to folder:
```bash
$ cd _build/prod/rel/mqtt_server
```
and run command to start server:
```bash
$ bin/mqtt_server console
```

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

To set up ports for TCP and TLS socket connection go to mqtt.config. This is OTP application configuration file contained startup data for
lager, ranch and mqtt server configuration.

### Add/Remove users

The MQTT server is distributed with preset user dets db. Users are guest/guest and admin/admin.
To add other users run Erlang shell:
```bash
$ erl -pa _build/default/lib/*/ebin
```
and issue command in Erlang shell:
```erlang
1>mqtt_server:add_user("UserName", <<"UserPassword">>).
```
or for user delete:
```erlang
1>mqtt_server:remove_user("UserName").
```

## References

1. [https://mosquitto.org/] - Mosquitto MQTT server.
2. [https://www.rabbitmq.com/] - RabbitMQ server with MQTT plugin.
3. [https://sourceforge.net/projects/mqtt-client/] - Erlang MQTT client.
4. [http://www.hivemq.com/demos/websocket-client/] - MQTT websocket client.
5. [http://www.mqttfx.org/] - MQTT client.


