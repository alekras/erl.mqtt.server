# Stage 0: Build the release
FROM erlang:28-alpine AS builder
RUN apk add git
WORKDIR /erl.mqtt.server
# Copy the application source code
COPY . .
# clean up build folder with previous release
RUN rm -f -R _build/default/rel/mqtt_server_dev
# Build the release (using rebar3 as an example)
RUN rebar3 do version
RUN rebar3 do clean --all
RUN rebar3 release -n mqtt_server_dev

# Stage 1: Create the final, minimal image
FROM erlang:28-alpine
RUN mkdir mqtt_server
WORKDIR /mqtt_server

# Copy the release from the builder stage
COPY --from=builder erl.mqtt.server/_build/default/rel/mqtt_server_dev .

# Expose necessary ports 
EXPOSE 8080
EXPOSE 18883
EXPOSE 18483
EXPOSE 8880
EXPOSE 4443

# Command to run the Erlang application release
CMD ["bin/mqtt_server_dev", "console"]

# Command from host terminal to build image
# docker build -t mqtt_server_dev --file Dockerfile .

# docker run -it --rm erlang:slim erl
