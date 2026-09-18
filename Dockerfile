ARG RELEASE_NAME=mqtt_server_dev

# Stage 0: Build the release
FROM erlang:28-alpine AS builder
ARG RELEASE_NAME
RUN apk add git
WORKDIR /erl.mqtt.server
# Copy the application source code
COPY . .
# clean up build folder with previous release
RUN rm -f -R _build/default/rel/$RELEASE_NAME
# Build the release (using rebar3 as an example)
RUN rebar3 do version
RUN rebar3 do clean --all
RUN rebar3 release -n $RELEASE_NAME

# Stage 1: Create the final, minimal image
FROM erlang:28-alpine
ARG RELEASE_NAME
RUN mkdir mqtt_server
WORKDIR /mqtt_server

# Copy the release from the builder stage
COPY --from=builder erl.mqtt.server/_build/default/rel/$RELEASE_NAME .

# Expose necessary ports 
EXPOSE 8080
EXPOSE 18883
EXPOSE 18483
EXPOSE 8880
EXPOSE 4443

# Command to run the Erlang application release
ENV RELEASE_NAME=$RELEASE_NAME
CMD bin/${RELEASE_NAME} console

# Command from host terminal to build image
#DEV:  docker build --build-arg RELEASE_NAME=mqtt_server_dev -t mqtt_server_dev --file Dockerfile .
#PROD: docker build --build-arg RELEASE_NAME=mqtt_server -t mqtt_server_dev --file Dockerfile .

# docker run -it --rm erlang:slim erl
