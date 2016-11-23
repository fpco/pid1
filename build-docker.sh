#!/usr/bin/env bash

set -eux

rm -rf build-docker
mkdir -p build-docker
stack install --local-bin-path build-docker

cat > build-docker/Dockerfile <<EOF
FROM ubuntu:16.04
MAINTAINER Michael Snoyman (michael@fpcomplete.com)
RUN apt-get update && apt-get install -y libgmp10
ADD pid1 /sbin/pid1
ENTRYPOINT ["/sbin/pid1", "--RTS"]
EOF

docker build --tag fpco/pid1:16.04 build-docker

# Sanity check
docker run --rm fpco/pid1:16.04 ps

# Push
docker tag fpco/pid1:16.04 fpco/pid1:latest
docker push fpco/pid1:16.04
docker push fpco/pid1:latest
