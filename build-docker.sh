#!/usr/bin/env bash

set -eux

docker build --tag fpco/static-base static-base

LAST_LINE=$(stack sdist 2>&1 | tail -n 1)
SDIST=${LAST_LINE##* }

rm -rf build-docker
mkdir build-docker
mkdir -p build-home
docker run --rm \
    -v $(pwd)/build-docker:/host-bin \
    -v $SDIST:/sdist.tar.gz \
    -v $(pwd)/build-home:/home/build \
    fpco/static-base \
    /bin/bash -c \
    'tar zxfv /sdist.tar.gz && cd pid1-* && stack install --test --local-bin-path /host-bin --ghc-options "-optl-static -fPIC -optc-Os" && upx --best --ultra-brute /host-bin/pid1'


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
