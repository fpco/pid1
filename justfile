# List all recipies
default:
    just --list --unsorted

# Build image
build-image:
    docker build --file Dockerfile . --tag pid1

# Copy static binary from container
copy-static-pid1: build-image
    #!/usr/bin/env bash
    set -euo pipefail
    CID=$(docker create pid1)
    docker cp ${CID}:/app/pid1 .
    docker rm ${CID}
    ls -lah pid1

# Sanity test the binary
test:
    file ./pid1
    ./pid1 -- ps
