FROM ubuntu:20.04

MAINTAINER Michael Snoyman (michael@fpcomplete.com)

RUN apt-get update && apt-get install -y libgmp10
COPY pid1 /sbin/pid1

ENTRYPOINT ["/sbin/pid1", "--RTS"]
