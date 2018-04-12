FROM debian:9.4-slim

RUN apt-get update && \
    apt-get install -y git openssh-client netbase && \
    rm -rf /var/lib/apt/lists/*

COPY .stack-work/install/x86_64-linux-*/lts-10.6/8.2.2/bin/services-gaffer /usr/bin/services-gaffer
