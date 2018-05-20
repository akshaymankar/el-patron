FROM debian:9.4-slim

RUN apt-get update && \
    apt-get install -y git openssh-client netbase && \
    rm -rf /var/lib/apt/lists/*

RUN git config --global user.email "el-patron@example.com"
RUN git config --global user.name "el-patron"

COPY .stack-work/install/x86_64-linux-*/lts-10.6/8.2.2/bin/el-patron-api /usr/bin/el-patron-api
