FROM alpine

RUN apk add -U git

COPY .stack-work/install/x86_64-linux-*/lts-10.6/8.2.2/bin/services-gaffer /usr/bin/services-gaffer
