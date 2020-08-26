FROM ubuntu:latest
RUN apt-get update
RUN apt-get install libssl1.1
COPY dispatcher-launcher-agent /dispatcher-launcher-agent
CMD ["/dispatcher-launcher-agent"]