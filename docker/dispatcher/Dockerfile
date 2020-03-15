FROM ubuntu:latest
RUN apt-get update
RUN apt-get install libssl1.1
COPY dispatcher-agent /dispatcher-agent
CMD ["/dispatcher-agent"]