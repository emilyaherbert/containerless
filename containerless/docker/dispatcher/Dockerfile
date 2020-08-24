FROM ubuntu:18.04
RUN apt-get update
RUN apt-get install apt-utils libssl1.1
COPY dispatcher-launcher-agent /dispatcher-launcher-agent
CMD ["/dispatcher-launcher-agent"]