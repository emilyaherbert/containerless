FROM ubuntu:18.04
RUN apt-get update
RUN apt-get install -yq libssl1.1
COPY function-storage-agent /function-storage-agent
CMD ["/function-storage-agent"]