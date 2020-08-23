FROM ubuntu:latest
RUN apt-get update
RUN apt-get install libssl1.1
COPY function-storage-agent /function-storage-agent
CMD ["/function-storage-agent"]