FROM ubuntu:latest
RUN apt-get update
RUN apt-get install libssl1.1
COPY controller-agent /controller-agent
CMD ["/controller-agent"]