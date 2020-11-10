
FROM ubuntu:latest
RUN apt-get update
RUN apt-get install -yq libssl1.1
COPY log-echo-agent /log-echo-agent
CMD ["/log-echo-agent"]