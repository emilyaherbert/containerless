FROM ubuntu:18.04
RUN apt-get update
RUN apt-get install -yq libssl1.1
COPY local-database-agent /local-database-agent
CMD ["/local-database-agent"]