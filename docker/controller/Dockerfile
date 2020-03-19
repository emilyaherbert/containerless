FROM rust:1.41
RUN apt-get update
RUN apt-get install libssl1.1
RUN mkdir /src
COPY dispatcher-agent /src/dispatcher-agent
COPY k8s /src/k8s
RUN (cd /src/dispatcher-agent && cargo fetch)
RUN (cd /src/dispatcher-agent && cargo build)
COPY controller-agent /controller-agent
CMD ["/controller-agent"]