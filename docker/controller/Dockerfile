FROM rust:1.42
RUN apt-get update -qq
RUN apt-get install -y -qq libssl1.1
RUN rustup component add rustfmt
COPY controller-agent /controller-agent
CMD ["/controller-agent"]