FROM rust:1.41
RUN apt-get update
RUN apt-get install libssl1.1
RUN rustup component add rustfmt
COPY controller-agent /controller-agent
CMD ["/controller-agent"]