FROM ubuntu:18.04
RUN apt-get update
RUN apt-get install -yq curl build-essential
RUN curl https://sh.rustup.rs -sSf > rustup.sh
RUN bash rustup.sh -y
COPY rs /root/rs
COPY ts /root/ts
ENV PATH /root/.cargo/bin:$PATH
WORKDIR /root/rs
RUN cargo build
RUN $HOME/.cargo/bin/cargo test