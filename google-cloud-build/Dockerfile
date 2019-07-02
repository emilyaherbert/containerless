FROM ubuntu:18.04
RUN apt-get update
RUN apt-get install -yq curl build-essential
RUN curl https://sh.rustup.rs -sSf > rustup.sh
RUN bash rustup.sh -y
ENV PATH /root/.cargo/bin:$PATH
RUN curl -sL https://deb.nodesource.com/setup_12.x | bash -
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" > /etc/apt/sources.list.d/yarn.list
RUN apt-get update -q
RUN apt-get install -yq nodejs yarn
ENTRYPOINT /workspace/build.sh