#!/bin/bash

# This builds an executable with Amazon Linux as target and zips up the executable
# to be uploaded to AWS Lambda
cargo build --release --target x86_64-unknown-linux-musl && \
zip -j lambda.zip ./target/x86_64-unknown-linux-musl/release/bootstrap