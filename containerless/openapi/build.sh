#!/bin/bash
# There are two Rust code generators -- "rust" and "rust-server". The "rust"
# code generator is deprecated, so we use "-g rust-server".
npx @openapitools/openapi-generator-cli generate \
    -i log-echo.yaml \
    -g rust-server \
    -o ../rust/log-openapi \
    --additional-properties=packageName=log-openapi