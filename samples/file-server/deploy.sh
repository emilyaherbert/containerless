#!/bin/bash

CONTAINERLESS=../../containerless/rust/target/debug/cli

$CONTAINERLESS create -n fileserver -f fileServer.js
