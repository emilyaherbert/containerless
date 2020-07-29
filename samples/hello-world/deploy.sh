#!/bin/bash

CONTAINERLESS=$HOME/decontainerization/containerless/rust/target/debug/cli

$CONTAINERLESS create -n hi -f helloWorld.js
