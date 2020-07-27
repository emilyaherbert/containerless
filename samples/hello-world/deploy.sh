#!/bin/bash

CONTAINERLESS=$HOME/decontainerization/containerless/rust/target/debug/cli

$CONTAINERLESS create-function -n helloWorld -f helloWorld.js
