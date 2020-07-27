#!/bin/bash

CONTAINERLESS=$HOME/decontainerization/containerless/rust/target/debug/cli

$CONTAINERLESS create-function -n hello -f helloWorld.js
