#!/bin/bash

CONTAINERLESS=../../containerless/rust/target/debug/cli

$CONTAINERLESS create -n hi -f helloWorld.js
