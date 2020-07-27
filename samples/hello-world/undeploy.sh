#!/bin/bash

CONTAINERLESS=$HOME/decontainerization/containerless/rust/target/debug/cli

$CONTAINERLESS delete-function -n helloWorld
