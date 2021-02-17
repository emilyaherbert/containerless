#!/bin/bash

CONTAINERLESS=../../containerless/rust/target/debug/cli

while :
do
  sleep 1s
  $CONTAINERLESS invoke -n hi
done
