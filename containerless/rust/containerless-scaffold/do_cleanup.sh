#!/bin/bash

killall containerless-scaffold
docker stop -t 0 vanilla-{0..5}
docker stop -t 0 tracing
