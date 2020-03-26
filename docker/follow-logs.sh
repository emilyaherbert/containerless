#!/bin/bash
microk8s.kubectl logs --selector app -n containerless -f
