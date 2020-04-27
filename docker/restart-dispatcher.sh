# Deletes the pod containing the dispatcher, and all Kubernetes resources
# that the dispatcher creates dynamically. Since the dispatcher runs in
# a deployment, a pod will be recreated.
#!/bin/sh
microk8s.kubectl delete all --selector "dynamic=true" -n containerless 
microk8s.kubectl delete pod --selector "app=dispatcher" -n containerless 