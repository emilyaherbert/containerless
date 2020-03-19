all:
	rsync -a ../../rust/controller-agent/target/debug/controller-agent .
	rsync -a --delete --exclude 'target' ../../rust/k8s .
	rsync -a --delete --exclude 'target' ../../rust/dispatcher-agent .
	docker build -t localhost:32000/controller .
	docker push localhost:32000/controller
