all:
	rsync -a ../../rust/controller-agent/target/debug/controller-agent .
	docker build -t localhost:32000/controller .
	docker push localhost:32000/controller
