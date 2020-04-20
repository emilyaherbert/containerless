all:
	rsync -a ../../rust/target/debug/controller-agent .
	docker build -t localhost:32000/controller .
	docker push localhost:32000/controller
