all:
	rsync -a ../../rust/target/debug/log-echo-agent .
	docker build -t localhost:32000/controller-logger .
	docker push localhost:32000/controller-logger
