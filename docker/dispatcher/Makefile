all:
	rsync -a ../../rust/dispatcher-agent/target/debug/dispatcher-agent .
	docker build -t localhost:32000/dispatcher .
	docker push localhost:32000/dispatcher
