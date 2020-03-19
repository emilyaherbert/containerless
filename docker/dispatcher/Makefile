all:
	rsync -a ../../rust/dispatcher-launcher-agent/target/debug/dispatcher-launcher-agent .
	docker build -t localhost:32000/dispatcher .
	docker push localhost:32000/dispatcher
