all:
	rsync -a ../../rust/function-storage-agent/target/debug/function-storage-agent .
	docker build -t localhost:32000/function-storage .
	docker push localhost:32000/function-storage
