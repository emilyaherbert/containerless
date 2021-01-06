all:
	rsync -a ../../rust/target/debug/function-storage-agent .
	docker build -t localhost:32000/function-storage .
	docker push localhost:32000/function-storage

release:
	rsync -a ../../rust/target/release/function-storage-agent .
	docker build -t localhost:32000/function-storage .
	docker push localhost:32000/function-storage
