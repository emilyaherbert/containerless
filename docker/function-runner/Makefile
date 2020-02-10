all: node_modules
	rsync -a ../../rust/function-runner-agent/target/debug/function-runner-agent .
	rsync -a ../../javascript/containerless node_modules/
	rsync -a ../../javascript/js-transform node_modules/
	docker build -t function-runner .
	docker tag function-runner localhost:5000/function-runner
	docker push localhost:5000/function-runner


node_modules:
	mkdir node_modules