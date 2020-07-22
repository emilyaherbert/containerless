This directory contains several scripts that help run and test Containerless.

- `./deploy.sh` deploys Containerless to MicroK8s. *Warning*: We assume you
  have a single-node MicroK8s cluster. In particular, it mounts directories
  within this repository as volumes for storage agent, to make re-deploying
  Containerless faster after you have made changes.

- `./controller.sh` (EXPIRED) starts and stops the controller. Run `./controller.sh start`
  to start the controller after running `./deploy.sh`.

- `./test.sh` runs the integration tests.

- `./undeploy.sh` deletes all Containerless resources.

- `./start-lone-function.sh` manually launches a serverless function within a 
   pod that is not managed by the Containerless dispatcher. Therefore, the
   pod is *not* shutdown after a period of inactivity. This can help debug
   problems that occur in JavaScript, and not Rust. *Note:* Run `./deploy.sh`
   before running this script, to deploy the function storage service.

- `./stop-lone-function.sh` deletes a serverless function created by
  `./start-lone-function.sh`.
