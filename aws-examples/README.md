# AWS-Examples
AWS Lambda functions in Rust translated from Node.
Each directory is a different AWS lambda function.

## Build
  1. Select a function you want to deploy and navigate to the directory.
  2. Add the Amazon Linux Environment as target to Rust:\
   `rustup target add x86_64-unknown-linux-musl`
  3. Add linker
     * If on Ubuntu/Debian, install musl-tools by running:\
       `sudo apt install musl-tools`
     * If on Mac, run:
       ```bash
       brew install filosottile/musl-cross/musl-cross
       mkdir .cargo
       echo '[target.x86_64-unknown-linux-musl]
       linker = "x86_64-linux-musl-gcc"' > .cargo/config
       ```
       According to Amazon's [Rust Runtime for AWS Lambda](https://aws.amazon.com/blogs/opensource/rust-runtime-for-aws-lambda/), 
       > On my system, some of the dependencies did not pick up the configured linker automatically and tried to use musl-gcc anyway. To get around this quickly, I simply created a symlink to the new linker:
       `ln -s /usr/local/bin/x86_64-linux-musl-gcc /usr/local/bin/musl-gcc`
  4. Everything should be all set, just run the bash script by doing:\
     `./build.sh`
  5. You can deploy it by following the Deploying the Function on AWS Lambda section in  [Rust Runtime for AWS Lambda](https://aws.amazon.com/blogs/opensource/rust-runtime-for-aws-lambda/)

## Functions list
Original node functions
- [Node simple HTTP endpoint](https://github.com/serverless/examples/blob/master/aws-node-simple-http-endpoint/handler.js)
