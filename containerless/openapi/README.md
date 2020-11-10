This directory contains OpenAPI specifications, which we use to generate stubs
for HTTP clients and servers. We commit the generated code to the repository,
which is not something that we usually do. But, I think of it as the same
as committing a generated `Cargo.lock` or `package-lock.json` file to the
repository.

## Installation

These steps must be run once to install the OpenAPI code generator.

- Install a Java runtime, e.g., `sudo apt-get install default-jre`
- Run `./install.sh`, which installs the OpenAPI code generator in this
  directory. The code generator is an NPM package, and `./..gitignore` file
  is configured to ignore the installed code generator. You can also install
  the code generator globally using a small variation of `install.sh`
  (the `-g` flag to `npm`).

## Generating Code

If you edit a `.yaml` file in this directory to modify an API, you need to
run the code generator to generate new Rust code:

```
./build.sh
```