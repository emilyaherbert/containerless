# decontainerization

Linked with [Neon](https://neon-bindings.com/).

### Commands:

~~~
yarn run build && neon build && yarn run run
yarn run build && neon build && yarn run test
yarn run build && neon build && yarn run test test/linked/
~~~

- `yarn run build`: builds TypeScript
- `neon build`: builds modules from Rust
- `yarn run run`: runs the thing
- `yarn run test`: tests the thing

### Organization

~~~
decontainerization
  ├── cloudbuild.yaml
  ├── Dockerfile
  ├── .dockerignore
  ├── .gitignore
  ├── native/
  │   └── ...
  ├── package.json
  ├── README.md
  ├── rust-trials/
  │   └── ...
  ├── test/
  │   └── ...
  ├── ts/
  │   └── ...
  └── tsconfig.json
~~~

- `ts`: TypeScript source
- `native`: Rust source
- `test`: tests