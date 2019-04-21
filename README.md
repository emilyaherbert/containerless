# neon-decontainerization

`yarn run build && neon build && yarn run run`
`yarn run build && neon build && yarn test`

### Commands:

- `yarn run build`: builds TypeScript
- `neon build`: builds modules from Rust
- `yarn run run`: runs the thing
- `yarn run test`: tests the thing

### Organization

`
decontainerization
  ├── .gitignore
  ├── native/
  │   └── ...
  ├── package.json
  ├── README.md
  ├── test/
  │   └── ...
  ├── ts/
  │   └── ...
  └── tsconfig.jsonls
`

- `ts`: TypeScript source
- `native`: Rust source
- `test`: tests