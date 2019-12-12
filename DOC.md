
## Supported Operations

|   |   |
|---|---|
| :heavy_check_mark: | supported |
| :grey_question: | unknown |
| :bangbang: | should be supported, unimplemented |
| :heavy_minus_sign: | will not be supported |

# Objects

|   |   |   |   |
|---|---|---|---|
| object literal | `{x:1, y:2}` | :heavy_check_mark: |   |
| get field | `{x:1}.x` | :heavy_check_mark: |   |
|           | `{x:1}["x"]` | :grey_question: | will want to merge this and ^ | 
| get field - not found | `{x:1}.y` -> `undefined` | :grey_question: | |
| update field | `{x:1}.x = 4` | :heavy_check_mark: |   |
| create field | `{x:1}.y = 4` | :grey_question: | |
| delete field | `delete {x.1}.x` | :bangbang: | |
| delete field - not found | `delete {x.1}.y` | :bangbang: | |
