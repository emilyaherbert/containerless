# NOTE: This file is very outdated -- use at your own risk!

# Supported Operations

|   |   |
|---|---|
| :heavy_check_mark: | supported |
| :grey_question: | unknown |
| :bangbang: | should be supported, unimplemented |
| :heavy_minus_sign: | will not be supported |

## Statements and Control Operators

|   |   |   |   |
|---|---|---|---|
| if-true | `if(true) { e1 } else { e2 }` | :heavy_check_mark: | |
|         | `if(true) { e1 }` | :heavy_check_mark: | |
| if-false | `if(false) { e1 } else { e2 }` | :heavy_check_mark: | |
|          | `if(false) { e1 }` | :heavy_check_mark: | |
| while | `while(e1) { e2 }` | :heavy_check_mark: | |
| while-false | `while(false) { e2 }` | :heavy_check_mark: | | 
| throw | | :heavy_minus_sign: | |
| try-catch | | :heavy_minus_sign: | |
| try-catch-pop | | :heavy_minus_sign: | |
| uncaught-exception | | :grey_question: | |
| try-finally-error | | :heavy_minus_sign: | |
| try-finally-break | | :heavy_minus_sign: | |
| try-finally-pop | | :heavy_minus_sign: | |
| label-break | `l: { e; break l v; }` | :heavy_check_mark: | |
| label-break-pop | `l1: { e; break l2 v; }` | :heavy_check_mark: | |
| label-pop | `l: { v }` | :grey_question: | |
| break-break | `break l1 (break l2 v)` | :grey_question: | |


## Objects

|   |   |   |   |
|---|---|---|---|
| object literal | `{x:1, y:2}` | :heavy_check_mark: |   |
| get field | `{x:1}.x` | :heavy_check_mark: |   |
|           | `{x:1}["x"]` | :grey_question: | TODO: merge this and ^ | 
| get field - not found | `{x:1}.y` -> `undefined` | :grey_question: | |
| get field - proto null | `{x:1, __proto__: null}.y` -> `undefined` | :bangbang: | |
| get field - proto | `{x:1, __proto__: l}.y` -> `(deref l).y` | :bangbang: | |
| update field | `{x:1}.x = 4` | :heavy_check_mark: |   |
| create field | `{x:1}.y = 4` | :grey_question: | |
| delete field | `delete {x.1}.x` | :bangbang: | |
| delete field - not found | `delete {x.1}.y` | :bangbang: | |

## Arrays

|   |   |   |
|---|---|---|
| | | |

## Operators

### Binary Operators
|   |   |   |
|---|---|---|
| `+` | :heavy_check_mark: | TODO: ensure it matches with implicit type conversions |
| `-` | :heavy_check_mark: | |
| `/` | :heavy_check_mark: | |
| `%` | :bangbang: | |
| `*` | :heavy_check_mark: | |
| `**` | :bangbang: | |
| `&` | :heavy_minus_sign: | |
| `\|` | :heavy_minus_sign: | |
| `>>` | :heavy_minus_sign: | |
| `>>>` | :heavy_minus_sign: | |
| `<<` | :heavy_minus_sign: | |
| `^` | :heavy_minus_sign: | |
| `==` | :heavy_minus_sign: | |
| `===` | :heavy_check_mark: | |
| `!=` | :heavy_minus_sign: | |
| `!==` | :heavy_check_mark: | |
| `in` | :bangbang: | |
| `instanceof` | :grey_question: | |
| `>` | :heavy_check_mark: | |
| `<` | :heavy_check_mark: | |
| `>=` | :heavy_check_mark: | |
| `<=` | :heavy_check_mark: | |
| `++` | :bangbang: | NOTE: might get desugared with js-transform |
| `--` | :bangbang: | NOTE: might get desugared with js-transform |

### Logical Operators

|   |   |   |
|---|---|---|
| | | |

### Unary Operators

|   |   |   |
|---|---|---|
| | | |

## Keywords

|   |   |   |
|---|---|---|
| `this` | :grey_question: | |
| `instanceof` | :grey_question: | |
| `typeof` | :grey_question: | TODO: ensure all cases are implemented |

## Type Conversions

|   |   |   |
|---|---|---|
| | | |

## Assorted

### Prototype-based Objects
:grey_question:

### Functions as Objects
:heavy_minus_sign:

### Local Variables

|   |   |   |
|---|---|---|
| `var` | :heavy_check_mark: | |
| `let` | :heavy_check_mark: | |

### Global Variables

:grey_question:

### With Statements

:heavy_minus_sign:
