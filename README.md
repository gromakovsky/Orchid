# Orchid language

Orchid is a programming language not intended to be used be anyone. It's named Orchid because I can.

## Features

- [x] standard types: `int64` (signed 64-bit integer) and `bool`
- [x] statically typed global and local mutable variables
- [x] arithmetical and boolean expressions (e. g. `+`, `%`, `**`, `or`, `not`, `>=`, etc.)
- [x] `if` statement
- [x] `while` statement
- [x] `pass` statement
- [x] functions
- [x] comments
- [ ] primitive IO
- [ ] user-defined classes

## Syntax

Orchid's syntax is similar to the one used in Python programming language. However, it's extremely simplified and there are some changes because Orchid is statically typed and is not interpreted but compiled. Full grammar specification may be found in [grammar.txt](grammar.txt). Below is a less formal descprition of syntax.

### Overall structure

Input is a sequence of statements. Statement may be either simple or compound. Simple statement takes one line, while compound statement takes more than one line. Simple statements may consist of multiple substatements delimited by `;`. Comments start with `#` and last until the end of line. Comments and empty lines are completely ignored. Only function and global variable definitions are allowed as top-level statements (it may be changed in future versions).

### Indentation

Compound statements use indentation to group sequence of statements into blocks. It is implemented just like in Python, i. e. by introducing two special tokens: `INDENT` and `DEDENT`. Full rules may be found on [Python website](https://docs.python.org/3/reference/lexical_analysis.html#indentation). There are a difference though: only spaces are valid in `Orchid`, input MUST NOT contain tab characters.

### Function definition

Function definition starts with `def` keyword followed by function name, argument list, optional return type and `:` symbol. The next lines contain indented block with function's body. Argument list is enclosed in parenthesis and contains (possible empty) list of typed arguments delimited by `comma`. Trailing comma is permitted. Typed argument has a form `name : type`. Return type is separated by `→` (U+2192) character and represented as identifier. Function body is a sequence of statements. Nested function definitions are accepted by parser, but are prohibited in this version (the error is raised in such cases). 

Example:

```
def f(a : int64, b: int64) → int64:
    return a + b
```

### Variable definition

Variable definition may occur as a top-level statement (in this case it's global variable) or inside function body (local variable). It has a form `type name = value`.

Example:

```
int64 a = 42
```

### `pass`

There is a special keyword `pass` which doesn't do anything and may be used anywhere (even as top-level statement).

Example:

```
while True:  # hang
    pass
```

### Control flow statements

This version has only one control flow statement: `return`. It's represented using keyword `return` with optional expression after it.

Example:

```
def f() → bool:
  return True
```

### Assignment

Assingment is represented using `=` operator: `var = expr`.

Example:

```
a = 1
```

### Expressions

The most simple expressions are constants and variables itself. Integer constants are represented as usual, boolean constants are `True` and `False`. More complex expressions are function calls and operator applications. Function call has a form `function(arg1, arg2)`. Operators may be unary (`not`, `+`, `-`) and binary (`or`, `and`, `<`, `>`, `==`, `<=`, `>=`, `!=`, `+`, `-`, `*`, `/`, `%`, `**`). `**` is power, the rest are intuitive enough. Expressions may be enclosed in parenthesis to influence priority in a standard way.

Examples:

`True`, `1 < 2`, `False or True`, `f(1)`, `not (f(5 + 1) % g())`.

### `while`

While statement starts with a keyword `while`, followed by condition expression, `:` character and indented block with body.

Example:

```
while a < b:
  a = a + 1
```

### `if`

If statement start with a keyword `if`, followed by condition expression, `:` character and indent block with body. It may optionally contain `else:` statement on a separate line with the same indentation as `if` keyword.

Example:

```
if a < 0:
  return -a
else:
  return a
```

## Semantics

**TODO**

## Examples

Examples may be found in [examples directory](examples/).

# Orchid compiler

Orchid compiler (named `orchid`) is implemented in [Haskell](https://www.haskell.org/) programming language. Target platform is [LLVM](http://llvm.org/).

## Build dependencies

The only officially supported way to build this compiler is to use [stack](http://docs.haskellstack.org). Full list of dependencies is the following:

- LLVM 3.5
- stack

Note that by default stack will download and install correct version of `GHC` into isolated environment, that's why GHC is not listed in list of dependencies. You can modify this behavior by specifying `system-ghc: True` in [stack.yaml](stack.yaml). However, it may break compilation on some systems.

## Usage

Run `stack setup` and then `stack build` to build the compiler. Run `stack exec Orchid -- <ARGUMENTS>` to run the compiler. Run `stack exec orchid -- --help` to get a full list of options.

If you want to run tests use `stack test Orchid` command.