# Orchid language [![Build Status](https://travis-ci.org/gromakovsky/Orchid.svg?branch=master)](https://travis-ci.org/gromakovsky/Orchid)

Orchid is a programming language not intended to be used be anyone. It's named Orchid because I can.

## Features

- [x] standard types: `int64` (signed 64-bit integer) and `bool`
- [x] pointer types
- [x] statically typed global and local mutable variables
- [x] arithmetical and boolean expressions (e. g. `+`, `%`, `**`, `or`, `not`, `>=`, etc.)
- [x] `if` statement
- [x] `while` statement
- [x] `pass` statement
- [x] functions
- [x] comments
- [x] simple IO with basic error handling
- [x] user-defined classes
  - [x] public/private
  - [x] inheritance
  - [x] virtual methods
- [x] dynamic memory (new/delete)

## Syntax

Orchid's syntax is similar to the one used in Python programming
language. However, it's extremely simplified and there are some
changes because Orchid is statically typed and is not interpreted but
compiled. Full grammar specification may be found in
[grammar.txt](grammar.txt). Below is a less formal description of
syntax.

### Overall structure

Input is a sequence of statements. Statement may be either simple or
compound. Simple statement takes one line, while compound statement
takes more than one line. Simple statements may consist of multiple
substatements delimited by `;`. Comments start with `#` and last until
the end of line. Comments and empty lines are completely ignored. Only
function and global variable definitions are allowed as top-level
statements (it may be changed in future versions).

### Indentation

Compound statements use indentation to group sequence of statements
into blocks. It is implemented just like in Python, i. e. by
introducing two special tokens: `INDENT` and `DEDENT`. Full rules may
be found on
[Python website](https://docs.python.org/3/reference/lexical_analysis.html#indentation). There
are a difference though: only spaces are valid in `Orchid`, input MUST
NOT contain tab characters.

### Function definition

Function definition starts with `def` keyword followed by function
name, argument list, optional return type and `:` symbol. The next
lines contain indented block with function's body. Argument list is
enclosed in parenthesis and contains (possible empty) list of typed
arguments delimited by `comma`. Trailing comma is permitted. Typed
argument has a form `name : type`. Type of argument is an identifier
optionally followed by `*` (to denote pointer type). Return type is
separated by `→` (U+2192) character and represented as
identifier. Function body is a sequence of statements. Nested function
definitions are accepted by parser, but are prohibited in this version
(the error is raised in such cases). Function names starting with `__`
are reserved for internal usage by standard library.

Example:

```
def f(a : int64, b: int64) → int64:
    return a + b

def q(a : A *) → int64:
    (*a).z()
    return 0
```

### Variable definition

Variable definition may occur as a top-level statement (in this case
it's global variable) or inside function body (local variable). It has
a form `type name = value`.

Example:

```
int64 a = 42
```

### Class definition

Class definition starts with `class` keyword followed by class name,
optional parent class in parenthesis and `:` symbol. The next lines
contain indented block with class's body containing sequence of class
statements.

Class statement is either a function (method) definition or variable
(member) definition. Each class statement starts with access modifier
(either `public` or `private`). Function definition inside class body
is the same as for top-level function, except that it may be prefixed with
`virtual` keyword to denote virtual function. Variable definition
inside class body has exactly the same syntax as outside class
body. Initializer is used when object of the class is constructed.

Example:

```
class A:
    private int64 x = 0
    public f() → bool:
        return True

    virtual public g() → int64:
        return x

class B(A):
    virtual public g() → int64:
        return 8
```

### `pass`

There is a special keyword `pass` which doesn't do anything and may be
used anywhere (even as top-level statement).

Example:

```
while True:  # hang
    pass
```

### Control flow statements

This version has only one control flow statement: `return`. It's
represented using keyword `return` with optional expression after it.

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

The most simple expressions are constants and variables
itself. Integer constants are represented as usual, boolean constants
are `True` and `False`. More complex expressions are function calls
and operator applications. Function call has a form `function(arg1,
arg2)`. Operators may be unary (`not`, `+`, `-`, `*`, `&`) and binary
(`or`, `and`, `<`, `>`, `==`, `<=`, `>=`, `!=`, `+`, `-`, `*`, `/`,
`%`, `**`). Unary `*` dereferences pointer, `&` takes pointer, `**` is
power, the rest are intuitive enough. Expressions may be enclosed in
parenthesis to influence priority in a standard way.

Examples:

`True`, `1 < 2`, `False or True`, `f(1)`, `not (f(5 + 1) % g())`, `&a`.

### `while`

While statement starts with a keyword `while`, followed by condition
expression, `:` character and indented block with body.

Example:

```
while a < b:
  a = a + 1
```

### `if`

If statement start with a keyword `if`, followed by condition
expression, `:` character and indent block with body. It may
optionally contain `else:` statement on a separate line with the same
indentation as `if` keyword.

Example:

```
if a < 0:
  return -a
else:
  return a
```

### `new`

Operator `new` allocated memory in heap for the value of given type
and stores pointer under given name. Syntax is `new TYPE NAME`. Value
is not initialized.

Example:

```
new Point p
```

### `delete`

Operator `delete` frees memory allocated by `new`. It takes variable
name which must store pointer returned by `new`. Syntax is `delete
NAME`.

Example:

```
delete p
```

## Semantics

**TODO** Most of semantics is absolutely intuitive, but still has to be described here.

## Standard library

All functions in standard library start with `std` prefix. Standard
library contains the following functions:

- `stdExit(int64)`

  This function aborts execution of the program with given exit code
  and prints a message to stdout.

- `stdReadInt() → int64`

  This function reads 64-bit integer from stdin and expects `\n` in
  the end. It aborts execution with exit code `1` in case of any
  error.

- `stdReadBool() → bool`

  This function reads boolean value from stdin. It behaves just like
  `stdReadInt` and interprets `0` as `False` and all other numbers as
  `True`.

- `stdWriteInt(int64)`

  This function prints 64-bit integer followed by `\n` to stdout.  It
  aborts execution with exit code `2` in case of any error.

- `stdWriteBool(bool)`

  This function prints boolean value followed by `\n` to stdout. It
  behaves just like `stdWriteInt` and prints `True` as `1` and `False`
  as `0`.

- `stdPower(int64, int64)`

  `stdPower(a, b)` is the same as `a ** b`. It uses exponentation by
  squaring algorithm. It aborts execution with exit code `3` if both
  arguments are 0.

## Examples

Examples may be found in [examples directory](examples/).

# Orchid compiler

Orchid compiler (named `orchid`) is implemented in
[Haskell](https://www.haskell.org/) programming language. Target
platform is [LLVM](http://llvm.org/).

## Build dependencies

The only officially supported way to build this compiler is to use
[stack](http://docs.haskellstack.org). Full list of dependencies is
the following:

- LLVM 3.5
- stack
- `llvm-as` and `lli` are needed to run tests

### Mac OS

There is a known problem with build on Mac OS. It is described in this
[issue](https://github.com/commercialhaskell/stack/issues/1826). One
possible solution is to set correct value for `DYLD_LIBRARY_PATH`
environmental variable and workaround
[another bug](http://docs.haskellstack.org/en/stable/faq/#why-is-dyld_library_path-ignored).

## Usage

Run `stack setup` and then `stack build` to build the compiler. Run
`stack exec orchid -- <ARGUMENTS>` to run the compiler. Run `stack
exec orchid -- --help` to get a full list of options.

If you want to run tests use `stack test Orchid` command.

## Known bugs

- return is required even if function returns void
