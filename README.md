# toy-lisp

Learning Haskell by implementing a (Common) Lisp interpreter.

## Quick Start

This project is managed using [Nix](https://nixos.org/).
While it can be used as a standard Cabal (Haskell) project, using Nix (with flakes enabled) is highly recommended.
For development, `devShell` provides a convenient environment, and [nix-direnv](https://github.com/nix-community/nix-direnv) can further enhance the experience.

### Build & Run

If using Nix, you do not need to install `cabal`, `ghc`, or any additional dependencies.
Building and running can be done entirely with Nix (without requiring `devShell` or `nix-direnv`).

To build, use `nix build`, which generates the binary inside the `result` directory:

```console
$ nix build

$ ./result/bin/toy-lisp
ToyLisp REPL
>
```

To execute a script file, pass the file path using the `--script` option:

```console
$ ./result/bin/toy-lisp --script [YourFilePath]
```

### Using devShell

For development, using `devShell` is more convenient.

The `cabal` command is available in `devShell`, allowing you to build and run the project with:

```console
$ cabal run
ToyLisp REPL
>
```

Similarly, to execute a script file, use the `--script` option.
However, it cannot be passed directly to `cabal run`; instead, use:

```console
$ cabal run toy-lisp -- --script [YourFilePath]
```

### Installation

You can install `toy-lisp` using the Nix profile system.

Run the following command in the same directory as `flake.nix`:

```console
$ nix profile install .

$ toy-lisp
ToyLisp REPL
>
```

## Syntax

`toy-lisp` follows a basic Lisp syntax, similar to Scheme and Common Lisp.
It supports S-expressions using parentheses (`(` and `)`), strings enclosed in double quotes (`"`), quoting with `'`, as well as integers and floating-point numbers.

However, since this project is primarily for learning Haskell, certain syntactic features are unsupported.

### Identifiers

Identifiers are used as variable names or symbols.
Only ASCII characters are allowed in `toy-lisp`, excluding whitespace, `"`, `'`, and certain other symbols.

**Invalid Example:**

```lisp
; !! This code does not work !!
(let ((変数 10)) 変数)
```

**Valid Example:**

```lisp
(let ((x 10)) x)
```

### Comments

Only single-line comments using `;` are supported:

```lisp
; This is a single-line comment
```

### Strings

`toy-lisp` does not support character types, only strings.

Strings are enclosed in double quotes (`"`).
The only supported escape sequence is `\"` (escaped double quote).
Other backslash sequences are not treated as escapes and are interpreted as literal characters.

```lisp
(princ "This is a string with a \"quote\" inside.")
(print "Multiline strings are allowed:
Second line")
```

### Numbers (Integers and Floats)

Numbers are parsed using Haskell’s `Text.Read` module (`readMaybe`).
This means `1e9` notation is supported, but leading-dot decimals like `.12` are not.

```lisp
(princ (+ 42 3.14))  ; Integer and float operations
(print 1e9)          ; Scientific notation supported
```

### Quote (`'`)

Lists and symbols can be quoted using `'`.
The parser expands `'` expressions, e.g., `'a` becomes `(quote a)`, and `'(1 2 3)` becomes `(quote (1 2 3))`.

```lisp
(princ 'symbol)
(print '(1 2 3))
```

## Data Types

`toy-lisp` supports the following data types (with corresponding Haskell representations in parentheses):

- INTEGER (`Integer`)
- FLOAT (`Double`)
- STRING (`String`)
- SYMBOL (a `newtype` wrapper around `Text`)
- NULL (`[]`)
- LIST (`[a]`)
- FUNCTION
- T

Similar to Common Lisp, `'()` is equivalent to `nil`.
These types can be inspected using the `type-of` function.

### Notes

- There is no Cons Cell type. Lists are represented using Haskell’s native list type (`[a]`).
- While string types exist, character types do not.

## Functions

A subset of Common Lisp functions is available.
Each function is implemented as closely as possible to the ANSI Common Lisp definition, referencing SBCL’s behavior.

There are two types of functions provided in `toy-lisp`:

1. Functions implemented in Haskell.
2. Functions defined in `app/prelude.lisp`.

### Functions Implemented in Haskell

The implementation can be found in [`src/ToyLisp/Evaluator.hs`](src/ToyLisp/Evaluator.hs) under `systemFunctionBindingsMap`.

`toy-lisp` does not support defining functions with variable-length arguments, so such functions must be implemented in Haskell.

<details>
<summary>Click to expand the list of functions</summary>

- `*`
- `+`
- `-`
- `/`
- `/=`
- `<`
- `<=`
- `=`
- `>`
- `>=`
- `and`
- `car`
- `cdr`
- `cond`
- `defparameter`
- `defun`
- `defvar`
- `eq`
- `eql`
- `equal`
- `equalp`
- `funcall`
- `function`
- `if`
- `lambda`
- `let`
- `list`
- `mod`
- `nthcdr`
- `or`
- `princ`
- `prog1`
- `progn`
- `quote`
- `rem`
- `setq`
- `type-of`

</details>

### Functions in prelude.lisp

The implementation can be found in [`app/prelude.lisp`](app/prelude.lisp).

The loading of this file can be disabled using the `--no-prelude` command-line argument.

<details>
<summary>Click to expand the list of functions</summary>

- `print`
- `terpri`
- `not`
- `null`
- `atom`
- `listp`
- `first`
- `second`
- `third`
- `fourth`
- `fifth`
- `sixth`
- `seventh`
- `eighth`
- `ninth`
- `tenth`
- `nth`
- `rest`
- `last`
- Various `car`/`cdr` compositions (`caar`, `cadr`, ..., `cddddr`)

</details>

## Reference

- [Common Lisp Community Spec (CLCS)](https://cl-community-spec.github.io/pages/index.html)
