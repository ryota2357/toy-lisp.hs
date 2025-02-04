# toy-lisp

Learning Haskell by implementing a (Common) Lisp interpreter.

## Syntax

`toy-lisp` follows a basic Lisp syntax, similar to Scheme and Common Lisp.
It supports S-expressions using parentheses (`(` and `)`), strings enclosed in double quotes (`"`), quoting with `'`, as well as integers and floating-point numbers.

However, as this project is primarily for learning Haskell, certain syntactic features are unsupported.

### Identifiers

Identifiers are used as variable names or symbols.
Only ASCII characters are allowed in' toy-lisp', excluding whitespace, `"`, `'`, and certain other symbols.

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

Only single-line comments using `;` are supported.

```lisp
; This is a single-line comment
```

### Strings

`toy-lisp` does not support character types, only strings.

Strings are enclosed in double quotes (`"`).
The only supported escape sequence is `\"` (double quote escape). 
Other backslash sequences are not treated as escapes but are interpreted as literal characters.

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

- Integer (`Integer`)
- Float (`Double`)
- String (`String`)
- Symbol (a `newtype` wrapper around `Text`)
- Null (`[]`)
- List (`[a]`)
- Function
- T

Similar to Common Lisp, `'()` is equivalent to `nil`. 
These types can be inspected using the `type-of` function.

### Notes

- There is no Cons Cell type. Lists are represented using Haskell’s native list type (`[a]`).
- While string types exist, character types do not.

## Functions

A subset of functions defined in Common Lisp is available.
Each function is implemented to be as close as possible to its ANSI Common Lisp definition.

There are two types of functions provided in `toy-lisp`:

1. Functions implemented in Haskell.
2. Functions defined in app/prelude.lisp.

The reason for this split is simply for convenience—implementing everything in Haskell would be cumbersome.
The loading of [app/prelude.lisp](app/prelude.lisp) can be disabled using the `--no-prelude` command-line argument.

### Functions Implemented in Haskell

The implementation can be found in [src/ToyLisp/Evaluator.hs](src/ToyLisp/Evaluator.hs) under `systemFunctionBindingsMap`.

| Function Name | Arguments | Description | 
| :--    | :---     | :--- |
|        |          |      |

### Functions in prelude.lisp

The implementation can be found in [app/prelude.lisp](app/prelude.lisp).

| Function Name | Arguments | Description | 
| :--    | :---     | :--- |
|        |          |      |
