# Ophidian

A Python lexer, parser and interpreter (_eventually!_), written as a hobby project for educational purposes.

User-friendly errors courtesy of [`miette`](https://crates.io/crates/miette).

![`miette` error](images/miette_error.png)

## Lexing

Contents of `test.py`:

```python
if foo:
  a = 3+5j
```

```sh
ophidian lex test.py

If
True
Colon
NewLine
Indent
Identifier("a")
Equals
Complex("3+5j")
```

## Parsing

_In progress!_
