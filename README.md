# Ophidian

A Python lexer and parser, written as a hobby project for educational purposes.

User-friendly errors courtesy of [`miette`](https://crates.io/crates/miette).

![`miette` error](images/miette_error.png)

## Lexing

Contents of `test.py`:

```python
class Solution:
    def is_anagram(self, s: str, t: str) -> bool:
        if len(s) != len(t):
            return False

        counts = [0] * 26
        for i in range(len(s)):
            counts[ord(s[i]) - ord("a")] += 1
            counts[ord(t[i]) - ord("a")] -= 1

        for i in counts:
            if i != 0:
                return False

        return True
```

```sh
ophidian lex test.py

Class
Identifier("Solution")
Colon
NewLine
Indent
Def
Identifier("is_anagram")
LeftParen
Identifier("self")
Comma
Identifier("s")
Colon
Identifier("str")
Comma
Identifier("t")
Colon
Identifier("str")
RightParen
Arrow
Identifier("bool")
Colon
NewLine
Indent
If
...
```

## Parsing

_In progress!_
