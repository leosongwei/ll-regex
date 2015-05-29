README.md
=========

leo's light regex egine, for Common Lisp.

**ll-regex is released under a BSD style license, please see LICENSE.md.**

Feature:

repeat operators:

* *
* +
* ?

meta character:

* \d digit
* \e lower case
* \E upper case

Support parentheses and "|" operator for union.

Example:

```
(setf D2 (construct-dfa "a(b|)c"))
(match "ac" D2)
(match "abc" D2)
(match "abbc" D2)
```

for more info, see the docstring of MATCH (in ./match.lsp).

for update, see https://github.com/leosongwei/ll-regex
