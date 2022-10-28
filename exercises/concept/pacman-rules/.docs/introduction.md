# Introduction

Booleans in Haskell are represented by the `Bool` type, which values can be either `True` or `False`.

Haskell supports three boolean operators: `not` (NOT), `&&` (AND), and `||` (OR).

The three boolean operators each have a different operator precedence, which causes them to be evaluated in this order: `not` first, `&&` second, and finally `||`.
If you want to override these rules, you can enclose a boolean expression in parentheses (`()`), as the parentheses have an even higher operator precedence.
