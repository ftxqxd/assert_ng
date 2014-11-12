`assert_ng!`
============

This provides an improved assert macro for the Rust programming language,
eliminating the need for `assert_eq!` while also providing equivalents for `!=`,
`>` and so on.

The basic idea is that if the macro is provided a condition formatted like `a ==
b`, it’ll do what `assert_eq!` does, i.e., print out `a` and `b` if the
assertion fails. To avoid this functionality, simply surround the condition in
parentheses: `assert!((a == b))`.
