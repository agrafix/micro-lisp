# micro-lisp

A small lisp interpreter written in Haskell with minimal dependencies. A few notes:

* The implementation currently includes a small parser combinator library derived of [my blog post](https://www.athiemann.net/2016/05/27/parser-combinators.html). This could easily be replaced, but I wanted to keep the dependencies minimal for now. A result is that currently the parser error messages are not very friendly and comments are not supported.
* The behavior of the implementation is loosely based on [carld/micro-lisp](https://github.com/carld/micro-lisp), but somewhat stricter in a sense that more sanity checks are performed during runtime resulting in error messages instead of crashes.
* A test suite capturing most of the current semantics.

Pull requests are welcome - if you find a bug or plan to add a feature please include unit tests.

A blog post might follow... :-)

## Setup

* Download and install [Haskell Stack](http://haskellstack.org/).
* Clone the repo, run `stack setup && stack build`
* To install system wide run `stack install`.

## Usage

```bash
# from file
micro-lisp -f [source-file]

# inline
micro-lisp "(+ 1 1)"

# from standard in
echo "(+ 1 1)" | micro-lisp
```
