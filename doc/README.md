# PG-Entity Book

In order to build this book, you must install
[mdbook](https://rust-lang.github.io/mdBook/guide/installation.html).

## Continuous build

Run the following in the top-level directory:

```bash
$ ghcid --reload=./doc/src --target exe:book -T Main.main --setup ':set args process'
```

In another terminal, run

```bash
$ mdbook serve --open
```
