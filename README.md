# Legends of Code and Magic (OCaml submission)

This is my submission to the CodinGame contest "[Legends of Code and Magic][legends]".  It got me to the
Bronze league but I believe it was a good basis for the Silver league.

## Build

This worked with OCaml 4.07.0.

```bash
opam install alcotest dune
dune build
dune runtest
```

This produces:

* `_build/default/answer.ml`: Assembled source code that could be submitted to the game server.
* `_build/default/sort_cards.exe`: Utility that shows in what order the rating algorithm sorts
  the cards.

[legends]: https://jakubkowalski.tech/Projects/LOCM/
