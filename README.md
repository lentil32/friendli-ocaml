## How to run

```
opam switch create . 5.2.0
dune build
opam install --deps-only
dune exec bot
```



## References

- Actor model: https://doc.akka.io/docs/akka/current/typed/guide/tutorial_1.html
- Multicore OCaml: https://github.com/ocaml-multicore/eio
