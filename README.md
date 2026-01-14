# Typed algebraic effects for OxCaml

OCaml 5 introduced *effect handlers* - a mechanism for modular programming
with user-defined *algebraic effects*. Effect handlers allow programmers to describe
computations that perform effectful operations, whose meaning is described by handlers
enclosing them. Effects in OCaml 5 are built on top of *delimited continuations*,
which generalize non-local control flow mechanisms like exceptions, green threads,
coroutines, generators, and async. Unlike monads, they can be composed.

However, the standard library interface for effects in OCaml is not *effect-safe*:
the compiler provides no guarantee that every performed effectful operation will
be handled.

Luckily, OxCaml gives us a convenient tool to track whether given code has
an effect handler installed on its stack: the `local` mode. Each effect handler can be
represented as a `local` argument, which is needed to perform its operations.
Functions accepting `local` functions are then effect-polymorphic:

```ocaml
val divide : ExnHandler.t @ local -> int -> int -> int
val print : IoHandler.t @ local -> string -> unit
val map : 'a list -> f:('a -> 'b) @ local -> 'b list
val catch : f:(ExnHandler.t @ local -> 'a) @ local -> 'a option

let divide_by_list m ns = catch ~f:(fun h -> map ns ~f:(fun n -> divide h m n))
```
