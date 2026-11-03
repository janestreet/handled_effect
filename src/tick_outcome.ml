open! Base

type t = Stdlib.Effect.tick_outcome =
  | Preempt
  | Continue
