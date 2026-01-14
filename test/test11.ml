(* Tests [RESUMETERM] with [extra_args != 0] in bytecode, by calling a handler with a
   tail-continue that returns a function *)

open Handled_effect

type 'a op = E : int op

module Eff = Handled_effect.Make (struct
    type 'a t = 'a op
  end)

open Eff

let handle comp =
  let rec handle = function
    | Value v -> v
    | Operation (E, k) -> handle (continue k 10 [])
    | Exception e -> raise e
  in
  handle (Eff.run comp)
;;

let%expect_test ("[RESUMETERM] with [extra_args != 0]" [@tags "runtime5-only"]) =
  handle
    (fun h ->
      Printf.printf "%d\n" (perform h E);
      Printf.printf "%d\n")
    42;
  [%expect
    {|
    10
    42
    |}]
;;
