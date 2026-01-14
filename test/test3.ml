type 'a op = E : unit op [@@warning "-37"]

exception X

module Eff = Handled_effect.Make (struct
    type 'a t = 'a op
  end)

open Eff

let%expect_test ("exception handling" [@tags "runtime5-only"]) =
  let handle = function
    | Value v -> v
    | Exception X -> 10
    | Exception e -> raise e
    | Operation (E, _) -> 11
  in
  Printf.printf
    "%d\n%!"
    (handle
       (Eff.run (fun _ ->
          Printf.printf "in handler. raising X\n%!";
          raise X)));
  [%expect
    {|
    in handler. raising X
    10
    |}]
;;
