type 'a op = E : unit op [@@warning "-37"]

module Eff = Handled_effect.Make (struct
    type 'a t = 'a op
  end)

open Eff

let rec even n = if n = 0 then true else handle (Eff.run (fun _ -> odd (n - 1)))
and odd n = if n = 0 then false else even (n - 1)

and handle = function
  | Value v -> v
  | Operation (E, _) -> assert false
  | Exception e -> raise e
;;

let%expect_test ("evenodd" [@tags "runtime5-only"]) =
  let n = 100_000 in
  Printf.printf "even %d is %B\n%!" n (even n);
  [%expect {| even 100000 is true |}]
;;
