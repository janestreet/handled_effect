type 'a op =
  | Get : int op
  | Set : int -> unit op

module Eff = Handled_effect.Make (struct
    type 'a t = 'a op
  end)

open Eff

let handle_state init f x =
  let rec handle (state : int) = function
    | Value result -> result, state
    | Exception e -> raise e
    | Operation (Get, k) -> handle state (Handled_effect.continue k state [])
    | Operation (Set new_state, k) -> handle new_state (Handled_effect.continue k () [])
  in
  let res = run (fun h -> f h x) in
  handle init res
;;

let comp h () =
  Printf.printf "Initial state: %d\n" (perform h Get);
  perform h (Set 42);
  Printf.printf "Updated state: %d\n" (perform h Get);
  perform h (Set 43)
;;

let%expect_test ("shallow state handling" [@tags "runtime5-only"]) =
  let (), i = handle_state 0 comp () in
  Printf.printf "Final state: %d" i;
  [%expect
    {|
    Initial state: 0
    Updated state: 42
    Final state: 43
    |}]
;;
