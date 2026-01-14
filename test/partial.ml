type 'a op = E : unit op

module Eff = Handled_effect.Make (struct
    type 'a t = 'a op
  end)

exception Done

let handle_partial f =
  let handle = function
    | Eff.Value v -> v
    | Eff.Exception e -> raise e
    | Eff.Operation (E, _) -> assert false
  in
  handle (Eff.run f)
;;

let f _ =
  ();
  fun h2 -> Eff.perform h2 E
;;

let%expect_test ("partial effect handling" [@tags "runtime5-only"]) =
  let rec handle = function
    | Eff.Value _ -> assert false
    | Eff.Exception Done -> print_string "ok"
    | Eff.Exception e -> raise e
    | Eff.Operation (E, k) -> handle (Handled_effect.discontinue k Done [])
  in
  handle (Eff.run (fun h -> handle_partial f h));
  [%expect {| ok |}]
;;
