type 'a op = E : unit op [@@warning "-37"]

module Eff = Handled_effect.Make (struct
    type 'a t = 'a op
  end)

let%expect_test ("basic value handling" [@tags "runtime5-only"]) =
  let handle = function
    | Eff.Value x -> x
    | Eff.Exception e -> raise e
    | Eff.Operation (E, _) -> 11
  in
  Printf.printf "%d\n%!" (handle (Eff.run (fun _ -> 10)));
  [%expect {| 10 |}]
;;
