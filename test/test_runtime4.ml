type 'a op = E : unit op [@@warning "-37"]

module Eff = Handled_effect.Make (struct
    type 'a t = 'a op
  end)

let%expect_test ("failure" [@tags "runtime4-only"]) =
  let handle = function
    | Eff.Value x -> x
    | Eff.Exception e -> raise e
    | Eff.Operation (E, _) -> 11
  in
  Expect_test_helpers_base.require_does_raise (fun () -> handle (Eff.run (fun _ -> 10)));
  [%expect {| (Failure "Effects require the OCaml 5 runtime.") |}]
;;
