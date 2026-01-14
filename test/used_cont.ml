type 'a op = E : unit op

module Eff = Handled_effect.Make (struct
    type 'a t = 'a op
  end)

let r = ref (None : (unit, int, unit) Eff.Continuation.t option)

let%expect_test ("used continuation test" [@tags "runtime5-only"]) =
  let rec handle = function
    | Eff.Value n -> assert (n = 42)
    | Eff.Exception e -> raise e
    | Eff.Operation (E, k) ->
      handle (Handled_effect.continue (Basement.Stdlib_shim.Obj.magic_unique k) () []);
      r := Some (Basement.Stdlib_shim.Obj.magic_unique k);
      Gc.full_major ();
      print_string "ok"
  in
  handle
    (Eff.run (fun h ->
       Eff.perform h E;
       42));
  [%expect {| ok |}]
;;
