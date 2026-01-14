type 'a op = E : unit op [@@warning "-37"]

module Eff = Handled_effect.Make (struct
    type 'a t = 'a op
  end)

let%expect_test ("continuation comparison and hashing" [@tags "runtime5-only"]) =
  let handle = function
    | Eff.Value x -> x
    | Eff.Exception e -> raise e
    | Eff.Operation (E, k) ->
      (match
         (* We have to make sure that neither the match nor the call to [caml_equal] are
            eliminated, so we call [print_string] and we print the result of [caml_equal]. *)
         print_string "";
         k = k
       with
       | b ->
         Printf.printf "%b" b;
         assert false
       | exception Invalid_argument _ -> print_endline "ok");
      (match Hashtbl.hash k with
       | _ -> print_endline "ok")
  in
  handle (Eff.run (fun h -> Eff.perform h E));
  [%expect
    {|
    ok
    ok
    |}]
;;
