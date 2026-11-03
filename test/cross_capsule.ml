type 'a op =
  | Set : int -> unit op
  | Get : int op

module Eff = Handled_effect.Make (struct
    type 'a t = 'a op
  end)

let store = ref 0

let rec handle = function
  | Eff.Contended.Value v -> v
  | Exception e -> raise e
  | Operation (Set n, k) ->
    store := n;
    handle (Handled_effect.continue k () [])
  | Operation (Get, k) ->
    let n = !store in
    handle (Handled_effect.continue k n [])
;;

let%expect_test ("cross capsule data sharing" [@tags "effects-enabled"]) =
  handle
    (Eff.Contended.run (fun h ->
       let (P k) = Capsule.Prim.create () in
       let ( { Basement.Stdlib_shim.Modes.Many.many =
                 { Basement.Stdlib_shim.Modes.Aliased.aliased = x }
             }
           , k )
         =
         Capsule.Prim.Key.access k ~f:(fun access ->
           { Basement.Stdlib_shim.Modes.Many.many =
               { Basement.Stdlib_shim.Modes.Aliased.aliased =
                   Capsule.Prim.Data.wrap ~access (ref 4)
               }
           })
       in
       let (), _ =
         Capsule.Prim.Key.with_password k ~f:(fun password ->
           Capsule.Prim.Data.iter
             ~password
             ~f:(fun a -> Eff.Contended.perform h (Set !a))
             x [@nontail])
       in
       Printf.printf "Get: %d\n" (Eff.Contended.perform h Get)));
  [%expect {| Get: 4 |}]
;;
