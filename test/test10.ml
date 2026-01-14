open Handled_effect

module Ops1 = struct
  type 'a t = Peek : int t
end

module Ops2 = struct
  type 'a t = Poke : unit t [@@warning "-37"]
end

module Eff1 = Handled_effect.Make (Ops1)
module Eff2 = Handled_effect.Make (Ops2)

let a h i = Eff1.perform h Peek + Random.int i
let b h i = a h i + Random.int i
let c h i = b h i + Random.int i

let d h i =
  let rec handle = function
    | Eff2.Value result -> result
    | Eff2.Exception exn -> raise exn
    | Eff2.Operation (Poke, k) -> handle (continue k () [ h ])
  in
  Random.int i + handle (Eff2.run_with [ h ] (fun [ _; h ] -> c h i))
;;

let e i =
  let rec handle = function
    | Eff1.Value result -> result
    | Eff1.Exception exn -> raise exn
    | Eff1.Operation (Peek, k) ->
      let _, k = Continuation.get_callstack k 100 in
      handle (continue k 42 [])
  in
  Random.int i + handle (Eff1.run (fun h -> d h i))
;;

let%expect_test ("different effects" [@tags "runtime5-only"]) =
  ignore (e 1 : int);
  print_string "ok\n";
  [%expect {| ok |}]
;;
