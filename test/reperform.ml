type 'a op1 = E : int -> int op1

module Eff1 = Handled_effect.Make (struct
    type 'a t = 'a op1
  end)

type 'a op2 = F : unit op2 [@@warning "-37"]

module Eff2 = Handled_effect.Make (struct
    type 'a t = 'a op2
  end)

let rec nest h n =
  if n = 0
  then Eff1.perform h (E 42)
  else (
    let handle = function
      | Eff2.Value v ->
        Printf.printf " %d]\n" n;
        v
      | Eff2.Exception e ->
        Printf.printf " !%d]\n" n;
        raise e
      | Eff2.Operation (F, _) -> assert false
    in
    handle
    @@ Eff2.run_with [ h ] (fun [ _; h ] ->
      Printf.printf "[%d\n" n;
      nest h (n - 1)))
;;

let%expect_test ("reperforming effects - success case" [@tags "runtime5-only"]) =
  let rec handle = function
    | Eff1.Value v -> Printf.printf "= %d\n" v
    | Eff1.Exception e -> raise e
    | Eff1.Operation (E n, k) -> handle (Handled_effect.continue k (n + 100) [])
  in
  handle (Eff1.run (fun h -> nest h 5));
  [%expect
    {|
    [5
    [4
    [3
    [2
    [1
     1]
     2]
     3]
     4]
     5]
    = 142
    |}]
;;

let%expect_test ("reperforming effects - exception case" [@tags "runtime5-only"]) =
  let rec handle = function
    | Eff1.Value _ -> assert false
    | Eff1.Exception e -> Printf.printf "%s\n" (Printexc.to_string e)
    | Eff1.Operation (E _, k) -> handle (Handled_effect.discontinue k Not_found [])
  in
  handle (Eff1.run (fun h -> nest h 5));
  [%expect
    {|
    [5
    [4
    [3
    [2
    [1
     !1]
     !2]
     !3]
     !4]
     !5]
    Not_found
    |}]
;;
