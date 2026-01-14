module Uniqueue : sig
  type 'a t

  val create : unit -> 'a t
  val push : 'a @ once unique -> 'a t -> unit
  val pop : 'a t -> 'a @ once unique
  val is_empty : 'a t -> bool
end = struct
  type 'a t = 'a Unique.Once.t Queue.t

  let create () = Queue.create ()
  let push v t = Queue.push (Unique.Once.make v) t
  let pop t = Unique.Once.get_exn (Queue.pop t)
  let is_empty t = Queue.is_empty t
end

module OnceSeq : sig
  type 'a node
  type 'a t = unit -> 'a node @ once

  val nil : 'a node
  val cons : 'a @ once -> 'a t @ once -> 'a node @ once
  val to_dispenser : 'a t @ once -> (unit -> 'a option @ once)
end = struct
  type 'a node =
    | Nil
    | Cons of 'a * 'a t

  and 'a t = unit -> 'a node @ once

  let nil = Nil
  let cons h t = Cons (h, fun () -> t ())

  let to_dispenser t =
    let cell = Unique.Ref.make t in
    fun () ->
      match (Unique.Ref.exchange cell (fun () -> Nil)) () with
      | Nil -> None
      | Cons (x, xs) ->
        Unique.Ref.set cell xs;
        Some x
  ;;
end

open Handled_effect

type 'a op1 = Xchg : int -> int op1

module Eff1 = Handled_effect.Make (struct
    type 'a t = 'a op1
  end)

let comp1 h =
  let a = Xchg 0 in
  let x = Eff1.perform h a in
  let b = Xchg 1 in
  let y = Eff1.perform h b in
  x + y
;;

let comp2 h =
  let (_ : int) = Eff1.perform h (Xchg 0) in
  raise Not_found
;;

let comp3 h =
  let (_ : int) = Eff1.perform h (Xchg 0) in
  int_of_string "fdjsl"
;;

let handle comp =
  (* try *)
  let rec handle = function
    | Eff1.Value x -> x - 30
    | Eff1.Exception _ -> 42
    | Eff1.Operation (Xchg n, k) -> handle (continue k (n + 17) [])
  in
  Format.printf "%d@." (handle (Eff1.run comp))
;;

let%expect_test ("basic effect handling" [@tags "runtime5-only"]) =
  handle comp1;
  handle comp2;
  handle comp3;
  [%expect
    {|
    5
    42
    42
    |}]
;;

type 'a status =
  | Complete of 'a @@ aliased global
  | Suspended of
      { msg : int
      ; cont : (int, 'a, unit) Eff1.Continuation.t
      }

let handle = function
  | Eff1.Value v -> Complete v
  | Eff1.Exception e -> Basement.Stdlib_shim.raise e
  | Eff1.Operation (Xchg msg, cont) -> Suspended { msg; cont }
;;

let step (f : local_ _ -> 'a) () : 'a status = handle (Eff1.run f)

let rec run_both a b =
  match a (), b () with
  | Complete va, Complete vb -> va, vb
  | Suspended { msg = m1; cont = k1 }, Suspended { msg = m2; cont = k2 } ->
    run_both (fun () -> handle (continue k1 m2 [])) (fun () -> handle (continue k2 m1 []))
  | _ -> failwith "Improper synchronization"
;;

let comp2 h = Eff1.perform h (Xchg 21) * Eff1.perform h (Xchg 21)

let%expect_test ("bidirectional communication" [@tags "runtime5-only"]) =
  let x, y = run_both (step comp1) (step comp2) in
  Format.printf ">> %d %d" x y;
  [%expect {| >> 42 0 |}]
;;

type ('a, 't) op2 =
  | Fork : (local_ 't Handler.t -> unit) -> (unit, 't) op2
  | Yield : (unit, 't) op2
  | Xchg : int -> (int, 't) op2

module Eff2 = Handled_effect.Make_rec (struct
    type ('a, 't) t = ('a, 't) op2
  end)

let fork h f = Eff2.perform h (Fork f)
let yield h () = Eff2.perform h Yield
let xchg h v = Eff2.perform h (Xchg v)

(* A concurrent round-robin scheduler *)
let run main : unit =
  let exchanger : (int * (int, unit, unit) Eff2.Continuation.t) or_null Unique.Ref.t =
    Unique.Ref.make Null
  in
  (* waiting exchanger *)
  let run_q = Uniqueue.create () in
  (* scheduler queue *)
  let dequeue () =
    if Uniqueue.is_empty run_q
    then () (* done *)
    else (
      let task = Uniqueue.pop run_q in
      task ())
  in
  let rec enqueue
    : type a. (a, _, _) Eff2.Continuation.t @ once unique -> a @ unique -> unit
    =
    fun k v ->
    let task () = handle (continue k v []) in
    Uniqueue.push task run_q
  and handle = function
    | Eff2.Value () -> dequeue ()
    | Eff2.Exception e ->
      print_endline (Printexc.to_string e);
      dequeue ()
    | Eff2.Operation (Yield, k) ->
      enqueue k ();
      dequeue ()
    | Eff2.Operation (Fork f, k) ->
      enqueue k ();
      handle (Eff2.run f)
    | Eff2.Operation (Xchg n, k) ->
      (match Unique.Ref.exchange exchanger Null with
       | This (n', k') ->
         enqueue k' n;
         handle (continue k n' [])
       | Null ->
         Unique.Ref.set exchanger (This (n, k));
         dequeue ())
  in
  handle (Eff2.run main)
;;

let%expect_test ("concurrent scheduler" [@tags "runtime5-only"]) =
  run (fun h ->
    fork h (fun h ->
      Format.printf "[t1] Sending 0\n";
      let v = xchg h 0 in
      Format.printf "[t1] received %d\n" v);
    fork h (fun h ->
      Format.printf "[t2] Sending 1\n";
      let v = xchg h 1 in
      let () = yield h () in
      Format.printf "[t2] received %d\n" v));
  [%expect
    {|
    [t1] Sending 0
    [t2] Sending 1
    [t1] received 1
    [t2] received 0
    |}]
;;

(*****)

type 'a op3 = E : string op3
type 'a op4 = F : string op4

module Eff3 = Handled_effect.Make (struct
    type 'a t = 'a op3
  end)

module Eff4 = Handled_effect.Make (struct
    type 'a t = 'a op4
  end)

let foo h1 h2 = Eff4.perform h2 F ^ " " ^ Eff3.perform h1 E ^ " " ^ Eff4.perform h2 F

let bar h =
  let rec handle = function
    | Eff3.Value x -> x
    | Eff3.Exception e -> raise e
    | Eff3.Operation (E, k) -> handle (continue k "Coucou!" [ h ])
  in
  handle (Eff3.run_with [ h ] (fun [ h1; h2 ] -> foo h1 h2)) [@nontail]
;;

let baz () =
  let rec handle = function
    | Eff4.Value x -> x
    | Eff4.Exception e -> raise e
    | Eff4.Operation (F, k) -> handle (continue k "Hello, world!" [])
  in
  handle (Eff4.run bar)
;;

let%expect_test ("nested effect handlers" [@tags "runtime5-only"]) =
  Format.printf "%s" (baz ());
  [%expect {| Hello, world! Coucou! Hello, world! |}]
;;

let%expect_test ("discontinuation" [@tags "runtime5-only"]) =
  Format.printf
    "%s"
    (let rec handle = function
       | Eff4.Value x -> x
       | Eff4.Exception e -> raise e
       | Eff4.Operation (F, k) -> handle (Handled_effect.discontinue k Not_found [])
     in
     handle
       (Eff4.run (fun h ->
          try Eff4.perform h F with
          | Not_found -> "Discontinued")));
  [%expect {| Discontinued |}]
;;

let%expect_test ("one-shot continuations" [@tags "runtime5-only"]) =
  try
    Format.printf "%d"
    @@
    let rec handle = function
      | Eff1.Value x -> x
      | Eff1.Exception e -> raise e
      | Eff1.Operation (Xchg _, k) ->
        handle (Handled_effect.continue (Basement.Stdlib_shim.Obj.magic_unique k) 21 [])
        + handle (Handled_effect.continue (Basement.Stdlib_shim.Obj.magic_unique k) 21 [])
    in
    handle (Eff1.run (fun h -> Eff1.perform h (Xchg 0)))
  with
  | Continuation_already_resumed ->
    Format.printf "One-shot";
    [%expect {| One-shot |}]
;;

type ('a, 'p) op5 = Yield : 'p -> (unit, 'p) op5

module Eff5 = Handled_effect.Make1 (struct
    type ('a, 'p) t = ('a, 'p) op5
  end)

let invert (type a) ~(iter : local_ (a -> unit) -> unit) : a OnceSeq.t =
  fun () ->
  let rec handle = function
    | Eff5.Value () -> OnceSeq.nil
    | Eff5.Exception e -> raise e
    | Eff5.Operation (Yield v, k) -> OnceSeq.cons v (fun () -> handle (continue k () []))
  in
  handle (Eff5.run (fun h -> iter (fun x -> Eff5.perform h (Yield x)) [@nontail]))
;;

let string_iter f s =
  for i = 0 to String.length s - 1 do
    f (String.unsafe_get s i)
  done
;;

let%expect_test ("iterator" [@tags "runtime5-only"]) =
  let s = invert ~iter:(fun yield -> string_iter yield "OCaml") in
  let next = OnceSeq.to_dispenser s in
  let rec loop () =
    match next () with
    | Some c ->
      Format.printf "%c" c;
      loop ()
    | None -> ()
  in
  loop ();
  [%expect {| OCaml |}]
;;

type 'a op6 =
  | Send : int -> unit op6
  | Recv : int op6

module Eff6 = Handled_effect.Make (struct
    type 'a t = 'a op6
  end)

let run comp =
  let rec handle_send = function
    | Eff6.Value x -> x
    | Eff6.Exception e -> raise e
    | Eff6.Operation (Send n, k) -> handle_recv n (continue k () [])
    | Eff6.Operation (Recv, _) -> failwith "protocol violation"
  and handle_recv n = function
    | Eff6.Value x -> x
    | Eff6.Exception e -> raise e
    | Eff6.Operation (Recv, k) -> handle_send (continue k n [])
    | Eff6.Operation (Send _, _) -> failwith "protocol violation"
  in
  handle_send (Eff6.run comp)
;;

let%expect_test ("send-receive protocol" [@tags "runtime5-only"]) =
  run (fun h ->
    Format.printf "Send 42\n";
    Eff6.perform h (Send 42);
    Format.printf "Recv: %d\n" (Eff6.perform h Recv);
    Format.printf "Send 43\n";
    Eff6.perform h (Send 43);
    Format.printf "Recv: %d" (Eff6.perform h Recv));
  [%expect
    {|
    Send 42
    Recv: 42
    Send 43
    Recv: 43
    |}]
;;
