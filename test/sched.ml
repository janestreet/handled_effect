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

type ('a, 'e) op =
  | Yield : (unit, 'e) op
  | Fork : (local_ 'e Handled_effect.Handler.t -> string) -> (unit, 'e) op
  | Ping : (unit, 'e) op

module Eff = Handled_effect.Make_rec (struct
    type ('a, 'e) t = ('a, 'e) op
  end)

open Eff

exception E
exception Pong

let say = print_string

let run main =
  let run_q = Uniqueue.create () in
  let enqueue k = Uniqueue.push k run_q in
  let rec dequeue () =
    if Uniqueue.is_empty run_q
    then `Finished
    else handle (Handled_effect.continue (Uniqueue.pop run_q) () [])
  and spawn f = handle (Eff.run f)
  and handle = function
    | Value "ok" ->
      say ".";
      dequeue ()
    | Value s -> failwith ("Unexpected result: " ^ s)
    | Exception E ->
      say "!";
      dequeue ()
    | Exception e -> raise e
    | Operation (Yield, k) ->
      say ",";
      enqueue k;
      dequeue ()
    | Operation (Fork f, k) ->
      say "+";
      enqueue k;
      spawn f
    | Operation (Ping, k) ->
      say "[";
      handle (Handled_effect.discontinue k Pong [])
  in
  spawn main
;;

let test h =
  say "A";
  perform
    h
    (Fork
       (fun h ->
         perform h Yield;
         say "C";
         perform h Yield;
         let handle = function
           | Value v -> v
           | Exception Pong -> say "]"
           | Exception e -> raise e
           | Operation (_, _) -> failwith "what?"
         in
         let res =
           Eff.run_with [ h ] (fun [ _; h2 ] ->
             perform h2 Ping;
             failwith "no pong?")
         in
         handle res;
         raise E));
  perform
    h
    (Fork
       (fun _ ->
         say "B";
         "ok"));
  say "D";
  perform h Yield;
  say "E";
  "ok"
;;

let%expect_test ("scheduler test" [@tags "runtime5-only"]) =
  let `Finished = run test in
  [%expect {| A+,+B.C,D,[]!E. |}]
;;
