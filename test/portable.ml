open Basement

module Ops = struct
  type 'a t = Yield : int -> unit t
end

module Eff = Handled_effect.Make (Ops)

type state : value mod contended portable =
  | Used
  | Cont of
      (unit, (unit, unit) Eff.Portable.Result.t, unit) Handled_effect.Continuation.t
      @@ portable

let create f =
  let cont = Eff.Portable.fiber (fun h () -> f h) in
  Unique.Atomic.make (Cont cont)
;;

exception Already_used

let next t =
  match Unique.Atomic.exchange t Used with
  | Used -> raise Already_used
  | Cont cont ->
    let res =
      match Handled_effect.continue cont () [] with
      | Eff.Portable.Value () -> None
      | Exception e -> raise e
      | Operation (Yield i, (cont : (unit, _, _) Handled_effect.Continuation.t)) ->
        Some (i, cont)
    in
    (match res with
     | None -> None
     | Some (i, cont) ->
       Unique.Atomic.set t (Cont cont);
       Some i)
;;

let%expect_test ("portable continuations" [@tags "runtime5-only"]) =
  print_endline "Starting in domain 0";
  let t =
    create (fun h ->
      for i = 1 to 10 do
        print_string "Jump! ";
        Eff.Portable.perform h (Yield i)
      done)
  in
  let rec loop () =
    let stop = Atomic.make false in
    (match
       Multicore.spawn
         (fun () ->
           (match next t with
            | None -> ()
            | Some i ->
              Printf.printf "Now in domain %d\n" i;
              loop ());
           Atomic.set stop true)
         ()
     with
     | Spawned -> ()
     | Failed ((), exn, bt) -> Printexc.raise_with_backtrace exn bt);
    while not (Atomic.get stop) do
      Stdlib_shim.Domain.cpu_relax ()
    done
  in
  loop ();
  [%expect
    {|
    Starting in domain 0
    Jump! Now in domain 1
    Jump! Now in domain 2
    Jump! Now in domain 3
    Jump! Now in domain 4
    Jump! Now in domain 5
    Jump! Now in domain 6
    Jump! Now in domain 7
    Jump! Now in domain 8
    Jump! Now in domain 9
    Jump! Now in domain 10
    |}]
;;
