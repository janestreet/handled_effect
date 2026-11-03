open! Base
open! Expect_test_helpers_base

module%test [@tags "no-js"] _ = struct
  [@@@alert "-do_not_nest"]

  external poll : unit -> unit = "%poll"

  module Trigger = struct
    type state =
      | Initial
      | Awaiting of (unit -> unit) Unique.Once.t
      | Signaled

    type t = state ref

    let signal t =
      match !t with
      | Signaled -> ()
      | Awaiting f ->
        (Unique.Once.take_exn f) ();
        t := Signaled
      | Initial -> t := Signaled
    ;;

    let create () = ref Initial

    let on_signal t f =
      match !t with
      | Signaled | Awaiting _ ->
        failwith "Can't register two actions for the same trigger!"
      | Initial -> t := Awaiting (Unique.Once.make f)
    ;;
  end

  module Ops = struct
    type ('a, 'e) t =
      | Yield : (unit, 'e) t
      | Sync : Trigger.t -> (unit, 'e) t
      | Spawn : ('e Handled_effect.Handler.t -> unit) -> (unit, 'e) t
  end

  module Eff = Handled_effect.Make_rec (Ops)

  exception E

  let run main =
    let run_q = Uniqueue.create () in
    let enqueue k = Uniqueue.push k run_q in
    let on_tick () : Handled_effect.Preemptible.Tick_outcome.t = Preempt in
    let rec dequeue () =
      if Uniqueue.is_empty run_q
      then ()
      else
        handle (Handled_effect.Preemptible.continue ~on_tick (Uniqueue.pop run_q) () [])
    and spawn f = handle (Eff.Preemptible.run ~on_tick f)
    and handle = function
      | Value _ -> dequeue ()
      | Exception E -> dequeue ()
      | Exception e -> raise e
      | Operation (Yield, k) ->
        enqueue k;
        dequeue ()
      | Operation (Spawn f, k) ->
        enqueue k;
        spawn f
      | Operation (Sync trigger, k) ->
        Trigger.on_signal trigger (fun () -> enqueue k);
        dequeue ()
      | Preemption k ->
        enqueue k;
        dequeue ()
    in
    spawn main
  ;;

  let spawn h f = Eff.perform h (Ops.Spawn f)
  let sync h t = Eff.perform h (Ops.Sync t)
  let yield h = Eff.perform h Ops.Yield

  let%expect_test ("two tasks" [@tags "effects-enabled"]) =
    Domain.Tick.with_ ~interval_usec:150 (fun _ ->
      run (fun h ->
        let num_finished = ref 0 in
        let finished = Trigger.create () in
        spawn h (fun h ->
          yield h;
          num_finished := !num_finished - 1;
          if !num_finished = 0 then Trigger.signal finished);
        spawn h (fun h ->
          yield h;
          num_finished := !num_finished - 1;
          if !num_finished = 0 then Trigger.signal finished);
        sync h finished))
  ;;

  let%expect_test ("three tasks" [@tags "effects-enabled"]) =
    Domain.Tick.with_ ~interval_usec:150 (fun _ ->
      run (fun h ->
        let num_finished = ref 2 in
        let finished = Trigger.create () in
        spawn h (fun h ->
          yield h;
          num_finished := !num_finished - 1;
          if !num_finished = 0 then Trigger.signal finished);
        spawn h (fun h ->
          let ping = Trigger.create () in
          spawn h (fun h ->
            sync h ping;
            raise E);
          yield h;
          num_finished := !num_finished - 1;
          if !num_finished = 0 then Trigger.signal finished);
        sync h finished))
  ;;

  let%expect_test ("uncooperative blocking" [@tags "effects-enabled"]) =
    Domain.Tick.with_ ~interval_usec:150 (fun _ ->
      run (fun h ->
        let block_uncooperatively _ =
          let f = ref 1.7 in
          for _ = 0 to 1_000_000 do
            f := !f *. 1.01;
            if Float.(!f > 100.) then f := !f *. 0.1;
            poll ()
          done;
          print_s [%sexp (!f : float)]
        in
        for _ = 0 to 8 do
          spawn h block_uncooperatively
        done));
    [%expect
      {|
      40.602515212272614
      40.602515212272614
      40.602515212272614
      40.602515212272614
      40.602515212272614
      40.602515212272614
      40.602515212272614
      40.602515212272614
      40.602515212272614
      |}]
  ;;
end
