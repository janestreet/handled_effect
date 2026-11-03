open! Base
open! Expect_test_helpers_base
open Portable_kernel

module%test [@tags "no-js"] _ = struct
  external poll : unit -> unit @@ portable = "%poll"

  [@@@alert "-do_not_nest"]

  [@@@alert
    "-unsafe_effects"
    (* Explicitly testing mixed stdlib and handled effects, which is forbidden by the safe
       interface due to unyielding constraints on closures *)]

  type 'a no_op = |

  module No_op = Handled_effect.Make (struct
      type 'a t = 'a no_op
    end)

  module Tick_outcome = Handled_effect.Preemptible.Tick_outcome

  type 'a preemption_action =
    | Resume
    | Handled of 'a

  let never_preempt () = Tick_outcome.Continue

  let with_tick ?(interval_usec = 10_000) f =
    Domain.Tick.with_ ~interval_usec (fun _ -> f ())
  ;;

  let run_with_tick_handler ?(interval_usec = 1_000) ?(repeating = false) ~on_preemption f
    =
    with_tick ~interval_usec (fun () ->
      let preempted_once = Atomic.make false in
      let on_tick () =
        match ~repeating, ~already_preempted:(Atomic.exchange preempted_once true) with
        | ~repeating:false, ~already_preempted:true -> Tick_outcome.Continue
        | ~repeating:false, ~already_preempted:false
        | ~repeating:true, ~already_preempted:false
        | ~repeating:true, ~already_preempted:true -> Tick_outcome.Preempt
      in
      let rec handle : _ No_op.Result.Preemptible.t @ unique -> _ = function
        | Value value -> value
        | Exception exn -> raise exn
        | Operation (_, _) -> .
        | Preemption continuation ->
          (match on_preemption () with
           | Resume ->
             handle (Handled_effect.Preemptible.continue ~on_tick continuation () [])
           | Handled value -> value)
      in
      handle (No_op.Preemptible.run ~on_tick f))
  ;;

  let fail_if_timed_out start_at ~after =
    if Float.( > ) (Stdlib.Sys.time () -. start_at) after
    then failwith [%string "Didn't get preempted after %{after#Float} seconds"]
  ;;

  let%expect_test ("basic preemption" [@tags "effects-enabled"]) =
    let preempted = Atomic.make false in
    let x = ref 0 in
    run_with_tick_handler
      ~on_preemption:(fun () ->
        Atomic.set preempted true;
        Resume)
      (fun _ ->
        let start_at = Stdlib.Sys.time () in
        while not (Atomic.get preempted) do
          poll ();
          fail_if_timed_out start_at ~after:5.;
          x := !x + 1
        done;
        require (!x > 0))
  ;;

  let%expect_test ("multiple preemptions" [@tags "effects-enabled"]) =
    let count = ref 0 in
    run_with_tick_handler
      ~repeating:true
      ~on_preemption:(fun () ->
        Int.incr count;
        Resume)
      (fun _ ->
        let start_at = Stdlib.Sys.time () in
        while !count < 5 do
          poll ();
          fail_if_timed_out start_at ~after:10.
        done);
    print_s [%sexp (count : int ref)];
    [%expect {| 5 |}]
  ;;

  let%expect_test ("rapid preemptions with allocations" [@tags "effects-enabled"]) =
    let count = ref 0 in
    let allocations = ref [] in
    let result =
      run_with_tick_handler
        ~repeating:true
        ~on_preemption:(fun () ->
          Int.incr count;
          ignore (ref !count : int ref);
          Resume)
        (fun _ ->
          let start_at = Stdlib.Sys.time () in
          while !count < 50 do
            poll ();
            fail_if_timed_out start_at ~after:10.;
            allocations := Array.create ~len:10 !count :: !allocations;
            if List.length !allocations > 100 then allocations := []
          done;
          !count)
    in
    require (result >= 50);
    print_s [%sexp (result : int)];
    [%expect {| 50 |}]
  ;;

  let%expect_test ("preemption handler can return without resuming"
    [@tags "effects-enabled"])
    =
    let result =
      run_with_tick_handler
        ~on_preemption:(fun () -> Handled "handled")
        (fun _ ->
          let start_at = Stdlib.Sys.time () in
          while true do
            poll ();
            fail_if_timed_out start_at ~after:5.
          done;
          "not reached")
    in
    print_endline result;
    [%expect {| handled |}]
  ;;

  let%expect_test ("preemption handler can raise" [@tags "effects-enabled"]) =
    let exception Handler_exn in
    let result =
      try
        run_with_tick_handler
          ~on_preemption:(fun () -> raise Handler_exn)
          (fun _ ->
            let start_at = Stdlib.Sys.time () in
            while true do
              poll ();
              fail_if_timed_out start_at ~after:5.
            done;
            0)
      with
      | Handler_exn -> -1
    in
    print_s [%sexp (result : int)];
    [%expect {| -1 |}]
  ;;

  let%expect_test ("exception raised by preemptible fiber" [@tags "effects-enabled"]) =
    let exception Test_exn of int in
    (try
       run_with_tick_handler
         ~interval_usec:1_000_000
         ~on_preemption:(fun () ->
           Stdlib.Gc.full_major ();
           Resume)
         (fun _ ->
           let start_at = Stdlib.Sys.time () in
           let counter = ref 0 in
           while true do
             poll ();
             fail_if_timed_out start_at ~after:5.;
             Int.incr counter;
             if !counter > 10_000 then raise (Test_exn !counter)
           done)
     with
     | Test_exn n ->
       require (n > 10_000);
       print_endline "caught exception");
    [%expect {| caught exception |}]
  ;;

  let%expect_test ("allocation in preemption handler" [@tags "effects-enabled"]) =
    let preempted = Atomic.make false in
    let data = ref [] in
    run_with_tick_handler
      ~on_preemption:(fun () ->
        Atomic.set preempted true;
        ignore (Array.create ~len:10 0 : int array);
        Resume)
      (fun _ ->
        let start_at = Stdlib.Sys.time () in
        while not (Atomic.get preempted) do
          poll ();
          fail_if_timed_out start_at ~after:5.;
          data := [ 1; 2; 3; 4; 5 ] :: !data;
          if List.length !data > 100 then data := []
        done;
        List.iter !data ~f:(fun l -> if List.length l <> 5 then failwith "Data corrupted"))
  ;;

  let%expect_test ("allocation survives GC in preemption handler"
    [@tags "effects-enabled"])
    =
    let preempted = Atomic.make false in
    let result =
      run_with_tick_handler
        ~on_preemption:(fun () ->
          Atomic.set preempted true;
          Stdlib.Gc.full_major ();
          Resume)
        (fun _ ->
          let start_at = Stdlib.Sys.time () in
          let arr = ref [||] in
          while not (Atomic.get preempted) do
            poll ();
            fail_if_timed_out start_at ~after:5.;
            arr := Array.init 1000 ~f:(fun i -> Random.int 10_000, ref i);
            Array.sort !arr ~compare:(fun (a, _) (b, _) -> Int.compare a b)
          done;
          !arr)
    in
    require_equal (module Int) (Array.length result) 1000
  ;;

  let%expect_test ("preemption handler performs another effect" [@tags "effects-enabled"])
    =
    let module Preemption_handler_effect = struct
      module Ops = struct
        type 'a t = Ping : string t
      end

      include Ops
      include Handled_effect.Make (Ops)
    end
    in
    let preempted = Atomic.make false in
    let rec handle_outer : _ Preemption_handler_effect.Result.t @ unique -> _ = function
      | Value value -> value
      | Exception exn -> raise exn
      | Operation (Ping, continuation) ->
        handle_outer
          (Handled_effect.continue
             continuation
             "handled effect from preemption handler"
             [])
    in
    with_tick (fun () ->
      handle_outer
        (Preemption_handler_effect.run (fun handler_effect ->
           let preempted_once = Atomic.make false in
           let on_tick () =
             match Atomic.exchange preempted_once true with
             | true -> Tick_outcome.Continue
             | false -> Tick_outcome.Preempt
           in
           let rec handle_preemptible
             :  Preemption_handler_effect.Handler.t @ local
             -> _ No_op.Result.Preemptible.t @ unique -> _
             =
             fun handler_effect -> function
             | Value value -> value
             | Exception exn -> raise exn
             | Operation (_, _) -> .
             | Preemption continuation ->
               Atomic.set preempted true;
               let message = Preemption_handler_effect.perform handler_effect Ping in
               print_endline message;
               handle_preemptible
                 handler_effect
                 (Handled_effect.Preemptible.continue
                    ~on_tick
                    continuation
                    ()
                    [ handler_effect ])
           in
           handle_preemptible
             handler_effect
             (No_op.Preemptible.run_with ~on_tick [ handler_effect ] (fun [ _; _ ] ->
                let start_at = Stdlib.Sys.time () in
                while not (Atomic.get preempted) do
                  poll ();
                  fail_if_timed_out start_at ~after:5.
                done;
                print_endline "resumed")))
        [@nontail]));
    [%expect
      {|
      handled effect from preemption handler
      resumed
      |}]
  ;;

  let%expect_test ("finalizable value survives compaction" [@tags "effects-enabled"]) =
    let preempted = Atomic.make false in
    let finalized = ref false in
    let make_finalizable () =
      let obj = ref [ 1; 2; 3; 4; 5 ] in
      Stdlib.Gc.finalise (fun _ -> finalized := true) obj;
      obj
    in
    let result =
      run_with_tick_handler
        ~on_preemption:(fun () ->
          Atomic.set preempted true;
          ignore
            (Array.init 10_000 ~f:(fun i ->
               Array.init 100 ~f:(fun j -> i, j, ref (i + j)))
             : (int * int * int ref) array array);
          Stdlib.Gc.full_major ();
          Stdlib.Gc.compact ();
          Resume)
        (fun _ ->
          let start_at = Stdlib.Sys.time () in
          let obj = make_finalizable () in
          while not (Atomic.get preempted) do
            poll ();
            fail_if_timed_out start_at ~after:5.;
            obj := List.map !obj ~f:(fun x -> x + 1)
          done;
          obj)
    in
    require_equal (module Int) (List.length !result) 5;
    Stdlib.Gc.full_major ();
    if !finalized then failwith "Large allocation: object finalized!";
    ignore (Sys.opaque_identity result : int list ref)
  ;;

  let%expect_test ("lazy value survives GC" [@tags "effects-enabled"]) =
    let preempted = Atomic.make false in
    let finalized = ref false in
    let make_finalizable v =
      let obj = ref v in
      Stdlib.Gc.finalise (fun _ -> finalized := true) obj;
      obj
    in
    let result =
      run_with_tick_handler
        ~on_preemption:(fun () ->
          Atomic.set preempted true;
          Stdlib.Gc.full_major ();
          Resume)
        (fun _ ->
          let lazy_value =
            lazy
              (let obj = make_finalizable 100 in
               let arr = Array.create ~len:100 42 in
               obj, arr)
          in
          let start_at = Stdlib.Sys.time () in
          while not (Atomic.get preempted) do
            poll ();
            fail_if_timed_out start_at ~after:5.
          done;
          lazy_value)
    in
    let obj, arr = Lazy.force result in
    require_equal (module Int) !obj 100;
    require_equal (module Int) (Array.length arr) 100;
    Stdlib.Gc.full_major ();
    ignore
      (Sys.opaque_identity (obj, arr, result)
       : int ref * int array * (int ref * int array) Lazy.t);
    if !finalized then failwith "Lazy values: object finalized!"
  ;;

  let%expect_test ("live values across preemption" [@tags "effects-enabled"]) =
    let preempted = Atomic.make false in
    let finalized_count = ref 0 in
    let make_finalizable v =
      let obj = ref v in
      Stdlib.Gc.finalise (fun _ -> Int.incr finalized_count) obj;
      obj
    in
    let result =
      run_with_tick_handler
        ~on_preemption:(fun () ->
          Atomic.set preempted true;
          Stdlib.Gc.full_major ();
          Stdlib.Gc.compact ();
          Stdlib.Gc.full_major ();
          Stdlib.Gc.compact ();
          Resume)
        (fun _ ->
          let start_at = Stdlib.Sys.time () in
          let obj1 = make_finalizable 1 in
          let obj2 = make_finalizable 2 in
          let obj3 = make_finalizable 3 in
          let arr = Array.init 1000 ~f:Fn.id in
          while not (Atomic.get preempted) do
            poll ();
            fail_if_timed_out start_at ~after:5.;
            Int.incr obj1;
            Int.incr obj2;
            Int.incr obj3;
            arr.(0) <- arr.(0) + 1
          done;
          obj1, obj2, obj3, arr)
    in
    let obj1, obj2, obj3, arr = result in
    require (!obj1 > 1);
    require (!obj2 > 2);
    require (!obj3 > 3);
    require_equal (module Int) (Array.length arr) 1000;
    Stdlib.Gc.full_major ();
    ignore
      (Sys.opaque_identity (obj1, obj2, obj3, arr, result)
       : int ref
         * int ref
         * int ref
         * int array
         * (int ref * int ref * int ref * int array));
    if !finalized_count > 0 then failwith "Live objects were finalized!"
  ;;

  type 'a counter_op =
    | Inc : int -> int counter_op
    | Dec : int -> int counter_op

  module Counter = Handled_effect.Make (struct
      type 'a t = 'a counter_op
    end)

  let%expect_test ("preemption mixed with operations" [@tags "effects-enabled"]) =
    let preempted = Atomic.make false in
    let rec handle ~on_tick : _ Counter.Result.Preemptible.t @ unique -> _ = function
      | Value value -> value
      | Exception exn -> raise exn
      | Operation (Inc n, continuation) ->
        handle
          ~on_tick
          (Handled_effect.Preemptible.continue ~on_tick continuation (n + 1) [])
      | Operation (Dec n, continuation) ->
        handle
          ~on_tick
          (Handled_effect.Preemptible.continue ~on_tick continuation (n - 50) [])
      | Preemption continuation ->
        Atomic.set preempted true;
        Stdlib.Gc.full_major ();
        handle ~on_tick (Handled_effect.Preemptible.continue ~on_tick continuation () [])
    in
    let result =
      with_tick (fun () ->
        let preempted_once = Atomic.make false in
        let on_tick () =
          match Atomic.exchange preempted_once true with
          | true -> Tick_outcome.Continue
          | false -> Tick_outcome.Preempt
        in
        handle
          ~on_tick
          (Counter.Preemptible.run ~on_tick (fun h ->
             let start_at = Stdlib.Sys.time () in
             let counter = ref 0 in
             while not (Atomic.get preempted) do
               poll ();
               fail_if_timed_out start_at ~after:5.;
               counter := Counter.perform h (Inc !counter);
               if !counter > 100 then counter := Counter.perform h (Dec !counter)
             done;
             !counter)))
    in
    require (result > 0)
  ;;

  let%expect_test ("nested try" [@tags "effects-enabled"]) =
    let preempted = Atomic.make false in
    let exception Inner_exn of int in
    let exception Outer_exn of string in
    let result =
      try
        try
          run_with_tick_handler
            ~on_preemption:(fun () ->
              Atomic.set preempted true;
              Stdlib.Gc.full_major ();
              Resume)
            (fun _ ->
              let start_at = Stdlib.Sys.time () in
              let counter = ref 0 in
              while not (Atomic.get preempted) do
                poll ();
                fail_if_timed_out start_at ~after:5.;
                Int.incr counter;
                try if !counter > 5000 then raise (Inner_exn !counter) with
                | Inner_exn n -> if n > 10_000 then raise (Outer_exn "outer")
              done;
              !counter)
        with
        | Outer_exn _ -> -1
      with
      | Failure _ -> -2
    in
    require_not_equal (module Int) result (-2)
  ;;

  let%expect_test ("stored continuation" [@tags "effects-enabled"]) =
    let preempted = Atomic.make false in
    let saved
      : (unit, int * int * int, unit) No_op.Continuation.Preemptible.t or_null
          Unique.Ref.t
      =
      Unique.Ref.make Null
    in
    let result =
      with_tick (fun () ->
        let preempted_once = Atomic.make false in
        let on_tick () =
          match Atomic.exchange preempted_once true with
          | true -> Tick_outcome.Continue
          | false -> Tick_outcome.Preempt
        in
        let handle_initial
          : (int * int * int, unit) No_op.Result.Preemptible.t @ unique -> int * int * int
          = function
          | Value value -> value
          | Exception exn -> raise exn
          | Operation (_, _) -> .
          | Preemption continuation ->
            Atomic.set preempted true;
            Unique.Ref.set saved (This continuation);
            0, 0, 0
        in
        handle_initial
          (No_op.Preemptible.run ~on_tick (fun _ ->
             let obj1 = ref 1 in
             let obj2 = ref 2 in
             let obj3 = ref 3 in
             let start_at = Stdlib.Sys.time () in
             while not (Atomic.get preempted) do
               poll ();
               fail_if_timed_out start_at ~after:5.;
               Int.incr obj1;
               Int.incr obj2;
               Int.incr obj3
             done;
             !obj1, !obj2, !obj3)))
    in
    let r1, r2, r3 = result in
    require_equal (module Int) r1 0;
    require_equal (module Int) r2 0;
    require_equal (module Int) r3 0;
    Stdlib.Gc.full_major ();
    Stdlib.Gc.compact ();
    let continuation =
      match Unique.Ref.exchange saved Null with
      | Null -> failwith "No continuation saved"
      | This continuation -> continuation
    in
    let handle_resume : _ No_op.Result.Preemptible.t @ unique -> _ = function
      | Value value -> value
      | Exception exn -> raise exn
      | Operation (_, _) -> .
      | Preemption _ -> failwith "Unexpected second preemption"
    in
    let v1, v2, v3 =
      handle_resume
        (Handled_effect.Preemptible.continue ~on_tick:never_preempt continuation () [])
    in
    require (v1 > 1);
    require (v2 > 2);
    require (v3 > 3)
  ;;

  let%expect_test ("weak references" [@tags "effects-enabled"]) =
    let preempted = Atomic.make false in
    let result =
      run_with_tick_handler
        ~on_preemption:(fun () ->
          Atomic.set preempted true;
          Stdlib.Gc.full_major ();
          Stdlib.Gc.compact ();
          Resume)
        (fun _ ->
          let start_at = Stdlib.Sys.time () in
          let obj1 = ref 100 in
          let obj2 = ref 200 in
          let obj3 = ref 300 in
          let weak = Stdlib.Weak.create 3 in
          Stdlib.Weak.set weak 0 (Some obj1);
          Stdlib.Weak.set weak 1 (Some obj2);
          Stdlib.Weak.set weak 2 (Some obj3);
          while not (Atomic.get preempted) do
            poll ();
            fail_if_timed_out start_at ~after:5.;
            Int.incr obj1;
            Int.incr obj2;
            Int.incr obj3
          done;
          obj1, obj2, obj3, weak)
    in
    Stdlib.Gc.full_major ();
    let obj1, obj2, obj3, weak = Sys.opaque_identity result in
    (match Stdlib.Weak.get weak 0, Stdlib.Weak.get weak 1, Stdlib.Weak.get weak 2 with
     | Some w1, Some w2, Some w3 ->
       require (!w1 > 100);
       require (!w2 > 200);
       require (!w3 > 300)
     | _ -> failwith "Weak references: objects were collected!");
    ignore
      (Sys.opaque_identity (obj1, obj2, obj3, weak)
       : int ref * int ref * int ref * int ref Stdlib.Weak.t)
  ;;

  let%expect_test ("resume after allocation in preemption handler"
    [@tags "effects-enabled"])
    =
    let preempted = Atomic.make false in
    let result = ref None in
    run_with_tick_handler
      ~on_preemption:(fun () ->
        Atomic.set preempted true;
        ignore (Sys.opaque_identity (ref 1) : int ref);
        ignore (Sys.opaque_identity (ref 2) : int ref);
        ignore (Sys.opaque_identity (ref 3) : int ref);
        Resume)
      (fun _ ->
        let a = ref 1 in
        let b = ref 2 in
        let c = ref 3 in
        let d = ref 4 in
        let e = ref 5 in
        let f = ref 6 in
        let g = ref 7 in
        let h = ref 8 in
        let start_at = Stdlib.Sys.time () in
        while not (Atomic.get preempted) do
          poll ();
          fail_if_timed_out start_at ~after:5.;
          a := !a + !b;
          b := !b + !c;
          c := !c + !d;
          d := !d + !e;
          e := !e + !f;
          f := !f + !g;
          g := !g + !h;
          h := !h + 1;
          if !h > 1000
          then (
            a := 1;
            b := 2;
            c := 3;
            d := 4;
            e := 5;
            f := 6;
            g := 7;
            h := 8)
        done;
        result := Some (!a, !b, !c, !d, !e, !f, !g, !h));
    match !result with
    | Some _ -> ()
    | None -> failwith "Delayed resume: FAILED - no result"
  ;;

  let%expect_test ("multiple nested preemptible fibers" [@tags "effects-enabled"]) =
    let module Eff = struct
      module [@warning "-unused-constructor"] Ops = struct
        type _ t = Was_preempted : bool t
      end

      include Handled_effect.Make (Ops)
    end
    in
    let open struct
      type 'a result =
        { ticks_received : int
        ; preempted : bool
        ; child : 'a
        }
      [@@deriving sexp_of]
    end in
    let res =
      with_tick (fun _ ->
        let run ~preempt_after f =
          let preempted = ref false in
          let ticks_received = Atomic.make 0 in
          let on_tick () : Tick_outcome.t =
            let ticks = Atomic.fetch_and_add ticks_received 1 in
            match preempt_after with
            | This preempt_after when ticks >= preempt_after -> Preempt
            | Null | This _ -> Continue
          in
          let rec handle : _ Eff.Result.Preemptible.t @ unique -> _ = function
            | Value child ->
              { child
              ; ticks_received = Atomic.get ticks_received
              ; preempted = !preempted
              }
            | Exception exn -> raise exn
            | Operation (Was_preempted, k) ->
              handle (Handled_effect.Preemptible.continue ~on_tick k !preempted [])
            | Preemption k ->
              preempted := true;
              handle (Handled_effect.Preemptible.continue ~on_tick k () [])
          in
          handle (Eff.Preemptible.run ~on_tick (fun h -> f preempted h))
        in
        run ~preempt_after:Null (fun _ _h ->
          run ~preempt_after:(This 3) (fun _ _h ->
            run ~preempt_after:(This 6) (fun preempted _h ->
              while not !preempted do
                poll ()
              done))))
    in
    print_s [%sexp (res : unit result result result)];
    (* Preemptions go outside-in *)
    [%expect
      {|
      ((ticks_received 4)
       (preempted      false)
       (child (
         (ticks_received 4)
         (preempted      false)
         (child (
           (ticks_received 3)
           (preempted      true)
           (child ()))))))
      |}]
  ;;

  let%expect_test ("preemptible fiber under nonpreemptible fiber"
    [@tags "effects-enabled"])
    =
    let ((), ~state:inner_state), ~state:outer_state =
      with_tick (fun _ ->
        let run_counter f =
          let state = ref 0 in
          let rec handle : _ Counter.Result.t @ unique -> _ = function
            | Value value -> value, ~state:!state
            | Exception exn -> raise exn
            | Operation (Inc x, k) ->
              state := !state + x;
              handle (Handled_effect.continue k !state [])
            | Operation (Dec x, k) ->
              state := !state - x;
              handle (Handled_effect.continue k !state [])
          in
          handle (Counter.run f)
        in
        let preempted = Atomic.make false in
        let run_preemptible_with h f =
          let state = ref 0 in
          let on_tick () : Tick_outcome.t =
            if Atomic.get preempted then Continue else Preempt
          in
          let rec handle : _ Counter.Result.Preemptible.t @ unique -> _ = function
            | Value value -> value, ~state:!state
            | Exception exn -> raise exn
            | Operation (Inc x, k) ->
              state := !state + x;
              handle (Handled_effect.Preemptible.continue ~on_tick k !state [ h ])
            | Operation (Dec x, k) ->
              state := !state - x;
              handle (Handled_effect.Preemptible.continue ~on_tick k !state [ h ])
            | Preemption k ->
              Atomic.set preempted true;
              handle (Handled_effect.Preemptible.continue ~on_tick k () [ h ])
          in
          handle (Counter.Preemptible.run_with ~on_tick [ h ] f) [@nontail]
        in
        run_counter (fun counter ->
          run_preemptible_with counter (fun [ inner_counter; outer_counter ] ->
            Counter.perform outer_counter (Inc 2) |> [%sexp_of: int] |> print_s;
            [%expect {| 2 |}];
            Counter.perform inner_counter (Inc 12) |> [%sexp_of: int] |> print_s;
            [%expect {| 12 |}];
            Counter.perform outer_counter (Dec 2) |> [%sexp_of: int] |> print_s;
            [%expect {| 0 |}];
            Counter.perform inner_counter (Dec 12) |> [%sexp_of: int] |> print_s;
            [%expect {| 0 |}];
            while not (Atomic.get preempted) do
              ignore (Counter.perform outer_counter (Inc 2) : int);
              ignore (Counter.perform outer_counter (Dec 2) : int)
            done;
            Counter.perform outer_counter (Inc 6) |> [%sexp_of: int] |> print_s;
            [%expect {| 6 |}];
            Counter.perform inner_counter (Inc 66) |> [%sexp_of: int] |> print_s;
            [%expect {| 66 |}];
            Counter.perform outer_counter (Dec 7) |> [%sexp_of: int] |> print_s;
            [%expect {| -1 |}];
            Counter.perform inner_counter (Dec 77) |> [%sexp_of: int] |> print_s;
            [%expect {| -11 |}])
          [@nontail]))
    in
    print_s [%message (outer_state : int) (inner_state : int)];
    [%expect
      {|
      ((outer_state -1)
       (inner_state -11))
      |}]
  ;;

  let%expect_test ("repeated preemptions while performing to a nonpreemptible handler"
    [@tags "effects-enabled"])
    =
    (* Preempt on every tick while performing to the outer handler in a tight loop, to
       maximize the chance of a preemption landing in the middle of a cross-stack perform. *)
    with_tick ~interval_usec:50 (fun () ->
      let preemptions = Atomic.make 0 in
      let on_tick () : Tick_outcome.t = Preempt in
      let run_counter f =
        let state = ref 0 in
        let rec handle : _ Counter.Result.t @ unique -> _ = function
          | Value value -> value
          | Exception exn -> raise exn
          | Operation (Inc x, k) ->
            state := !state + x;
            handle (Handled_effect.continue k !state [])
          | Operation (Dec x, k) ->
            state := !state - x;
            handle (Handled_effect.continue k !state [])
        in
        handle (Counter.run f)
      in
      let run_preemptible_with h f =
        let rec handle : _ No_op.Result.Preemptible.t @ unique -> _ = function
          | Value value -> value
          | Exception exn -> raise exn
          | Operation (_, _) -> .
          | Preemption k ->
            Atomic.incr preemptions;
            handle (Handled_effect.Preemptible.continue ~on_tick k () [ h ])
        in
        handle (No_op.Preemptible.run_with ~on_tick [ h ] f) [@nontail]
      in
      run_counter (fun counter ->
        run_preemptible_with counter (fun [ _no_op; counter ] ->
          let start_at = Stdlib.Sys.time () in
          while Atomic.get preemptions < 200 do
            fail_if_timed_out start_at ~after:20.;
            ignore (Counter.perform counter (Inc 1) : int);
            ignore (Counter.perform counter (Dec 1) : int)
          done)
        [@nontail]))
  ;;

  let%expect_test ("nonpreemptible fiber under preemptible fiber"
    [@tags "effects-enabled"])
    =
    let (), ~state =
      with_tick (fun _ ->
        let run_counter f =
          let state = ref 0 in
          let rec handle : _ Counter.Result.t @ unique -> _ = function
            | Value value -> value, ~state:!state
            | Exception exn -> raise exn
            | Operation (Inc x, k) ->
              state := !state + x;
              handle (Handled_effect.continue k !state [])
            | Operation (Dec x, k) ->
              state := !state - x;
              handle (Handled_effect.continue k !state [])
          in
          handle (Counter.run f)
        in
        let preempted = Atomic.make false in
        let run_preemptible f =
          let on_tick () : Tick_outcome.t =
            if Atomic.get preempted then Continue else Preempt
          in
          let rec handle : _ No_op.Result.Preemptible.t @ unique -> _ = function
            | Value value -> value
            | Exception exn -> raise exn
            | Operation (_, _) -> .
            | Preemption k ->
              Atomic.set preempted true;
              handle (Handled_effect.Preemptible.continue ~on_tick k () [])
          in
          handle (No_op.Preemptible.run ~on_tick f) [@nontail]
        in
        run_preemptible (fun _ ->
          run_counter (fun counter ->
            Counter.perform counter (Inc 2) |> [%sexp_of: int] |> print_s;
            [%expect {| 2 |}];
            Counter.perform counter (Dec 2) |> [%sexp_of: int] |> print_s;
            [%expect {| 0 |}];
            while not (Atomic.get preempted) do
              ignore (Counter.perform counter (Inc 2) : int);
              ignore (Counter.perform counter (Dec 2) : int)
            done;
            Counter.perform counter (Inc 6) |> [%sexp_of: int] |> print_s;
            [%expect {| 6 |}];
            Counter.perform counter (Dec 7) |> [%sexp_of: int] |> print_s;
            [%expect {| -1 |}])
          [@nontail]))
    in
    print_s [%sexp (state : int)];
    [%expect {| -1 |}]
  ;;

  let%expect_test ("throw stdlib effect past preemptible handler"
    [@tags "effects-enabled"])
    =
    let open Stdlib.Effect in
    let open Stdlib.Effect.Deep in
    let open struct
      type _ Stdlib.Effect.t +=
        | Ping : int -> int Stdlib.Effect.t
        | Pong : string Stdlib.Effect.t
    end in
    let preempted = Atomic.make false in
    try_with
      (fun () ->
        run_with_tick_handler
          ~repeating:false
          ~on_preemption:(fun () ->
            Atomic.set preempted true;
            Resume)
          (fun _ ->
            while not (Atomic.get preempted) do
              poll ();
              let pong = perform Pong in
              print_s [%sexp (pong : string)];
              [%expect {| pong |}];
              let ping = perform (Ping 1) in
              print_s [%sexp (ping : int)];
              [%expect {| 2 |}]
            done))
      ()
      { effc =
          (fun (type a) (e : a t) ->
            match e with
            | Ping x -> Some (fun (k : (a, _) continuation) -> continue k (x + 1))
            | Pong -> Some (fun (k : (a, _) continuation) -> continue k "pong")
            | _ -> None)
      }
  ;;

  let%expect_test ("preemption during fiber allocation" [@tags "effects-enabled"]) =
    (* If a preemption tick fires while [Preemptible.fiber] is capturing the new
       continuation, the preemption effect is performed on the freshly allocated (not yet
       preemptible) stack. . *)
    let ticks = Atomic.make 0 in
    let on_tick () =
      Atomic.incr ticks;
      Tick_outcome.Preempt
    in
    let rec handle_outer : _ No_op.Result.Preemptible.t @ unique -> _ = function
      | Value value -> value
      | Exception exn -> raise exn
      | Operation (_, _) -> .
      | Preemption continuation ->
        handle_outer (Handled_effect.Preemptible.continue ~on_tick continuation () [])
    in
    (* The inner fiber never requests preemption itself, but the outer fiber's [on_tick]
       can still preempt it while it runs. *)
    let rec finish_inner : _ No_op.Result.Preemptible.t @ unique -> unit = function
      | Value () -> ()
      | Exception exn -> raise exn
      | Operation (_, _) -> .
      | Preemption continuation ->
        finish_inner
          (Handled_effect.Preemptible.continue ~on_tick:never_preempt continuation () [])
    in
    let allocated =
      with_tick ~interval_usec:50 (fun () ->
        handle_outer
          (No_op.Preemptible.run ~on_tick (fun _ ->
             let allocated = ref 0 in
             (* The allocation cap keeps memory bounded when ticks are starved (e.g. on a
                heavily loaded machine). *)
             while Atomic.get ticks < 200 && !allocated < 10_000 do
               let continuation = No_op.Preemptible.fiber (fun _ () -> ()) in
               finish_inner
                 (Handled_effect.Preemptible.continue
                    ~on_tick:never_preempt
                    continuation
                    ()
                    []);
               Int.incr allocated
             done;
             !allocated)))
    in
    require (allocated > 0)
  ;;

  let%expect_test ("perform Stdlib.Effect.Preemption in preemptible fiber"
    [@tags "effects-enabled"])
    =
    let preempted = ref false in
    let rec handle : _ No_op.Result.Preemptible.t @ unique -> _ = function
      | Value () -> print_endline "returned"
      | Exception exn -> raise exn
      | Operation (_, _) -> .
      | Preemption continuation ->
        preempted := true;
        print_endline "preempted";
        handle
          (Handled_effect.Preemptible.continue ~on_tick:never_preempt continuation () [])
    in
    handle
      (No_op.Preemptible.run ~on_tick:never_preempt (fun _ ->
         Stdlib.Effect.perform Stdlib.Effect.Preemption;
         print_s [%message (!preempted : bool)]));
    [%expect
      {|
      preempted
      (!preempted true)
      returned
      |}]
  ;;
end
