open! Base
open Expect_test_helpers_base

module%test
  [@tags
    "no-js"
    (* The unsafe stdlib effects API doesn't work with JSOO, and the safe effect APi
       forbids mixing stdlib effects and handled_effect effects. *)]
  _ =
struct
  [@@@alert
    "-unsafe_effects"
    (* Explicitly testing mixed stdlib and handled effects, which is forbidden by the safe
       interface due to unyielding constraints on closures *)]

  module%test Without_argument = struct
    type _ Stdlib.Effect.t += Stdlib_eff : unit Stdlib.Effect.t
    type 'a op = Oxcaml_eff : unit op [@@warning "-37"]

    module Eff = Handled_effect.Make (struct
        type 'a t = 'a op
      end)

    let%expect_test ("throw stdlib effect past an oxcaml effect handler"
      [@tags "effects-enabled"])
      =
      Stdlib.Effect.Deep.try_with
        (fun () ->
          match Eff.run (fun _ -> Stdlib.Effect.perform Stdlib_eff) with
          | Value () -> ()
          | Exception _ -> failwith "Unexpected exception"
          | Operation (Oxcaml_eff, _) -> failwith "Unexpected Oxcaml_eff")
        ()
        { effc =
            (fun (type b) (e : b Stdlib.Effect.t) ->
              match e with
              | Stdlib_eff ->
                Some
                  (fun (cont : (b, _) Stdlib.Effect.Deep.continuation) ->
                    print_endline "got stdlib effect!";
                    Stdlib.Effect.Deep.continue cont ())
              | _ -> None)
        };
      [%expect {| got stdlib effect! |}]
    ;;
  end

  module%test With_argument = struct
    type _ Stdlib.Effect.t += Stdlib_eff : int Stdlib.Effect.t
    type 'a op = Oxcaml_eff : unit op [@@warning "-37"]

    module Eff = Handled_effect.Make (struct
        type 'a t = 'a op
      end)

    let%expect_test ("throw stdlib effect past an oxcaml effect handler"
      [@tags "effects-enabled"])
      =
      let res =
        Stdlib.Effect.Deep.try_with
          (fun () ->
            match Eff.run (fun _ -> Stdlib.Effect.perform Stdlib_eff + 4) with
            | Value x -> x
            | Exception _ -> failwith "Unexpected exception"
            | Operation (Oxcaml_eff, _) -> failwith "Unexpected Oxcaml_eff")
          ()
          { effc =
              (fun (type b) (e : b Stdlib.Effect.t) ->
                match e with
                | Stdlib_eff ->
                  Some
                    (fun (cont : (b, _) Stdlib.Effect.Deep.continuation) ->
                      print_endline "got stdlib effect!";
                      Stdlib.Effect.Deep.continue cont 5)
                | _ -> None)
          }
      in
      [%expect {| got stdlib effect! |}];
      print_s [%sexp (res : int)];
      [%expect {| 9 |}]
    ;;
  end
end
