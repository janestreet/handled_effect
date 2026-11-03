open Base
include Handled_effect_intf

module Handler_index : sig @@ portable
  (** [(es1, es2) t] represents an index into the effect list [es2]. [es1] is the tail
      sublist containing the index and all following effects. For instance,
      [('b * ('c * unit), 'a * ('b * ('c * unit)) t] represents effect 1, ['b].

      It is used for building [Raw_handler.t]s, which represent one of the effects handled
      by a particular handler. *)
  type ('es1, 'es2) t : immediate

  (** [zero] is the index of the first element of an effect list. *)
  val zero : ('es, 'es) t

  (** [zero] is the index of the second element of an effect list. *)
  val one : ('es, 'e * 'es) t

  (** [succ t] is the index of the next element of an effect list after [t]. *)
  val succ : ('e * 'es1, 'es2) t -> ('es1, 'es2) t

  (** [is_zero t] tests if [t] is the index of the first element of an effect list. If it
      is then we learn that the remaining list is equal to the full list. *)
  val is_zero : ('es1, 'es2) t -> ('es1, 'es2) Type.eq option

  (** [weaken t] is an index for [es], where [t] is an index for [e * es]. If [t] is
      [zero] then [weaken t] is an invalid index and does not correspond to an element in
      [es]. *)
  val weaken : ('es1, 'e * 'es2) t -> ('es1, 'es2) t

  (** [to_int t] is the integer representation of [t]. *)
  val to_int : ('es1, 'es2) t -> int
end = struct
  type ('es1, 'es2) t = int

  let zero = 0
  let one = 1
  let succ t = t + 1

  let is_zero (type es1 es2) (t : (es1, es2) t) =
    if Int.equal t 0
    then (Obj.magic (Some Type.Equal) : (es1, es2) Type.eq option)
    else None
  ;;

  let weaken t = t - 1
  let to_int t = t
end

module Raw_handler : sig @@ portable
  (** [(e, es) t] is a handler for the effect [e], which must be an element of the effect
      list [es].

      It is used to represent one of the effects handled by a particular handler. *)
  type ('e, 'es) t : immediate

  (** [of_index i] is the handler corresponding to index [t]. *)
  val of_index : ('e * _, 'es) Handler_index.t -> ('e, 'es) t

  (** [zero] is [of_index (Handler_index.zero)]. *)
  val zero : ('e, 'e * _) t

  (** [is_zero t] tests if this handler is [zero]. If it is we learn that the handled
      effect is the first effect in the list. *)
  val is_zero : ('e1, 'e2 * _) t -> ('e1, 'e2) Type.eq option

  (** [weaken t] is a handler from the list [es], where [t] is a handler from the list
      [e * es]. If [t] is [zero] then [weaken t] is an invalid handler (i.e. not actually
      in the list). *)
  val weaken : ('e1, 'e2 * 'es2) t -> ('e1, 'es2) t

  (** [to_int t] is the integer representation of [t]. *)
  val to_int : ('e, 'es) t -> int
end = struct
  type ('e, 'es) t = Raw_handler : ('e * _, 'es) Handler_index.t -> ('e, 'es) t
  [@@unboxed]

  let of_index i = Raw_handler i
  let zero = Raw_handler Handler_index.zero

  let is_zero (type e1 e2 es) (Raw_handler i : (e1, e2 * es) t) : (e1, e2) Type.eq option =
    match Handler_index.is_zero i with
    | Some Equal as eq -> eq
    | None as eq -> eq
  ;;

  let weaken (Raw_handler i) = Raw_handler (Handler_index.weaken i)
  let to_int (Raw_handler i) = Handler_index.to_int i
end

module Yielding = struct
  (* The JSOO backend uses the yielding-ness of function applications (a function
     application is unyielding if the function and all its arguments are at mode
     unyielding) to direct-call from within CPS. In order for this to be accurate, all
     functions which might perform a free effect must either be yielding, or take a
     yielding argument. Throughout this module, we use this marker type to ensure that. It
     is important that any function application which might eventually perform an effect
     (including [%perform], but also [%continue], [%discontinue], and
     [%discontinue_with_backtrace], since the resumed fiber might itself perform more
     effects) takes either a [Yielding.t] directly, or an argument which contains a
     [Yielding.t] *)
  type t : void mod external_ forkable global immutable many stateless
end

module Handler : sig @@ portable
  (** [e t] is a handler for the effect [e]. *)
  type 'e t0 = private ..

  (** Heap-allocated handler. *)
  type 'e t_u =
    #{ h : 'e t0 @@ global
     ; yielding : Yielding.t
     }

  type 'e t : value non_float

  val%template unbox : 'e t @ l yielding -> 'e t_u @ l yielding
  [@@mode l = (local, global)]

  val box : 'e t_u @ yielding -> 'e t @ yielding
  val globalize : 'e t @ local yielding -> 'e t @ yielding
  val make : 'e t0 -> 'e t @ yielding

  module List : sig
    include Handler_list with type 'e handler := 'e t

    val yielding : 'es t @ local -> Yielding.t @ yielding
  end

  include Handler with type 'e t := 'e t and module List := List

  type _ t0 += Dummy : 'a t0

  module type Create = sig
    type e
    type es
    type 'e t0 += C : ('e, e * es) Raw_handler.t -> 'e t0

    (** [initial ~length] is a list of handlers for effects [es], where [length] is the
        length of [es]. *)
    val initial : length:es List.Length.t @ local -> es List.t

    (** [initial_from hs] is a list of handlers for effect [es], where [hs] is another
        list of handlers for effects [es]. These handlers are selected from the effect
        list [e * es] for some effect [e]. Note that [hs] is being used only for its
        length -- the actual handlers in it do not affect the output. *)
    val initial_from : es List.t @ local -> es List.t
  end

  val create : unit -> (module Create with type e = 'e and type es = 'es)
end = struct
  type 'e t0 = ..

  type 'e t_u =
    #{ h : 'e t0 @@ global
     ; yielding : Yielding.t
     }

  type 'e t = { h : 'e t0 @@ global } [@@unboxed]

  external magic_mk_yielding : unit -> Yielding.t @ yielding @@ stateless = "%unbox_unit"

  let%template unbox { h } =
    #{ h; yielding = magic_mk_yielding () }
    [@exclave_if_local l ~reasons:[ Will_return_unboxed ]]
  [@@mode l = (local, global)]
  ;;

  let box (#{ h; _ } : _ t_u) : _ t = { h }
  let globalize { h } = { h }
  let make h = { h }

  type _ t0 += Dummy : 'a t0

  module List = struct
    type 'e handler = 'e t

    type 'es t =
      | [] : unit t
      | ( :: ) : 'e handler * 'es t -> ('e * 'es) t

    module Length = struct
      type x = X

      type 'es t =
        | [] : unit t
        | ( :: ) : x * 'es t -> ('e * 'es) t
    end

    let length t =
      let rec loop : type es. es t @ local -> es Length.t = function
        | [] -> []
        | _ :: rest -> X :: loop rest
      in
      loop t [@nontail]
    ;;

    let yielding (_ : _ t @ local) = magic_mk_yielding ()
  end

  module type Create = sig
    type e
    type es
    type 'e t0 += C : ('e, e * es) Raw_handler.t -> 'e t0

    val initial : length:es List.Length.t @ local -> es List.t
    val initial_from : es List.t @ local -> es List.t
  end

  let[@inline] create (type e es) () : (module Create with type e = e and type es = es) =
    let module Create = struct
      type nonrec e = e
      type nonrec es = es
      type 'e t0 += C : ('e, e * es) Raw_handler.t -> 'e t0

      let initial ~(length : es List.Length.t) : es List.t =
        let rec loop
          : type esr.
            (esr, e * es) Handler_index.t -> esr List.Length.t @ local -> esr List.t
          =
          fun i l ->
          match l with
          | [] -> []
          | X :: l' ->
            let h = Raw_handler.of_index i in
            { h = C h } :: loop (Handler_index.succ i) l'
        in
        loop Handler_index.one length [@nontail]
      ;;

      let initial_from (t : es List.t @ local) : es List.t =
        let rec loop
          : type esr. (esr, e * es) Handler_index.t -> esr List.t @ local -> esr List.t
          =
          fun i l ->
          match l with
          | [] -> []
          | _ :: rest ->
            let h = Raw_handler.of_index i in
            { h = C h } :: loop (Handler_index.succ i) rest
        in
        loop Handler_index.one t [@nontail]
      ;;
    end
    in
    (module Create)
  ;;
end

module Mapping : sig @@ portable
  (** [es t] represents a mutable mapping from an effect in [es] to its handler. *)
  type 'es t

  (** [lookup h t] looks up the effect handled by [h] in mapping [t] and returns its
      handler. *)
  val lookup : ('e, 'es) Raw_handler.t @ local -> 'es t -> 'e Handler.t @ yielding

  (** [empty] is the mapping for the empty effect list. *)
  val empty : unit -> unit t

  (** [create hs] creates a new mapping from [es], where [hs] is a list of handlers for
      the effects [es]. Its initial value is to map each effect in [es] to the
      corresponding handler in [hs]. *)
  val create : 'es Handler.List.t @ local -> 'es t

  (** [set hs t] updates the mapping [t] to map each effect handled by [hs] to its
      corresponding handler in [hs]. *)
  val set : 'es Handler.List.t @ local -> 'es t -> unit

  (** [unsafe_create_uninitialized len] creates a new uninitialized mapping from [es],
      where [len] is the length of [es]. The mapping must be initialized with [set] before
      it is used. *)
  val unsafe_create_uninitialized : 'es Handler.List.Length.t @ local -> 'es t
end = struct
  type element = P : 'e Handler.t0 -> element [@@unboxed]
  type 'es1 t = element or_null array

  let lookup (type e es) (h : (e, es) Raw_handler.t) (t : es t) =
    let (P elt) = Or_null.unsafe_value (Array.unsafe_get t (Raw_handler.to_int h)) in
    Handler.make (Obj.magic (elt : _ Handler.t0) : e Handler.t0)
  ;;

  let empty () = [||]

  let make (type es) (idx : (unit, es) Handler_index.t) : es t =
    Array.create ~len:(Handler_index.to_int idx) Null
  ;;

  let set_element
    (type e esr es)
    (t : es t)
    (idx : (e * esr, es) Handler_index.t)
    (h : e Handler.t @ local)
    =
    let elt : element = P ((Handler.unbox [@mode local]) h).#h in
    Array.unsafe_set t (Handler_index.to_int idx) (This elt)
  ;;

  let create (type es) (l : es Handler.List.t @ local) =
    let rec loop
      : type esr. (esr, es) Handler_index.t -> esr Handler.List.t @ local -> es t
      =
      fun idx l ->
      match l with
      | [] -> make idx
      | h :: rest ->
        let t = loop (Handler_index.succ idx) rest in
        set_element t idx h;
        t
    in
    loop Handler_index.zero l [@nontail]
  ;;

  let set (type es) (hs : es Handler.List.t @ local) (t : es t) =
    let rec loop
      : type esr. (esr, es) Handler_index.t -> esr Handler.List.t @ local -> es t -> unit
      =
      fun idx hs t ->
      match hs with
      | [] -> ()
      | h :: rest ->
        set_element t idx h;
        loop (Handler_index.succ idx) rest t
    in
    loop Handler_index.zero hs t [@nontail]
  ;;

  let unsafe_create_uninitialized (type es) (l : es Handler.List.Length.t @ local) =
    let rec loop
      : type esr. (esr, es) Handler_index.t -> esr Handler.List.Length.t @ local -> es t
      =
      fun idx l ->
      match l with
      | [] -> make idx
      | X :: l' -> loop (Handler_index.succ idx) l'
    in
    loop Handler_index.zero l [@nontail]
  ;;
end

type ('a, 'e) op

type ('a, 'e) perform =
  (* It is important that this tuple is in this order! This type has to be [Obj.magic]able
     from Stdlib.Effect, or else throwing a stdlib effect past an oxcaml handler can cause
     a segfault. Locality prevents the inverse from happening. *)
  'e Handler.t * ('a, 'e) op

(* [perform_] is able to return a unique value because [continue] is required to provide a
   unique value. *)
external perform_
  :  ('a, 'e) perform @ yielding
  -> 'a @ once unique
  @@ portable
  = "%perform"

let perform_ p = perform_ p [@nontail]

(* A last_fiber is a tagged pointer, so does not keep the fiber alive. It must never be
   the sole reference to the fiber, and is only used to cache the final fiber in the
   linked list formed by [cont.fiber->parent]. *)
type last_fiber : immediate
type (-'a, +'b) cont : value mod many

external get_cont_callstack
  :  ('a, 'b) cont @ local
  -> int
  -> Backtrace.t
  @@ portable
  = "caml_get_continuation_callstack"

external cont_set_last_fiber
  :  ('a, 'b) cont @ local
  -> last_fiber
  -> unit
  @@ portable
  = "%setfield1"

type 'b effc =
  { effc : 'o 'e. ('o, 'e) perform -> ('o, 'b) cont @ unique -> last_fiber -> 'b @ unique
  }
[@@unboxed]

module Prim = struct
  external with_stack
    :  ('a -> 'b)
    -> (exn -> 'b)
    -> 'b effc
    -> ('c -> 'a) @ once yielding
    -> 'c @ yielding
    -> 'b @ unique
    @@ portable
    = "%with_stack"

  external with_stack_preemptible
    :  ('a -> 'b)
    -> (exn -> 'b)
    -> 'b effc
    -> (unit -> Tick_outcome.t) @ portable
    -> ('c -> 'a) @ once yielding
    -> 'c @ yielding
    -> 'b @ unique
    @@ portable
    = "%with_stack_preemptible"

  external continue
    :  ('a, 'b) cont @ unique
    -> 'a @ once unique
    -> 'b @ once unique
    @@ portable
    = "%continue"

  external discontinue
    :  ('a, 'b) cont @ unique
    -> exn
    -> 'b @ once unique
    @@ portable
    = "%discontinue"

  external discontinue_with_backtrace
    :  ('a, 'b) cont @ unique
    -> exn
    -> Backtrace.t
    -> 'b @ once unique
    @@ portable
    = "%discontinue_with_backtrace"

  external update_cont_tick_handler
    :  ('a, 'b) cont @ unique
    -> (unit -> Tick_outcome.t) or_null @ portable
    -> ('a, 'b) cont @ unique
    @@ portable
    = "caml_continuation_update_tick_handler_noexc"
  [@@noalloc]
end

let with_stack valuec exnc effc f x = Prim.with_stack valuec exnc effc f x

let with_stack_preemptible valuec exnc effc tickc f x =
  Prim.with_stack_preemptible valuec exnc effc tickc f x
;;

let continue cont v = Prim.continue cont v
let discontinue cont e = Prim.discontinue cont e
let discontinue_with_backtrace cont e bt = Prim.discontinue_with_backtrace cont e bt

type (+'a, 'es) r =
  | Val : 'a @@ global many -> ('a, 'es) r
  | Exn : exn @@ global many -> ('a, 'es) r
  | Op :
      ('o, 'e) op @@ global many
      * ('e, 'es) Raw_handler.t
      * ('o, ('a, 'es) r) cont
      * last_fiber
      -> ('a, 'es) r

module Preemptible0 = struct
  type (+'a, 'es) r =
    | Val : 'a @@ global many -> ('a, 'es) r
    | Exn : exn @@ global many -> ('a, 'es) r
    | Op :
        ('o, 'e) op @@ global many
        * ('e, 'es) Raw_handler.t
        * ('o, ('a, 'es) r) cont
        * last_fiber
        -> ('a, 'es) r
    | Preemption : (unit, ('a, 'es) r) cont * last_fiber -> ('a, 'es) r

  let valuec v = Val v
  let exnc e = Exn e
end

let valuec v = Val v
let exnc e = Exn e

external reperform
  :  ('a, 'e) perform @ yielding
  -> ('a, 'b) cont @ unique
  -> last_fiber
  -> 'b @ unique
  @@ portable
  = "%reperform"

let reperform (_ : Yielding.t @ local yielding) p k lf = reperform p k lf

let alloc_cont
  (type a b h e es)
  (module H : Handler.Create with type e = e and type es = es)
  (f : (h @ local -> a @ once unique -> b) @ once)
  (h : h)
  : (a, (b, e * es) r) cont
  =
  let exception Ready__ of (a, (b, e * es) r) cont in
  let effc
    (type o eh)
    ((h, op) as perf : (o, eh) perform)
    (k : (o, (b, e * es) r) cont)
    last_fiber
    =
    match (Handler.unbox h).#h with
    | H.C h -> Op (op, h, k, last_fiber)
    | Handler.Dummy ->
      let k = (Obj.magic k : (a, (b, e * es) r) cont) in
      cont_set_last_fiber k last_fiber;
      (* Note that [k] is still actually unique here. *)
      Basement.Stdlib_shim.raise_notrace (Ready__ k)
    | _ -> reperform (Handler.unbox h).#yielding perf k last_fiber [@nontail]
  in
  let dummy_op : (a, e) op = Obj.magic () in
  match
    with_stack
      valuec
      exnc
      { effc }
      (fun () ->
        let p = Handler.make Handler.Dummy, dummy_op in
        f h (perform_ p))
      ()
  with
  | _ -> assert false
  (* [Ready__ k] is only ever raised once with an unique [k]. However, raised exceptions
     must have the legacy mode, so we can't get rid of [magic_unique] here. *)
  | exception Ready__ k -> Obj.magic_unique k
;;

let run_stack
  (type a h e es)
  (module H : Handler.Create with type e = e and type es = es)
  (f : (h @ local -> a) @ once)
  (h : h @ yielding)
  : (a, e * es) r
  =
  let effc ((h, op) as perf) k last_fiber =
    let #{ Handler.h; yielding } = Handler.unbox h in
    match h with
    | H.C h -> Op (op, h, k, last_fiber)
    | _ -> reperform yielding perf k last_fiber [@nontail]
  in
  with_stack valuec exnc { effc } f h
;;

let alloc_cont_preemptible
  (type a b h e es)
  (module H : Handler.Create with type e = e and type es = es)
  (f : (h @ local -> a @ once unique -> b) @ once)
  (h : h)
  : (a, (b, e * es) Preemptible0.r) cont
  =
  let exception Ready__ of (a, (b, e * es) Preemptible0.r) cont in
  let effc
    (type o eh)
    ((h, op) as perf : (o, eh) perform)
    (k : (o, (b, e * es) Preemptible0.r) cont)
    last_fiber
    : _ @ unique
    =
    if phys_equal (Obj.repr perf) (Obj.repr Stdlib.Effect.Preemption)
    then
      Preemptible0.Preemption
        ( ((Obj.magic [@mode unique]) k : (unit, (b, e * es) Preemptible0.r) cont)
        , last_fiber )
    else (
      match Handler.unbox h with
      | #{ h = H.C h; _ } -> Op (op, h, k, last_fiber)
      | #{ h = Handler.Dummy; _ } ->
        let k = (Obj.magic k : (a, (b, e * es) Preemptible0.r) cont) in
        cont_set_last_fiber k last_fiber;
        Basement.Stdlib_shim.raise_notrace (Ready__ k)
      | #{ yielding; _ } -> reperform yielding perf k last_fiber)
  in
  let dummy_op : (a, e) op = Obj.magic () in
  let p = Handler.make Handler.Dummy, dummy_op in
  let rec capture (run : (unit -> (b, e * es) Preemptible0.r @ unique) @ once)
    : _ @ unique
    =
    match run () with
    | Preemption (k, last_fiber) ->
      cont_set_last_fiber (borrow_ k) last_fiber;
      capture (fun () -> continue k ())
    | Val _ | Exn _ | Op _ -> assert false
    | exception Ready__ k -> Obj.magic_unique k
  in
  capture (fun () ->
    with_stack
      Preemptible0.valuec
      Preemptible0.exnc
      { effc }
      (fun p -> f h (perform_ p))
      p)
;;

let run_stack_preemptible
  (type a h e es)
  (module H : Handler.Create with type e = e and type es = es)
  (f : (h @ local -> a) @ once)
  (h : h)
  ~on_tick
  : (a, e * es) Preemptible0.r
  =
  let effc ((h, op) as perf) k last_fiber =
    let #{ Handler.h; yielding } = Handler.unbox h in
    if phys_equal (Obj.repr perf) (Obj.repr Stdlib.Effect.Preemption)
    then
      Preemptible0.Preemption
        ( ((Obj.magic [@mode unique]) k : (unit, (a, e * es) Preemptible0.r) cont)
        , last_fiber )
    else (
      match h with
      | H.C h -> Op (op, h, k, last_fiber)
      | _ -> reperform yielding perf k last_fiber)
  in
  with_stack_preemptible Preemptible0.valuec Preemptible0.exnc { effc } on_tick f h
;;

type (-'a, +'b, 'e, 'es) continuation : value mod many =
  | Cont :
      { cont : ('a, ('b, 'e * 'es) r) cont
      ; mapping : 'es Mapping.t @@ global many
      }
      -> ('a, 'b, 'e, 'es) continuation

type (-'a, +'b, 'e, 'es) preemptible_continuation : value mod many =
  | Preemptible_cont :
      { cont : ('a, ('b, 'e * 'es) Preemptible0.r) cont
      ; mapping : 'es Mapping.t @@ global many
      }
      -> ('a, 'b, 'e, 'es) preemptible_continuation

type ('a, 'e, 'es) res =
  | Value : 'a @@ global many -> ('a, 'e, 'es) res
  | Exception : exn @@ global many -> ('a, 'e, 'es) res
  | Operation :
      ('o, 'e) op @@ global many * ('o, 'a, 'e, 'es) continuation
      -> ('a, 'e, 'es) res

module Preemptible_res = struct
  type ('a, 'e, 'es) t =
    | Value : 'a @@ global many -> ('a, 'e, 'es) t
    | Exception : exn @@ global many -> ('a, 'e, 'es) t
    | Operation :
        ('o, 'e) op @@ global many * ('o, 'a, 'e, 'es) preemptible_continuation
        -> ('a, 'e, 'es) t
    | Preemption : (unit, 'a, 'e, 'es) preemptible_continuation -> ('a, 'e, 'es) t
end

let get_callstack (Cont { cont; _ }) i = get_cont_callstack cont i

(* [handle] takes a [yielding] witness so that the recursive call below (and [handle]'s
   call sites) are seen as able to perform: forwarding an effect to an outer handler
   reperforms, so these calls must not be compiled in direct style (see
   [Handler.unbox]/[Yielding]). The witness is threaded from the handlers held by
   [handle]'s callers, never summoned here. *)
let rec handle
  : type a e es.
    Yielding.t @ local yielding
    -> es Mapping.t
    -> (a, e * es) r @ once unique
    -> (a, e, es) res @ once unique
  =
  fun yielding mapping -> function
  | Val x -> Value x
  | Exn e -> Exception e
  | Op (op, handler, k, last_fiber) ->
    (match Raw_handler.is_zero handler with
     | Some Equal ->
       cont_set_last_fiber (borrow_ k) last_fiber;
       Operation (op, Cont { cont = k; mapping })
     | None ->
       let handler = Raw_handler.weaken handler in
       let fwd = Mapping.lookup handler mapping |> Handler.unbox in
       (* the bytecode compiler requires [reperform] to be in tail position *)
       let result = reperform yielding (Handler.box fwd, op) k last_fiber [@nontail] in
       handle yielding mapping result)
;;

let rec handle_preemptible
  : type a e es.
    Yielding.t @ local yielding
    -> es Mapping.t
    -> (a, e * es) Preemptible0.r @ once unique
    -> (a, e, es) Preemptible_res.t @ once unique
  =
  fun yielding mapping -> function
  | Val x -> Value x
  | Exn e -> Exception e
  | Op (op, handler, k, last_fiber) ->
    (match Raw_handler.is_zero handler with
     | Some Equal ->
       cont_set_last_fiber (borrow_ k) last_fiber;
       Operation (op, Preemptible_cont { cont = k; mapping })
     | None ->
       let handler = Raw_handler.weaken handler in
       let fwd = Mapping.lookup handler mapping in
       let result = (fun () -> reperform yielding (fwd, op) k last_fiber) () in
       handle_preemptible yielding mapping result)
  | Preemption (k, last_fiber) ->
    cont_set_last_fiber (borrow_ k) last_fiber;
    Preemption (Preemptible_cont { cont = k; mapping })
[@@inline]
;;

let continue_preemptible ~on_tick (Preemptible_cont { cont; mapping }) v handlers =
  let update_and_continue cont on_tick v =
    Prim.continue (Prim.update_cont_tick_handler cont on_tick) v
      [@@inline never]
  in
  Mapping.set handlers mapping;
  handle_preemptible
    (Handler.List.yielding handlers)
    mapping
    (update_and_continue cont (This on_tick) v)
;;

let continue (Cont { cont; mapping }) v handlers =
  Mapping.set handlers mapping;
  handle (Handler.List.yielding handlers) mapping (continue cont v)
;;

let discontinue (Cont { cont; mapping }) e handlers =
  Mapping.set handlers mapping;
  handle (Handler.List.yielding handlers) mapping (discontinue cont e)
;;

let discontinue_with_backtrace (Cont { cont; mapping }) e bt handlers =
  Mapping.set handlers mapping;
  handle (Handler.List.yielding handlers) mapping (discontinue_with_backtrace cont e bt)
;;

let fiber (type a b e) (f : (e Handler.t @ local -> a @ once unique -> b) @ once) =
  let module H =
    (val Handler.create () : Handler.Create with type e = e and type es = unit)
  in
  let handler : (e, e * unit) Raw_handler.t = Raw_handler.zero in
  let handler : e Handler.t = Handler.make (H.C handler) in
  let mapping = Mapping.empty () in
  let cont = alloc_cont (module H) f handler in
  Cont { cont; mapping }
;;

let fiber_with
  (type a b e es)
  (l : es Handler.List.Length.t @ local)
  (f : ((e * es) Handler.List.t @ local -> a @ once unique -> b) @ once)
  =
  let module H = (val Handler.create () : Handler.Create with type e = e and type es = es)
  in
  let handler : (e, e * es) Raw_handler.t = Raw_handler.zero in
  let handlers : (e * es) Handler.List.t =
    Handler.make (H.C handler) :: H.initial ~length:l
  in
  let mapping = Mapping.unsafe_create_uninitialized l in
  let cont = alloc_cont (module H) f handlers in
  Cont { cont; mapping }
;;

let run (type a e) (f : (e Handler.t @ local -> a) @ once) =
  let module H =
    (val Handler.create () : Handler.Create with type e = e and type es = unit)
  in
  let handler : (e, e * unit) Raw_handler.t = Raw_handler.zero in
  let handler : e Handler.t = Handler.make (H.C handler) in
  let res = run_stack (module H) f handler in
  handle (Handler.unbox handler).#yielding (Mapping.empty ()) res
;;

let run_with
  (type a e es)
  (hs : es Handler.List.t @ local)
  (f : ((e * es) Handler.List.t @ local -> a) @ once)
  =
  let module H = (val Handler.create () : Handler.Create with type e = e and type es = es)
  in
  let handler : (e, e * es) Raw_handler.t = Raw_handler.zero in
  let head = Handler.make (H.C handler) in
  let handlers : (e * es) Handler.List.t = head :: H.initial_from hs in
  let mapping = Mapping.create hs in
  let res = run_stack (module H) f handlers in
  handle (Handler.unbox head).#yielding mapping res
;;

let run_preemptible (type a e) ~on_tick (f : (e Handler.t @ local -> a) @ once) =
  let module H =
    (val (Handler.create () : (module Handler.Create with type e = e and type es = unit)))
  in
  let handler : (e, e * unit) Raw_handler.t = Raw_handler.zero in
  let handler : e Handler.t = Handler.make (H.C handler) in
  let res = run_stack_preemptible (module H) f handler ~on_tick in
  handle_preemptible (Handler.unbox handler).#yielding (Mapping.empty ()) res
;;

let preemptible_fiber
  (type a b e)
  (f : (e Handler.t @ local -> a @ once unique -> b) @ once)
  =
  let module H =
    (val (Handler.create () : (module Handler.Create with type e = e and type es = unit)))
  in
  let handler : (e, e * unit) Raw_handler.t = Raw_handler.zero in
  let handler : e Handler.t = Handler.make (H.C handler) in
  let mapping = Mapping.empty () in
  let cont = alloc_cont_preemptible (module H) f handler in
  Preemptible_cont { cont; mapping }
;;

let preemptible_fiber_with
  (type a b e es)
  (l : es Handler.List.Length.t @ local)
  (f : ((e * es) Handler.List.t @ local -> a @ once unique -> b) @ once)
  =
  let module H =
    (val (Handler.create () : (module Handler.Create with type e = e and type es = es)))
  in
  let handler : (e, e * es) Raw_handler.t = Raw_handler.zero in
  let head = Handler.make (H.C handler) in
  let handlers : (e * es) Handler.List.t = head :: H.initial ~length:l in
  let mapping = Mapping.unsafe_create_uninitialized l in
  let cont = alloc_cont_preemptible (module H) f handlers in
  Preemptible_cont { cont; mapping }
;;

let run_with_preemptible
  (type a e es)
  ~on_tick
  (hs : es Handler.List.t @ local)
  (f : ((e * es) Handler.List.t @ local -> a) @ once)
  =
  let module H =
    (val (Handler.create () : (module Handler.Create with type e = e and type es = es)))
  in
  let handler : (e, e * es) Raw_handler.t = Raw_handler.zero in
  let head = Handler.make (H.C handler) in
  let handlers : (e * es) Handler.List.t = head :: H.initial_from hs in
  let mapping = Mapping.create hs in
  let res = run_stack_preemptible (module H) f handlers ~on_tick in
  handle_preemptible (Handler.unbox head).#yielding mapping res
;;

(* Data-race-free makepers around [fiber]/[run] functions.

   Uses [Obj.magic_portable] to avoid duplicating implementations of
   [alloc_cont]/[run_stack] in absence of mode polymorphism.

   Makepers provide the handler at [portable], but require [portable] arguments and
   operations, which are marked as [contended]. *)
module DRF : sig @@ portable
  val fiber
    : ('a : value mod portable) 'b 'e.
    ('e Handler.t @ local portable -> 'a @ contended once unique -> 'b) @ once
    -> ('a, 'b, 'e, unit) continuation @ unique

  val fiber_with
    : ('a : value mod portable) 'b 'e 'es.
    'es Handler.List.Length.t @ local
    -> (('e * 'es) Handler.List.t @ local portable -> 'a @ contended once unique -> 'b)
       @ once
    -> ('a, 'b, 'e, 'es) continuation @ unique

  val run
    :  ('e Handler.t @ local portable -> 'a) @ once
    -> ('a, 'e, unit) res @ once unique
  (* Returns a [res] to be [Obj.magic]ed into the contended result type with
     [op @@ contended]. *)

  val run_with
    :  'es Handler.List.t @ local portable
    -> (('e * 'es) Handler.List.t @ local portable -> 'a) @ once
    -> ('a, 'e, 'es) res @ once unique
  (* Returns a [res] to be [Obj.magic]ed into the contended result type with
     [op @@ contended]. *)

  module Preemptible : sig
    val fiber
      : ('a : value mod portable) 'b 'e.
      ('e Handler.t @ local portable -> 'a @ contended once unique -> 'b) @ once
      -> ('a, 'b, 'e, unit) preemptible_continuation @ unique

    val fiber_with
      : ('a : value mod portable) 'b 'e 'es.
      'es Handler.List.Length.t @ local
      -> (('e * 'es) Handler.List.t @ local portable -> 'a @ contended once unique -> 'b)
         @ once
      -> ('a, 'b, 'e, 'es) preemptible_continuation @ unique

    val run
      :  on_tick:(unit -> Tick_outcome.t) @ portable
      -> ('e Handler.t @ local portable -> 'a) @ once
      -> ('a, 'e, unit) Preemptible_res.t @ once unique

    val run_with
      :  on_tick:(unit -> Tick_outcome.t) @ portable
      -> 'es Handler.List.t @ local portable
      -> (('e * 'es) Handler.List.t @ local portable -> 'a) @ once
      -> ('a, 'e, 'es) Preemptible_res.t @ once unique
  end
end = struct
  let[@inline] fiber f =
    let f h a =
      f (Obj.magic_portable h) ((Obj.magic_portable [@mode unique once]) a) [@nontail]
    in
    let k : ('a, 'b, 'e, unit) continuation = (Obj.magic [@mode unique]) (fiber f) in
    k
  ;;

  let[@inline] fiber_with hs f =
    let f hs a =
      f (Obj.magic_portable hs) ((Obj.magic_portable [@mode unique once]) a) [@nontail]
    in
    let k : ('a, 'b, 'e, 'es) continuation =
      (Obj.magic [@mode unique]) (fiber_with hs f)
    in
    k
  ;;

  let[@inline] run f =
    let f h = f (Obj.magic_portable h) [@nontail] in
    run f
  ;;

  let[@inline] run_with hs f =
    let f hs = f (Obj.magic_portable hs) [@nontail] in
    run_with hs f
  ;;

  module Preemptible = struct
    let fiber f =
      let f h a =
        f (Obj.magic_portable h) ((Obj.magic_portable [@mode unique once]) a) [@nontail]
      in
      let k : ('a, 'b, 'e, unit) preemptible_continuation =
        (Obj.magic [@mode unique]) (preemptible_fiber f)
      in
      k
    [@@inline]
    ;;

    let fiber_with hs f =
      let f hs a =
        f (Obj.magic_portable hs) ((Obj.magic_portable [@mode unique once]) a) [@nontail]
      in
      let k : ('a, 'b, 'e, 'es) preemptible_continuation =
        (Obj.magic [@mode unique]) (preemptible_fiber_with hs f)
      in
      k
    [@@inline]
    ;;

    let run ~on_tick f =
      let f h = f (Obj.magic_portable h) [@nontail] in
      run_preemptible ~on_tick f
    [@@inline]
    ;;

    let run_with ~on_tick hs f =
      let f hs = f (Obj.magic_portable hs) [@nontail] in
      run_with_preemptible ~on_tick hs f
    [@@inline]
    ;;
  end
end

module DRF_portable : sig @@ portable
  val fiber
    : ('a : value mod portable) 'b 'e.
    ('e Handler.t @ contended local portable -> 'a @ contended once unique -> 'b)
    @ once portable
    -> ('a, 'b, 'e, unit) continuation @ portable unique

  val fiber_with
    : ('a : value mod portable) 'b 'e 'es.
    'es Handler.List.Length.t @ local
    -> (('e * 'es) Handler.List.t @ contended local portable
        -> 'a @ contended once unique
        -> 'b)
       @ once portable
    -> ('a, 'b, 'e, 'es) continuation @ portable unique

  val run
    :  ('e Handler.t @ contended local portable -> 'a) @ once portable
    -> ('a, 'e, unit) res @ once unique

  val run_with
    :  'es Handler.List.t @ contended local portable
    -> (('e * 'es) Handler.List.t @ contended local portable -> 'a) @ once portable
    -> ('a, 'e, 'es) res @ once unique

  module Preemptible : sig
    val fiber
      : ('a : value mod portable) 'b 'e.
      ('e Handler.t @ contended local portable -> 'a @ contended once unique -> 'b)
      @ once portable
      -> ('a, 'b, 'e, unit) preemptible_continuation @ portable unique

    val fiber_with
      : ('a : value mod portable) 'b 'e 'es.
      'es Handler.List.Length.t @ local
      -> (('e * 'es) Handler.List.t @ contended local portable
          -> 'a @ contended once unique
          -> 'b)
         @ once portable
      -> ('a, 'b, 'e, 'es) preemptible_continuation @ portable unique

    val run
      :  on_tick:(unit -> Tick_outcome.t) @ portable
      -> ('e Handler.t @ contended local portable -> 'a) @ once portable
      -> ('a, 'e, unit) Preemptible_res.t @ once unique

    val run_with
      :  on_tick:(unit -> Tick_outcome.t) @ portable
      -> 'es Handler.List.t @ contended local portable
      -> (('e * 'es) Handler.List.t @ contended local portable -> 'a) @ once portable
      -> ('a, 'e, 'es) Preemptible_res.t @ once unique
  end
end = struct
  let[@inline] fiber f =
    let f h a =
      f
        (Obj.magic_portable h)
        ((Obj.magic_portable [@mode unique once])
           ((Obj.magic_uncontended [@mode unique once]) a)) [@nontail]
    in
    let k : ('a, 'b, 'e, unit) continuation = (Obj.magic [@mode unique]) (fiber f) in
    (Obj.magic_portable [@mode unique once]) k
  ;;

  let[@inline] fiber_with hs f =
    let f hs a =
      f
        (Obj.magic_portable hs)
        ((Obj.magic_portable [@mode unique once])
           ((Obj.magic_uncontended [@mode unique once]) a)) [@nontail]
    in
    let k : ('a, 'b, 'e, 'es) continuation =
      (Obj.magic [@mode unique]) (fiber_with hs f)
    in
    (Obj.magic_portable [@mode unique once]) k
  ;;

  let[@inline] run f =
    let f h = f (Obj.magic_portable h) [@nontail] in
    run f
  ;;

  let[@inline] run_with (hs @ contended local portable) f =
    let f hs = f (Obj.magic_portable hs) [@nontail] in
    run_with (Obj.magic_uncontended hs) f [@nontail]
  ;;

  module Preemptible = struct
    let fiber f =
      let f h a =
        f
          (Obj.magic_portable h)
          ((Obj.magic_portable [@mode unique once])
             ((Obj.magic_uncontended [@mode unique once]) a)) [@nontail]
      in
      let k : ('a, 'b, 'e, unit) preemptible_continuation =
        (Obj.magic [@mode unique]) (preemptible_fiber f)
      in
      (Obj.magic_portable [@mode unique once]) k
    [@@inline]
    ;;

    let fiber_with hs f =
      let f hs a =
        f
          (Obj.magic_portable hs)
          ((Obj.magic_portable [@mode unique once])
             ((Obj.magic_uncontended [@mode unique once]) a)) [@nontail]
      in
      let k : ('a, 'b, 'e, 'es) preemptible_continuation =
        (Obj.magic [@mode unique]) (preemptible_fiber_with hs f)
      in
      (Obj.magic_portable [@mode unique once]) k
    [@@inline]
    ;;

    let run ~on_tick f =
      let f h = f (Obj.magic_portable h) [@nontail] in
      run_preemptible ~on_tick f
    [@@inline]
    ;;

    let run_with ~on_tick (hs @ contended local portable) f =
      let f hs = f (Obj.magic_portable hs) [@nontail] in
      run_with_preemptible ~on_tick (Obj.magic_uncontended hs) f [@nontail]
    [@@inline]
    ;;
  end
end

module Continuation = struct
  type (-'a, +'b, 'es) t : value mod contended many =
    | Continuation : ('a, 'c, 'e, 'es) continuation -> ('a, 'b, 'es) t
  [@@unsafe_allow_any_mode_crossing "Only accesses mutable data uniquely. "] [@@unboxed]
  (* This type has an unexpressible constraint that ['b] is a type that can safely be
     [Obj.magic]ed from [(c, e, es) res] *)

  let get_callstack (Continuation cont) i = get_callstack cont i

  module Preemptible = struct
    type (-'a, +'b, 'es) t : value mod contended many =
      | Continuation : ('a, 'c, 'e, 'es) preemptible_continuation -> ('a, 'b, 'es) t
    [@@unsafe_allow_any_mode_crossing "Only accesses mutable data uniquely. "] [@@unboxed]
  end
end

let continue (type a b es) (k : (a, b, es) Continuation.t) v hs =
  let (Continuation (type e c) (cont : (a, c, e, es) continuation)) = k in
  let res : (c, e, es) res = continue cont v hs in
  (* Sound per the constraint on [Continuation.t]. *)
  ((Obj.magic [@mode unique once]) res : b)
;;

let discontinue (type a b es) (k : (a, b, es) Continuation.t) e hs =
  let (Continuation (type e c) (cont : (a, c, e, es) continuation)) = k in
  let res : (c, e, es) res = discontinue cont e hs in
  (* Sound per the constraint on [Continuation.t]. *)
  ((Obj.magic [@mode unique once]) res : b)
;;

let discontinue_with_backtrace (type a b es) (k : (a, b, es) Continuation.t) e bt hs =
  let (Continuation (type e c) (cont : (a, c, e, es) continuation)) = k in
  let res : (c, e, es) res = discontinue_with_backtrace cont e bt hs in
  (* Sound per the constraint on [Continuation.t]. *)
  ((Obj.magic [@mode unique once]) res : b)
;;

module Preemptible = struct
  module Tick_outcome = Tick_outcome

  let continue (type a b es) ~on_tick (k : (a, b, es) Continuation.Preemptible.t) v hs =
    let (Continuation (type e c) (cont : (a, c, e, es) preemptible_continuation)) = k in
    let res : (c, e, es) Preemptible_res.t = continue_preemptible ~on_tick cont v hs in
    ((Obj.magic [@mode unique once]) res : b)
  ;;
end

include Handled_effect_intf.Definitions (Handler) (Continuation)

module type Types = sig
  type ('p, 'q) t
  type ('a, 'p, 'q, 'e) ops
  type ('a, 'p, 'q, 'es) result
  type ('a, 'p, 'q, 'es) preemptible_result
end

module Make_generic (Types : Types) :
  S2_generic
  with type ('p, 'q) t := ('p, 'q) Types.t
   and type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q, 'e) Types.ops
   and type ('a, 'p, 'q, 'es) result := ('a, 'p, 'q, 'es) Types.result
   and type ('a, 'p, 'q, 'es) preemptible_result :=
    ('a, 'p, 'q, 'es) Types.preemptible_result = struct
  include Types

  let fiber f = Continuation.Continuation (fiber f)
  let fiber_with hs f = Continuation.Continuation (fiber_with hs f)

  let run (type a p q) f =
    let res : (a, (p, q) t, unit) res = run f in
    ((Obj.magic [@mode unique once]) res : (a, p, q, unit) result)
  ;;

  let run_with (type a p q es) hs f =
    let res : (a, (p, q) t, es) res = run_with hs f in
    ((Obj.magic [@mode unique once]) res : (a, p, q, es) result)
  ;;

  let perform (type a p q) (h : _ Handler.t @ local) (op : (a, p, q, (p, q) t) ops) =
    let op : (a, (p, q) t) op = Obj.magic op in
    perform_ (Handler.globalize h, op) [@nontail]
  ;;

  module Preemptible = struct
    let fiber f = Continuation.Preemptible.Continuation (preemptible_fiber f)

    let fiber_with hs f =
      Continuation.Preemptible.Continuation (preemptible_fiber_with hs f)
    ;;

    let run (type a p q) ~on_tick f =
      let res : (a, (p, q) t, unit) Preemptible_res.t = run_preemptible ~on_tick f in
      ((Obj.magic [@mode unique once]) res : (a, p, q, unit) preemptible_result)
    ;;

    let run_with (type a p q es) ~on_tick hs f =
      let res : (a, (p, q) t, es) Preemptible_res.t =
        run_with_preemptible ~on_tick hs f
      in
      ((Obj.magic [@mode unique once]) res : (a, p, q, es) preemptible_result)
    ;;
  end
end

module Make_generic_contended (Types : Types) :
  S2_generic
  [@mode portable nonportable]
  with type ('p, 'q) t := ('p, 'q) Types.t
   and type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q, 'e) Types.ops
   and type ('a, 'p, 'q, 'es) result := ('a, 'p, 'q, 'es) Types.result
   and type ('a, 'p, 'q, 'es) preemptible_result :=
    ('a, 'p, 'q, 'es) Types.preemptible_result = struct
  include Types

  let fiber f = Continuation.Continuation (DRF.fiber f)
  let fiber_with l f = Continuation.Continuation (DRF.fiber_with l f)

  let run (type a p q) f =
    let res : (a, (p, q) t, unit) res = DRF.run f in
    ((Obj.magic [@mode unique once]) res : (a, p, q, unit) result)
  ;;

  let run_with (type a p q es) hs f =
    let res : (a, (p, q) t, es) res = DRF.run_with hs f in
    ((Obj.magic [@mode unique once]) res : (a, p, q, es) result)
  ;;

  let perform (type a p q) (h : _ Handler.t @ contended local) op : a =
    let op : (a, (p, q) t) op = Obj.magic op in
    (* Here and below, [magic_uncontended] is safe since the handler is either portable or
       is executed in its original capsule. *)
    let h = Obj.magic_uncontended h in
    perform_ (Handler.globalize h, op) [@nontail]
  ;;

  module Preemptible = struct
    let fiber f = Continuation.Preemptible.Continuation (DRF.Preemptible.fiber f)

    let fiber_with hs f =
      Continuation.Preemptible.Continuation (DRF.Preemptible.fiber_with hs f)
    ;;

    let run (type a p q) ~on_tick f =
      let res : (a, (p, q) t, unit) Preemptible_res.t = DRF.Preemptible.run ~on_tick f in
      ((Obj.magic [@mode unique once]) res : (a, p, q, unit) preemptible_result)
    ;;

    let run_with (type a p q es) ~on_tick hs f =
      let res : (a, (p, q) t, es) Preemptible_res.t =
        DRF.Preemptible.run_with ~on_tick hs f
      in
      ((Obj.magic [@mode unique once]) res : (a, p, q, es) preemptible_result)
    ;;
  end
end

module Make_generic_portable (Types : Types) :
  S2_generic
  [@mode portable portable]
  with type ('p, 'q) t := ('p, 'q) Types.t
   and type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q, 'e) Types.ops
   and type ('a, 'p, 'q, 'es) result := ('a, 'p, 'q, 'es) Types.result
   and type ('a, 'p, 'q, 'es) preemptible_result :=
    ('a, 'p, 'q, 'es) Types.preemptible_result = struct
  include Types

  let fiber f = Continuation.Continuation (DRF_portable.fiber f)
  let fiber_with l f = Continuation.Continuation (DRF_portable.fiber_with l f)

  let run (type a p q) f =
    let res : (a, (p, q) t, unit) res = DRF_portable.run f in
    ((Obj.magic [@mode unique once]) res : (a, p, q, unit) result)
  ;;

  let run_with (type a p q es) hs f =
    let res : (a, (p, q) t, es) res = DRF_portable.run_with hs f in
    ((Obj.magic [@mode unique once]) res : (a, p, q, es) result)
  ;;

  let perform (type a p q) (h : _ Handler.t) (op : (a, p, q, (p, q) t) ops @ portable) =
    let op : (a, (p, q) t) op = Obj.magic op in
    let h = Obj.magic_uncontended h in
    perform_ (Handler.globalize h, op) [@nontail]
  ;;

  module Preemptible = struct
    let fiber f = Continuation.Preemptible.Continuation (DRF_portable.Preemptible.fiber f)

    let fiber_with hs f =
      Continuation.Preemptible.Continuation (DRF_portable.Preemptible.fiber_with hs f)
    ;;

    let run (type a p q) ~on_tick f =
      let res : (a, (p, q) t, unit) Preemptible_res.t =
        DRF_portable.Preemptible.run ~on_tick f
      in
      ((Obj.magic [@mode unique once]) res : (a, p, q, unit) preemptible_result)
    ;;

    let run_with (type a p q es) ~on_tick (hs @ contended) f =
      let res : (a, (p, q) t, es) Preemptible_res.t =
        DRF_portable.Preemptible.run_with ~on_tick hs f
      in
      ((Obj.magic [@mode unique once]) res : (a, p, q, es) preemptible_result)
    ;;
  end
end

module%template
  [@mode
    (p_arg, c_arg) = ((nonportable, uncontended), (portable, contended))
    , (p_res, c_res) = ((nonportable, uncontended), (portable, contended))]
  Make_rec_base
    (T : sig
       type t
     end)
    (Ops : Operations_rec) =
struct
  type ('a, 'es) result =
    | Value : 'a @@ global many -> ('a, 'es) result
    | Exception : exn @@ global many -> ('a, 'es) result
    | Operation :
        ('o, T.t) Ops.t @@ c_arg global many
        * ('o, ('a, 'es) result, 'es) Continuation.t @@ p_res
        -> ('a, 'es) result

  type ('a, 'es) preemptible_result =
    | Value : 'a @@ global many -> ('a, 'es) preemptible_result
    | Exception : exn @@ global many -> ('a, 'es) preemptible_result
    | Operation :
        ('o, T.t) Ops.t @@ c_arg global many
        * ('o, ('a, 'es) preemptible_result, 'es) Continuation.Preemptible.t @@ p_res
        -> ('a, 'es) preemptible_result
    | Preemption :
        (unit, ('a, 'es) preemptible_result, 'es) Continuation.Preemptible.t @@ p_res
        -> ('a, 'es) preemptible_result

  module Result = struct
    type ('a, 'es) t = ('a, 'es) result =
      | Value : 'a @@ global many -> ('a, 'es) t
      | Exception : exn @@ global many -> ('a, 'es) t
      | Operation :
          ('o, T.t) Ops.t @@ c_arg global many
          * ('o, ('a, 'es) result, 'es) Continuation.t @@ p_res
          -> ('a, 'es) t

    module Preemptible = struct
      type ('a, 'es) t = ('a, 'es) preemptible_result =
        | Value : 'a @@ global many -> ('a, 'es) t
        | Exception : exn @@ global many -> ('a, 'es) t
        | Operation :
            ('o, T.t) Ops.t @@ c_arg global many
            * ('o, ('a, 'es) t, 'es) Continuation.Preemptible.t @@ p_res
            -> ('a, 'es) t
        | Preemption :
            (unit, ('a, 'es) t, 'es) Continuation.Preemptible.t @@ p_res
            -> ('a, 'es) t
    end
  end
end

module Make_rec (Ops : Operations_rec) : S with type ('a, 'e) ops := ('a, 'e) Ops.t =
struct
  type t

  module T = struct
    type nonrec t = t
  end

  include Make_rec_base (T) (Ops)

  include Make_generic (struct
      type nonrec (_, _) t = t
      type nonrec ('a, _, _, 'e) ops = ('a, 'e) Ops.t
      type nonrec ('a, _, _, 'es) result = ('a, 'es) result
      type nonrec ('a, _, _, 'es) preemptible_result = ('a, 'es) preemptible_result
    end)

  module Contended = struct
    include Make_rec_base [@mode portable nonportable] (T) (Ops)

    include Make_generic_contended (struct
        type nonrec (_, _) t = t
        type nonrec ('a, _, _, 'e) ops = ('a, 'e) Ops.t
        type nonrec ('a, _, _, 'es) result = ('a, 'es) result
        type nonrec ('a, _, _, 'es) preemptible_result = ('a, 'es) preemptible_result
      end)
  end

  module Portable = struct
    include Make_rec_base [@mode portable portable] (T) (Ops)

    include Make_generic_portable (struct
        type nonrec (_, _) t = t
        type nonrec ('a, _, _, 'e) ops = ('a, 'e) Ops.t
        type nonrec ('a, _, _, 'es) result = ('a, 'es) result
        type nonrec ('a, _, _, 'es) preemptible_result = ('a, 'es) preemptible_result
      end)
  end

  module Handler = struct
    type nonrec t = t Handler.t
  end

  module Continuation = struct
    type ('a, 'b, 'es) t = ('a, ('b, 'es) result, 'es) Continuation.t

    module Preemptible = struct
      type ('a, 'b, 'es) t =
        ('a, ('b, 'es) preemptible_result, 'es) Continuation.Preemptible.t
    end
  end
end

module Make (Ops : Operations) : S with type ('a, 'e) ops := 'a Ops.t = Make_rec (struct
    type ('a, 'e) t = 'a Ops.t
  end)

module%template
  [@mode
    (p_arg, c_arg) = ((nonportable, uncontended), (portable, contended))
    , (p_res, c_res) = ((nonportable, uncontended), (portable, contended))]
  Make1_rec_base
    (T : sig
       type 'p t
     end)
    (Ops : Operations1_rec) =
struct
  type ('a, 'p, 'es) result =
    | Value : 'a @@ global many -> ('a, 'p, 'es) result
    | Exception : exn @@ global many -> ('a, 'p, 'es) result
    | Operation :
        ('o, 'p, 'p T.t) Ops.t @@ c_arg global many
        * ('o, ('a, 'p, 'es) result, 'es) Continuation.t @@ p_res
        -> ('a, 'p, 'es) result

  module Result = struct
    type ('a, 'p, 'es) t = ('a, 'p, 'es) result =
      | Value : 'a @@ global many -> ('a, 'p, 'es) t
      | Exception : exn @@ global many -> ('a, 'p, 'es) t
      | Operation :
          ('o, 'p, 'p T.t) Ops.t @@ c_arg global many
          * ('o, ('a, 'p, 'es) result, 'es) Continuation.t @@ p_res
          -> ('a, 'p, 'es) t

    module Preemptible = struct
      type ('a, 'p, 'es) t =
        | Value : 'a @@ global many -> ('a, 'p, 'es) t
        | Exception : exn @@ global many -> ('a, 'p, 'es) t
        | Operation :
            ('o, 'p, 'p T.t) Ops.t @@ c_arg global many
            * ('o, ('a, 'p, 'es) t, 'es) Continuation.Preemptible.t @@ p_res
            -> ('a, 'p, 'es) t
        | Preemption :
            (unit, ('a, 'p, 'es) t, 'es) Continuation.Preemptible.t @@ p_res
            -> ('a, 'p, 'es) t
    end
  end
end

module Make1_rec (Ops : Operations1_rec) :
  S1 with type ('a, 'p, 'e) ops := ('a, 'p, 'e) Ops.t = struct
  type 'p t

  module T = struct
    type nonrec 'p t = 'p t
  end

  include Make1_rec_base (T) (Ops)

  include Make_generic (struct
      type nonrec ('p, _) t = 'p t
      type nonrec ('a, 'p, _, 'e) ops = ('a, 'p, 'e) Ops.t
      type nonrec ('a, 'p, _, 'es) result = ('a, 'p, 'es) result
      type nonrec ('a, 'p, _, 'es) preemptible_result = ('a, 'p, 'es) Result.Preemptible.t
    end)

  module Contended = struct
    include Make1_rec_base [@mode portable nonportable] (T) (Ops)

    include Make_generic_contended (struct
        type nonrec ('p, _) t = 'p t
        type nonrec ('a, 'p, _, 'e) ops = ('a, 'p, 'e) Ops.t
        type nonrec ('a, 'p, _, 'es) result = ('a, 'p, 'es) result

        type nonrec ('a, 'p, _, 'es) preemptible_result =
          ('a, 'p, 'es) Result.Preemptible.t
      end)
  end

  module Portable = struct
    include Make1_rec_base [@mode portable portable] (T) (Ops)

    include Make_generic_portable (struct
        type nonrec ('p, _) t = 'p t
        type nonrec ('a, 'p, _, 'e) ops = ('a, 'p, 'e) Ops.t
        type nonrec ('a, 'p, _, 'es) result = ('a, 'p, 'es) result

        type nonrec ('a, 'p, _, 'es) preemptible_result =
          ('a, 'p, 'es) Result.Preemptible.t
      end)
  end

  module Handler = struct
    type nonrec 'p t = 'p t Handler.t
  end

  module Continuation = struct
    type ('a, 'b, 'p, 'es) t = ('a, ('b, 'p, 'es) result, 'es) Continuation.t

    module Preemptible = struct
      type ('a, 'b, 'p, 'es) t =
        ('a, ('b, 'p, 'es) Result.Preemptible.t, 'es) Continuation.Preemptible.t
    end
  end
end

module Make1 (Ops : Operations1) : S1 with type ('a, 'p, 'e) ops := ('a, 'p) Ops.t =
Make1_rec (struct
    type ('a, 'p, 'e) t = ('a, 'p) Ops.t
  end)

module%template
  [@mode
    (p_arg, c_arg) = ((nonportable, uncontended), (portable, contended))
    , (p_res, c_res) = ((nonportable, uncontended), (portable, contended))]
  Make2_rec_base
    (T : sig
       type ('p, 'q) t
     end)
    (Ops : Operations2_rec) =
struct
  type ('a, 'p, 'q, 'es) result =
    | Value : 'a @@ global many -> ('a, 'p, 'q, 'es) result
    | Exception : exn @@ global many -> ('a, 'p, 'q, 'es) result
    | Operation :
        ('o, 'p, 'q, ('p, 'q) T.t) Ops.t @@ c_arg global many
        * ('o, ('a, 'p, 'q, 'es) result, 'es) Continuation.t @@ p_res
        -> ('a, 'p, 'q, 'es) result

  module Result = struct
    type ('a, 'p, 'q, 'es) t = ('a, 'p, 'q, 'es) result =
      | Value : 'a @@ global many -> ('a, 'p, 'q, 'es) t
      | Exception : exn @@ global many -> ('a, 'p, 'q, 'es) t
      | Operation :
          ('o, 'p, 'q, ('p, 'q) T.t) Ops.t @@ c_arg global many
          * ('o, ('a, 'p, 'q, 'es) result, 'es) Continuation.t @@ p_res
          -> ('a, 'p, 'q, 'es) t

    module Preemptible = struct
      type ('a, 'p, 'q, 'es) t =
        | Value : 'a @@ global many -> ('a, 'p, 'q, 'es) t
        | Exception : exn @@ global many -> ('a, 'p, 'q, 'es) t
        | Operation :
            ('o, 'p, 'q, ('p, 'q) T.t) Ops.t @@ c_arg global many
            * ('o, ('a, 'p, 'q, 'es) t, 'es) Continuation.Preemptible.t @@ p_res
            -> ('a, 'p, 'q, 'es) t
        | Preemption :
            (unit, ('a, 'p, 'q, 'es) t, 'es) Continuation.Preemptible.t @@ p_res
            -> ('a, 'p, 'q, 'es) t
    end
  end
end

module Make2_rec (Ops : Operations2_rec) :
  S2 with type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q, 'e) Ops.t = struct
  type ('p, 'q) t

  module T = struct
    type nonrec ('p, 'q) t = ('p, 'q) t
  end

  include Make2_rec_base (T) (Ops)

  include Make_generic (struct
      type nonrec ('p, 'q) t = ('p, 'q) t
      type nonrec ('a, 'p, 'q, 'e) ops = ('a, 'p, 'q, 'e) Ops.t
      type nonrec ('a, 'p, 'q, 'es) result = ('a, 'p, 'q, 'es) result

      type nonrec ('a, 'p, 'q, 'es) preemptible_result =
        ('a, 'p, 'q, 'es) Result.Preemptible.t
    end)

  module Contended = struct
    include Make2_rec_base [@mode portable nonportable] (T) (Ops)

    include Make_generic_contended (struct
        type nonrec ('p, 'q) t = ('p, 'q) t
        type nonrec ('a, 'p, 'q, 'e) ops = ('a, 'p, 'q, 'e) Ops.t
        type nonrec ('a, 'p, 'q, 'es) result = ('a, 'p, 'q, 'es) result

        type nonrec ('a, 'p, 'q, 'es) preemptible_result =
          ('a, 'p, 'q, 'es) Result.Preemptible.t
      end)
  end

  module Portable = struct
    include Make2_rec_base [@mode portable portable] (T) (Ops)

    include Make_generic_portable (struct
        type nonrec ('p, 'q) t = ('p, 'q) t
        type nonrec ('a, 'p, 'q, 'e) ops = ('a, 'p, 'q, 'e) Ops.t
        type nonrec ('a, 'p, 'q, 'es) result = ('a, 'p, 'q, 'es) result

        type nonrec ('a, 'p, 'q, 'es) preemptible_result =
          ('a, 'p, 'q, 'es) Result.Preemptible.t
      end)
  end

  module Handler = struct
    type nonrec ('p, 'q) t = ('p, 'q) t Handler.t
  end

  module Continuation = struct
    type ('a, 'b, 'p, 'q, 'es) t = ('a, ('b, 'p, 'q, 'es) result, 'es) Continuation.t

    module Preemptible = struct
      type ('a, 'b, 'p, 'q, 'es) t =
        ('a, ('b, 'p, 'q, 'es) Result.Preemptible.t, 'es) Continuation.Preemptible.t
    end
  end
end

module Make2 (Ops : Operations2) :
  S2 with type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q) Ops.t = Make2_rec (struct
    type ('a, 'p, 'q, 'e) t = ('a, 'p, 'q) Ops.t
  end)

exception Continuation_already_resumed

type Stdlib.Printexc.t += Unhandled : 'e Handler.t -> Stdlib.Printexc.t

external magic_unyielding_global : 'a @ yielding -> 'a @@ stateless = "%identity"

(* Register the exceptions so that the runtime can access it *)
let _ =
  Basement.Stdlib_shim.Callback.Safe.register_exception
    "Effect.Unhandled"
    (Unhandled (magic_unyielding_global (Handler.globalize (Handler.make Handler.Dummy))))
;;

let _ =
  Basement.Stdlib_shim.Callback.Safe.register_exception
    "Effect.Continuation_already_resumed"
    Continuation_already_resumed
;;
