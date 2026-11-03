open Base
include Handled_effect_intf

module Handler_index : sig
  (** [(es1, es2) t] represents an index into the effect list [es2]. [es1] is the tail
      sublist containing the index and all following effects. For instance,
      [('b * ('c * unit), 'a * ('b * ('c * unit)) t] represents effect 1, ['b].

      It is used for building [Raw_handler.t]s, which represent one of the effects handled
      by a particular handler. *)
  type ('es1, 'es2) t [@@immediate]

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

module Raw_handler : sig
  (** [(e, es) t] is a handler for the effect [e], which must be an element of the effect
      list [es].

      It is used to represent one of the effects handled by a particular handler. *)
  type ('e, 'es) t [@@immediate]

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
  type t
end

module Handler : sig
  (** [e t] is a handler for the effect [e]. *)
  type 'e t0 = private ..

  (** Heap-allocated handler. *)
  type 'e t_u =
    { h : 'e t0
    ; yielding : Yielding.t
    }

  type 'e t

  val unbox : 'e t -> 'e t_u
  val box : 'e t_u -> 'e t
  val globalize : 'e t -> 'e t
  val make : 'e t0 -> 'e t

  module List : sig
    include Handler_list with type 'e handler := 'e t

    val yielding : 'es t -> Yielding.t
  end

  include Handler with type 'e t := 'e t and module List := List

  type _ t0 += Dummy : 'a t0

  module type Create = sig
    type e
    type es
    type 'e t0 += C : ('e, e * es) Raw_handler.t -> 'e t0

    (** [initial ~length] is a list of handlers for effects [es], where [length] is the
        length of [es]. *)
    val initial : length:es List.Length.t -> es List.t

    (** [initial_from hs] is a list of handlers for effect [es], where [hs] is another
        list of handlers for effects [es]. These handlers are selected from the effect
        list [e * es] for some effect [e]. Note that [hs] is being used only for its
        length -- the actual handlers in it do not affect the output. *)
    val initial_from : es List.t -> es List.t
  end

  val create : unit -> (module Create with type e = 'e and type es = 'es)
end = struct
  type 'e t0 = ..

  type 'e t_u =
    { h : 'e t0
    ; yielding : Yielding.t
    }

  type 'e t = { h : 'e t0 } [@@unboxed]

  external magic_mk_yielding : unit -> Yielding.t = "%identity"

  let unbox { h } = { h; yielding = magic_mk_yielding () }
  let box ({ h; _ } : _ t_u) : _ t = { h }
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
      let rec loop : type es. es t -> es Length.t = function
        | [] -> []
        | _ :: rest -> X :: loop rest
      in
      loop t [@nontail]
    ;;

    let yielding (_ : _ t) = magic_mk_yielding ()
  end

  module type Create = sig
    type e
    type es
    type 'e t0 += C : ('e, e * es) Raw_handler.t -> 'e t0

    val initial : length:es List.Length.t -> es List.t
    val initial_from : es List.t -> es List.t
  end

  let[@inline] create (type e es) () : (module Create with type e = e and type es = es) =
    let module Create = struct
      type nonrec e = e
      type nonrec es = es
      type 'e t0 += C : ('e, e * es) Raw_handler.t -> 'e t0

      let initial ~(length : es List.Length.t) : es List.t =
        let rec loop
          : type esr. (esr, e * es) Handler_index.t -> esr List.Length.t -> esr List.t
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

      let initial_from (t : es List.t) : es List.t =
        let rec loop : type esr. (esr, e * es) Handler_index.t -> esr List.t -> esr List.t
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

module Mapping : sig
  (** [es t] represents a mutable mapping from an effect in [es] to its handler. *)
  type 'es t

  (** [lookup h t] looks up the effect handled by [h] in mapping [t] and returns its
      handler. *)
  val lookup : ('e, 'es) Raw_handler.t -> 'es t -> 'e Handler.t

  (** [empty] is the mapping for the empty effect list. *)
  val empty : unit -> unit t

  (** [create hs] creates a new mapping from [es], where [hs] is a list of handlers for
      the effects [es]. Its initial value is to map each effect in [es] to the
      corresponding handler in [hs]. *)
  val create : 'es Handler.List.t -> 'es t

  (** [set hs t] updates the mapping [t] to map each effect handled by [hs] to its
      corresponding handler in [hs]. *)
  val set : 'es Handler.List.t -> 'es t -> unit

  (** [unsafe_create_uninitialized len] creates a new uninitialized mapping from [es],
      where [len] is the length of [es]. The mapping must be initialized with [set] before
      it is used. *)
  val unsafe_create_uninitialized : 'es Handler.List.Length.t -> 'es t
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
    (h : e Handler.t)
    =
    let elt : element = P (Handler.unbox h).h in
    Array.unsafe_set t (Handler_index.to_int idx) (This elt)
  ;;

  let create (type es) (l : es Handler.List.t) =
    let rec loop : type esr. (esr, es) Handler_index.t -> esr Handler.List.t -> es t =
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

  let set (type es) (hs : es Handler.List.t) (t : es t) =
    let rec loop
      : type esr. (esr, es) Handler_index.t -> esr Handler.List.t -> es t -> unit
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

  let unsafe_create_uninitialized (type es) (l : es Handler.List.Length.t) =
    let rec loop
      : type esr. (esr, es) Handler_index.t -> esr Handler.List.Length.t -> es t
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
external perform_ : ('a, 'e) perform -> 'a = "%perform"

let perform_ p = perform_ p [@nontail]

(* A last_fiber is a tagged pointer, so does not keep the fiber alive. It must never be
   the sole reference to the fiber, and is only used to cache the final fiber in the
   linked list formed by [cont.fiber->parent]. *)
type last_fiber [@@immediate]
type (-'a, +'b) cont

external get_cont_callstack
  :  ('a, 'b) cont
  -> int
  -> Backtrace.t
  = "caml_get_continuation_callstack"

external cont_set_last_fiber : ('a, 'b) cont -> last_fiber -> unit = "%setfield1"

type 'b effc = { effc : 'o 'e. ('o, 'e) perform -> ('o, 'b) cont -> last_fiber -> 'b }
[@@unboxed]

external cont_last_fiber : ('a, 'b) cont -> last_fiber = "%field1"
external cont_set_last_fiber : ('a, 'b) cont -> last_fiber -> unit = "%setfield1"

module Must_not_enter_gc = struct
  type (-'a, +'b) stack [@@immediate]

  external alloc_stack
    :  ('a -> 'b)
    -> (exn -> 'b)
    -> 'b effc
    -> ('a, 'b) stack
    = "caml_alloc_stack"

  external runstack : ('a, 'b) stack -> ('c -> 'a) -> 'c -> 'b = "%runstack"

  external take_cont_noexc
    :  ('a, 'b) cont
    -> ('a, 'b) stack
    = "caml_continuation_use_noexc"
  [@@noalloc]

  external resume : ('a, 'b) stack -> ('c -> 'a) -> 'c -> last_fiber -> 'b = "%resume"
end

let with_stack valuec exnc effc f x =
  Must_not_enter_gc.runstack (Must_not_enter_gc.alloc_stack valuec exnc effc) f x
[@@inline never]
;;

let with_cont cont f x =
  let fiber = cont_last_fiber cont in
  Must_not_enter_gc.resume (Must_not_enter_gc.take_cont_noexc cont) f x fiber
[@@inline never]
;;

let continue cont v = with_cont cont (fun x -> x) v

let discontinue cont e =
  with_cont cont (fun { aliased_many = e } -> raise e) { aliased_many = e }
;;

let discontinue_with_backtrace cont e bt =
  with_cont
    cont
    (fun { aliased_many = e } -> Exn.raise_with_original_backtrace e bt)
    { aliased_many = e }
;;

type (+'a, 'es) r =
  | Val : 'a -> ('a, 'es) r
  | Exn : exn -> ('a, 'es) r
  | Op :
      ('o, 'e) op * ('e, 'es) Raw_handler.t * ('o, ('a, 'es) r) cont * last_fiber
      -> ('a, 'es) r

let valuec v = Val v
let exnc e = Exn e

external reperform : ('a, 'e) perform -> ('a, 'b) cont -> last_fiber -> 'b = "%reperform"

let reperform (_ : Yielding.t) p k lf = reperform p k lf

let alloc_cont
  (type a b h e es)
  (module H : Handler.Create with type e = e and type es = es)
  (f : h -> a -> b)
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
    match (Handler.unbox h).h with
    | H.C h -> Op (op, h, k, last_fiber)
    | Handler.Dummy ->
      let k = (Obj.magic k : (a, (b, e * es) r) cont) in
      cont_set_last_fiber k last_fiber;
      (* Note that [k] is still actually unique here. *)
      Basement.Stdlib_shim.raise_notrace (Ready__ k)
    | _ -> reperform (Handler.unbox h).yielding perf k last_fiber [@nontail]
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
  (f : h -> a)
  (h : h)
  : (a, e * es) r
  =
  let effc ((h, op) as perf) k last_fiber =
    let { Handler.h; yielding } = Handler.unbox h in
    match h with
    | H.C h -> Op (op, h, k, last_fiber)
    | _ -> reperform yielding perf k last_fiber [@nontail]
  in
  with_stack valuec exnc { effc } f h
;;

type (-'a, +'b, 'e, 'es) continuation =
  | Cont :
      { cont : ('a, ('b, 'e * 'es) r) cont
      ; mapping : 'es Mapping.t
      }
      -> ('a, 'b, 'e, 'es) continuation

type ('a, 'e, 'es) res =
  | Value : 'a -> ('a, 'e, 'es) res
  | Exception : exn -> ('a, 'e, 'es) res
  | Operation : ('o, 'e) op * ('o, 'a, 'e, 'es) continuation -> ('a, 'e, 'es) res

let get_callstack (Cont { cont; _ }) i = get_cont_callstack cont i

(* [handle] takes a [yielding] witness so that the recursive call below (and [handle]'s
   call sites) are seen as able to perform: forwarding an effect to an outer handler
   reperforms, so these calls must not be compiled in direct style (see
   [Handler.unbox]/[Yielding]). The witness is threaded from the handlers held by
   [handle]'s callers, never summoned here. *)
let rec handle
  : type a e es. Yielding.t -> es Mapping.t -> (a, e * es) r -> (a, e, es) res
  =
  fun yielding mapping -> function
  | Val x -> Value x
  | Exn e -> Exception e
  | Op (op, handler, k, last_fiber) ->
    (match Raw_handler.is_zero handler with
     | Some Equal ->
       cont_set_last_fiber k last_fiber;
       Operation (op, Cont { cont = k; mapping })
     | None ->
       let handler = Raw_handler.weaken handler in
       let fwd = Mapping.lookup handler mapping |> Handler.unbox in
       (* the bytecode compiler requires [reperform] to be in tail position *)
       let result = reperform yielding (Handler.box fwd, op) k last_fiber [@nontail] in
       handle yielding mapping result)
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

let fiber (type a b e) (f : e Handler.t -> a -> b) =
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
  (l : es Handler.List.Length.t)
  (f : (e * es) Handler.List.t -> a -> b)
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

let run (type a e) (f : e Handler.t -> a) =
  let module H =
    (val Handler.create () : Handler.Create with type e = e and type es = unit)
  in
  let handler : (e, e * unit) Raw_handler.t = Raw_handler.zero in
  let handler : e Handler.t = Handler.make (H.C handler) in
  let res = run_stack (module H) f handler in
  handle (Handler.unbox handler).yielding (Mapping.empty ()) res
;;

let run_with (type a e es) (hs : es Handler.List.t) (f : (e * es) Handler.List.t -> a) =
  let module H = (val Handler.create () : Handler.Create with type e = e and type es = es)
  in
  let handler : (e, e * es) Raw_handler.t = Raw_handler.zero in
  let head = Handler.make (H.C handler) in
  let handlers : (e * es) Handler.List.t = head :: H.initial_from hs in
  let mapping = Mapping.create hs in
  let res = run_stack (module H) f handlers in
  handle (Handler.unbox head).yielding mapping res
;;

(* Data-race-free makepers around [fiber]/[run] functions.

   Uses [Obj.magic_portable] to avoid duplicating implementations of
   [alloc_cont]/[run_stack] in absence of mode polymorphism.

   Makepers provide the handler at [portable], but require [portable] arguments and
   operations, which are marked as [contended]. *)
module DRF : sig
  val fiber : 'a 'b 'e. ('e Handler.t -> 'a -> 'b) -> ('a, 'b, 'e, unit) continuation

  val fiber_with
    : 'a 'b 'e 'es.
    'es Handler.List.Length.t
    -> (('e * 'es) Handler.List.t -> 'a -> 'b)
    -> ('a, 'b, 'e, 'es) continuation

  val run : ('e Handler.t -> 'a) -> ('a, 'e, unit) res
  (* Returns a [res] to be [Obj.magic]ed into the contended result type with
     [op @@ contended]. *)

  val run_with
    :  'es Handler.List.t
    -> (('e * 'es) Handler.List.t -> 'a)
    -> ('a, 'e, 'es) res
  (* Returns a [res] to be [Obj.magic]ed into the contended result type with
     [op @@ contended]. *)
end = struct
  let[@inline] fiber f =
    let f h a = f (Obj.magic_portable h) (Obj.magic_portable a) [@nontail] in
    let k : ('a, 'b, 'e, unit) continuation = Obj.magic (fiber f) in
    k
  ;;

  let[@inline] fiber_with hs f =
    let f hs a = f (Obj.magic_portable hs) (Obj.magic_portable a) [@nontail] in
    let k : ('a, 'b, 'e, 'es) continuation = Obj.magic (fiber_with hs f) in
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
end

module DRF_portable : sig
  val fiber : 'a 'b 'e. ('e Handler.t -> 'a -> 'b) -> ('a, 'b, 'e, unit) continuation

  val fiber_with
    : 'a 'b 'e 'es.
    'es Handler.List.Length.t
    -> (('e * 'es) Handler.List.t -> 'a -> 'b)
    -> ('a, 'b, 'e, 'es) continuation

  val run : ('e Handler.t -> 'a) -> ('a, 'e, unit) res

  val run_with
    :  'es Handler.List.t
    -> (('e * 'es) Handler.List.t -> 'a)
    -> ('a, 'e, 'es) res
end = struct
  let[@inline] fiber f =
    let f h a =
      f (Obj.magic_portable h) (Obj.magic_portable (Obj.magic_uncontended a)) [@nontail]
    in
    let k : ('a, 'b, 'e, unit) continuation = Obj.magic (fiber f) in
    Obj.magic_portable k
  ;;

  let[@inline] fiber_with hs f =
    let f hs a =
      f (Obj.magic_portable hs) (Obj.magic_portable (Obj.magic_uncontended a)) [@nontail]
    in
    let k : ('a, 'b, 'e, 'es) continuation = Obj.magic (fiber_with hs f) in
    Obj.magic_portable k
  ;;

  let[@inline] run f =
    let f h = f (Obj.magic_portable h) [@nontail] in
    run f
  ;;

  let[@inline] run_with hs f =
    let f hs = f (Obj.magic_portable hs) [@nontail] in
    run_with (Obj.magic_uncontended hs) f [@nontail]
  ;;
end

module Continuation = struct
  type (-'a, +'b, 'es) t =
    | Continuation : ('a, 'c, 'e, 'es) continuation -> ('a, 'b, 'es) t
  [@@unsafe_allow_any_mode_crossing "Only accesses mutable data uniquely. "] [@@unboxed]
  (* This type has an unexpressible constraint that ['b] is a type that can safely be
     [Obj.magic]ed from [(c, e, es) res] *)

  let get_callstack (Continuation cont) i = get_callstack cont i
end

let continue (type a b es) (k : (a, b, es) Continuation.t) v hs =
  let (Continuation (type e c) (cont : (a, c, e, es) continuation)) = k in
  let res : (c, e, es) res = continue cont v hs in
  (* Sound per the constraint on [Continuation.t]. *)
  (Obj.magic res : b)
;;

let discontinue (type a b es) (k : (a, b, es) Continuation.t) e hs =
  let (Continuation (type e c) (cont : (a, c, e, es) continuation)) = k in
  let res : (c, e, es) res = discontinue cont e hs in
  (* Sound per the constraint on [Continuation.t]. *)
  (Obj.magic res : b)
;;

let discontinue_with_backtrace (type a b es) (k : (a, b, es) Continuation.t) e bt hs =
  let (Continuation (type e c) (cont : (a, c, e, es) continuation)) = k in
  let res : (c, e, es) res = discontinue_with_backtrace cont e bt hs in
  (* Sound per the constraint on [Continuation.t]. *)
  (Obj.magic res : b)
;;

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
    (Obj.magic res : (a, p, q, unit) result)
  ;;

  let run_with (type a p q es) hs f =
    let res : (a, (p, q) t, es) res = run_with hs f in
    (Obj.magic res : (a, p, q, es) result)
  ;;

  let perform (type a p q) (h : _ Handler.t) (op : (a, p, q, (p, q) t) ops) =
    let op : (a, (p, q) t) op = Obj.magic op in
    perform_ (Handler.globalize h, op) [@nontail]
  ;;
end

module Make_generic_contended (Types : Types) :
  S2_generic
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
    (Obj.magic res : (a, p, q, unit) result)
  ;;

  let run_with (type a p q es) hs f =
    let res : (a, (p, q) t, es) res = DRF.run_with hs f in
    (Obj.magic res : (a, p, q, es) result)
  ;;

  let perform (type a p q) (h : _ Handler.t) op : a =
    let op : (a, (p, q) t) op = Obj.magic op in
    (* Here and below, [magic_uncontended] is safe since the handler is either portable or
       is executed in its original capsule. *)
    let h = Obj.magic_uncontended h in
    perform_ (Handler.globalize h, op) [@nontail]
  ;;
end

module Make_generic_portable (Types : Types) :
  S2_generic
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
    (Obj.magic res : (a, p, q, unit) result)
  ;;

  let run_with (type a p q es) hs f =
    let res : (a, (p, q) t, es) res = DRF_portable.run_with hs f in
    (Obj.magic res : (a, p, q, es) result)
  ;;

  let perform (type a p q) (h : _ Handler.t) (op : (a, p, q, (p, q) t) ops) =
    let op : (a, (p, q) t) op = Obj.magic op in
    let h = Obj.magic_uncontended h in
    perform_ (Handler.globalize h, op) [@nontail]
  ;;
end

module Make_rec_base
    (T : sig
       type t
     end)
    (Ops : Operations_rec) =
struct
  type ('a, 'es) result =
    | Value : 'a -> ('a, 'es) result
    | Exception : exn -> ('a, 'es) result
    | Operation :
        ('o, T.t) Ops.t * ('o, ('a, 'es) result, 'es) Continuation.t
        -> ('a, 'es) result

  module Result = struct
    type ('a, 'es) t = ('a, 'es) result =
      | Value : 'a -> ('a, 'es) t
      | Exception : exn -> ('a, 'es) t
      | Operation :
          ('o, T.t) Ops.t * ('o, ('a, 'es) result, 'es) Continuation.t
          -> ('a, 'es) t
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
      type nonrec ('a, 'p, 'q, 'es) preemptible_result = ('a, 'p, 'q, 'es) result
    end)

  module Contended = struct
    include Make_rec_base (T) (Ops)

    include Make_generic_contended (struct
        type nonrec (_, _) t = t
        type nonrec ('a, _, _, 'e) ops = ('a, 'e) Ops.t
        type nonrec ('a, _, _, 'es) result = ('a, 'es) result
        type nonrec ('a, 'p, 'q, 'es) preemptible_result = ('a, 'p, 'q, 'es) result
      end)
  end

  module Portable = struct
    include Make_rec_base (T) (Ops)

    include Make_generic_portable (struct
        type nonrec (_, _) t = t
        type nonrec ('a, _, _, 'e) ops = ('a, 'e) Ops.t
        type nonrec ('a, _, _, 'es) result = ('a, 'es) result
        type nonrec ('a, 'p, 'q, 'es) preemptible_result = ('a, 'p, 'q, 'es) result
      end)
  end

  module Handler = struct
    type nonrec t = t Handler.t
  end

  module Continuation = struct
    type ('a, 'b, 'es) t = ('a, ('b, 'es) result, 'es) Continuation.t
  end
end

module Make (Ops : Operations) : S with type ('a, 'e) ops := 'a Ops.t = Make_rec (struct
    type ('a, 'e) t = 'a Ops.t
  end)

module Make1_rec_base
    (T : sig
       type 'p t
     end)
    (Ops : Operations1_rec) =
struct
  type ('a, 'p, 'es) result =
    | Value : 'a -> ('a, 'p, 'es) result
    | Exception : exn -> ('a, 'p, 'es) result
    | Operation :
        ('o, 'p, 'p T.t) Ops.t * ('o, ('a, 'p, 'es) result, 'es) Continuation.t
        -> ('a, 'p, 'es) result

  module Result = struct
    type ('a, 'p, 'es) t = ('a, 'p, 'es) result =
      | Value : 'a -> ('a, 'p, 'es) t
      | Exception : exn -> ('a, 'p, 'es) t
      | Operation :
          ('o, 'p, 'p T.t) Ops.t * ('o, ('a, 'p, 'es) result, 'es) Continuation.t
          -> ('a, 'p, 'es) t
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
      type nonrec ('a, 'p, 'q, 'es) preemptible_result = ('a, 'p, 'q, 'es) result
    end)

  module Contended = struct
    include Make1_rec_base (T) (Ops)

    include Make_generic_contended (struct
        type nonrec ('p, _) t = 'p t
        type nonrec ('a, 'p, _, 'e) ops = ('a, 'p, 'e) Ops.t
        type nonrec ('a, 'p, _, 'es) result = ('a, 'p, 'es) result
        type nonrec ('a, 'p, 'q, 'es) preemptible_result = ('a, 'p, 'q, 'es) result
      end)
  end

  module Portable = struct
    include Make1_rec_base (T) (Ops)

    include Make_generic_portable (struct
        type nonrec ('p, _) t = 'p t
        type nonrec ('a, 'p, _, 'e) ops = ('a, 'p, 'e) Ops.t
        type nonrec ('a, 'p, _, 'es) result = ('a, 'p, 'es) result
        type nonrec ('a, 'p, 'q, 'es) preemptible_result = ('a, 'p, 'q, 'es) result
      end)
  end

  module Handler = struct
    type nonrec 'p t = 'p t Handler.t
  end

  module Continuation = struct
    type ('a, 'b, 'p, 'es) t = ('a, ('b, 'p, 'es) result, 'es) Continuation.t
  end
end

module Make1 (Ops : Operations1) : S1 with type ('a, 'p, 'e) ops := ('a, 'p) Ops.t =
Make1_rec (struct
    type ('a, 'p, 'e) t = ('a, 'p) Ops.t
  end)

module Make2_rec_base
    (T : sig
       type ('p, 'q) t
     end)
    (Ops : Operations2_rec) =
struct
  type ('a, 'p, 'q, 'es) result =
    | Value : 'a -> ('a, 'p, 'q, 'es) result
    | Exception : exn -> ('a, 'p, 'q, 'es) result
    | Operation :
        ('o, 'p, 'q, ('p, 'q) T.t) Ops.t
        * ('o, ('a, 'p, 'q, 'es) result, 'es) Continuation.t
        -> ('a, 'p, 'q, 'es) result

  module Result = struct
    type ('a, 'p, 'q, 'es) t = ('a, 'p, 'q, 'es) result =
      | Value : 'a -> ('a, 'p, 'q, 'es) t
      | Exception : exn -> ('a, 'p, 'q, 'es) t
      | Operation :
          ('o, 'p, 'q, ('p, 'q) T.t) Ops.t
          * ('o, ('a, 'p, 'q, 'es) result, 'es) Continuation.t
          -> ('a, 'p, 'q, 'es) t
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
      type nonrec ('a, 'p, 'q, 'es) preemptible_result = ('a, 'p, 'q, 'es) result
    end)

  module Contended = struct
    include Make2_rec_base (T) (Ops)

    include Make_generic_contended (struct
        type nonrec ('p, 'q) t = ('p, 'q) t
        type nonrec ('a, 'p, 'q, 'e) ops = ('a, 'p, 'q, 'e) Ops.t
        type nonrec ('a, 'p, 'q, 'es) result = ('a, 'p, 'q, 'es) result
        type nonrec ('a, 'p, 'q, 'es) preemptible_result = ('a, 'p, 'q, 'es) result
      end)
  end

  module Portable = struct
    include Make2_rec_base (T) (Ops)

    include Make_generic_portable (struct
        type nonrec ('p, 'q) t = ('p, 'q) t
        type nonrec ('a, 'p, 'q, 'e) ops = ('a, 'p, 'q, 'e) Ops.t
        type nonrec ('a, 'p, 'q, 'es) result = ('a, 'p, 'q, 'es) result
        type nonrec ('a, 'p, 'q, 'es) preemptible_result = ('a, 'p, 'q, 'es) result
      end)
  end

  module Handler = struct
    type nonrec ('p, 'q) t = ('p, 'q) t Handler.t
  end

  module Continuation = struct
    type ('a, 'b, 'p, 'q, 'es) t = ('a, ('b, 'p, 'q, 'es) result, 'es) Continuation.t
  end
end

module Make2 (Ops : Operations2) :
  S2 with type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q) Ops.t = Make2_rec (struct
    type ('a, 'p, 'q, 'e) t = ('a, 'p, 'q) Ops.t
  end)

exception Continuation_already_resumed

type Stdlib.Printexc.t += Unhandled : 'e Handler.t -> Stdlib.Printexc.t

external magic_unyielding_global : 'a -> 'a = "%identity"

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
