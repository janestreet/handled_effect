open! Base

module type Handler = sig
  (** ['e t] is an effect handler for the effect ['e] on the current stack. *)
  type 'e t

  module List : sig
    type 'e handler := 'e t

    type 'es t =
      | [] : unit t
      | ( :: ) : 'e handler * 'es t -> ('e * 'es) t
      (** ['es t] is a typed list of [Handler.t]s *)

    module Length : sig
      type x = X

      type 'es t =
        | [] : unit t
        | ( :: ) : x * 'es t -> ('e * 'es) t
        (** ['es t] represents the length of a typed list of [Handler.t]s. It has slightly
            unusual constructors so that lengths can be written as [[X; X; X]] rather than
            e.g. [(S (S (S Z)))]. *)
    end

    (** [length hs] is the length of [hs] *)
    val length : 'es t @ local -> 'es Length.t
  end
end

module type Continuation = sig
  (** [('a, 'b, 'es) continuation] is a delimited continuation that expects an ['a] value
      and returns a ['b] value. It requires handlers for the effects ['es]. *)
  type (-'a, +'b, 'es) t : value mod contended many

  (** [get_callstack c n] returns a description of the top of the call stack on the
      continuation [c], with at most [n] entries. *)
  val get_callstack
    :  ('a, 'b, 'es) t @ unique
    -> int
    -> Backtrace.t Modes.Aliased.t * ('a, 'b, 'es) t @ unique
end

module Definitions (Handler : Handler) (Continuation : Continuation) = struct
  (** The generic signature for effects with 2 or fewer parameters.

      This is not a truly mode-polymorphic signature. Only select instances may be used,
      and each instance requires its specific result type for soundness. *)
  module type%template
    [@mode
      (p_arg, c_arg) = ((nonportable, uncontended), (portable, contended))
      , (p_res, c_res) = ((nonportable, uncontended), (portable, contended))] S2_generic = sig
    @@ portable
    (** [('p, 'q) t] represents the effect. It only appears as the argument to
        [Handler.t]. *)
    type ('p, 'q) t

    (** [('a, 'p, 'q, 'e) ops] is the type of operations of the effect [('p, 'q) t]. ['a]
        is the return type of the given operation. ['e] will be filled in with
        [('p, 'q) t] to tie the knot on recursive operations. *)
    type ('a, 'p, 'q, 'e) ops

    (** [('a, 'p, 'q, 'es) t] is the result of running a continuation until it either
        finishes and returns an ['a] value, raises an exception, or performs an operation. *)
    type ('a, 'p, 'q, 'es) result

    (** [fiber f] constructs a continuation that runs the computation [f]. [f] is passed a
        [t Handler.t] so that it can perform operations from the effect [t]. *)
    val fiber
      : ('a : value mod p_arg) 'b 'p 'q.
      (('p, 'q) t Handler.t @ c_res local p_arg -> 'a @ c_arg once unique -> 'b)
      @ once p_res
      -> ('a, ('b, 'p, 'q, unit) result, unit) Continuation.t @ p_res unique

    (** [fiber_with l f] constructs a continuation that runs the computation [f] and
        requires handlers for [l] additional effects. [f] is passed a typed list of
        handlers so that it can perform operations from the effect [t] and the effects
        ['es]. *)
    val fiber_with
      : ('a : value mod p_arg) 'b 'p 'q 'es.
      'es Handler.List.Length.t @ local
      -> ((('p, 'q) t * 'es) Handler.List.t @ c_res local p_arg
          -> 'a @ c_arg once unique
          -> 'b)
         @ once p_res
      -> ('a, ('b, 'p, 'q, 'es) result, 'es) Continuation.t @ p_res unique

    (** [run f] constructs a continuation that runs the computation [f] and immediately
        resumes it. *)
    val run
      :  (('p, 'q) t Handler.t @ c_res local p_arg -> 'a) @ once p_res
      -> ('a, 'p, 'q, unit) result @ once unique

    (** [run_with hs f] constructs a continuation that runs the computation [f] and
        immediately resumes it under the handlers [hs]. [f] is passed a typed list of
        handlers so that it can perform operations from the effect [t] and the effects
        ['es]. *)
    val run_with
      :  'es Handler.List.t @ c_res local p_arg
      -> ((('p, 'q) t * 'es) Handler.List.t @ c_res local p_arg -> 'a) @ once p_res
      -> ('a, 'p, 'q, 'es) result @ once unique

    (** [perform h e] performs an effect [e] that will be handled by [h] *)
    val perform
      : ('a : value mod p_arg) 'p 'q.
      ('p, 'q) t Handler.t @ c_arg local
      -> ('a, 'p, 'q, ('p, 'q) t) ops @ p_arg
      -> 'a @ c_arg once unique
  end

  (** The signature for effects with no type parameters at a combination of modes.

      This is not a truly mode-polymorphic signature. Only select instances may be used. *)
  module type%template
    [@mode
      (p_arg, c_arg) = ((nonportable, uncontended), (portable, contended))
      , (p_res, c_res) = ((nonportable, uncontended), (portable, contended))] S_base = sig
    type t
    type ('a, 'e) ops

    type ('a, 'es) result =
      | Value : 'a @@ global many -> ('a, 'es) result
      | Exception : exn @@ global many -> ('a, 'es) result
      | Operation :
          ('o, t) ops @@ c_arg global many
          * ('o, ('a, 'es) result, 'es) Continuation.t @@ p_res
          -> ('a, 'es) result
      (** [('a, 'es) t] is the result of running a continuation until it either finishes
          and returns an ['a] value, raises an exception, or performs an operation. *)

    include
      S2_generic
      [@mode p_arg p_res]
      with type (_, _) t := t
       and type ('a, _, _, 'e) ops := ('a, 'e) ops
       and type ('a, _, _, 'es) result := ('a, 'es) result

    module Result : sig @@ portable
      type ('a, 'es) t = ('a, 'es) result
    end
  end

  (** The signature for effects with no type parameters *)
  module type S = sig @@ portable
    type t

    (** [('a, 'e) ops] is the type of operations of effect [t]. ['a] is the return type of
        the given operation. ['e] will be filled in with [t] to tie the knot on recursive
        operations. *)
    type ('a, 'e) ops

    include S_base with type t := t and type ('a, 'e) ops := ('a, 'e) ops

    module Contended : sig
      include
        S_base
        [@mode portable nonportable]
        with type t := t
         and type ('a, 'e) ops := ('a, 'e) ops
    end

    module Portable : sig @@ portable
      include
        S_base
        [@mode portable portable]
        with type t := t
         and type ('a, 'e) ops := ('a, 'e) ops
    end

    module Handler : sig
      type nonrec t = t Handler.t
    end

    module Continuation : sig
      type ('a, 'b, 'es) t = ('a, ('b, 'es) result, 'es) Continuation.t
    end
  end

  (** The signature for effects with one type parameter at a combination of modes.

      This is not a truly mode-polymorphic signature. Only select instances may be used. *)
  module type%template
    [@mode
      (p_arg, c_arg) = ((nonportable, uncontended), (portable, contended))
      , (p_res, c_res) = ((nonportable, uncontended), (portable, contended))] S1_base = sig
    type 'p t
    type ('a, 'p, 'e) ops

    type ('a, 'p, 'es) result =
      | Value : 'a @@ global many -> ('a, 'p, 'es) result
      | Exception : exn @@ global many -> ('a, 'p, 'es) result
      | Operation :
          ('o, 'p, 'p t) ops @@ c_arg global many
          * ('o, ('a, 'p, 'es) result, 'es) Continuation.t @@ p_res
          -> ('a, 'p, 'es) result
      (** [('a, 'p, 'es) t] is the result of running a continuation until it either
          finishes and returns an ['a] value, raises an exception, or performs an
          operation. *)

    include
      S2_generic
      [@mode p_arg p_res]
      with type ('p, _) t := 'p t
       and type ('a, 'p, _, 'e) ops := ('a, 'p, 'e) ops
       and type ('a, 'p, _, 'e) result := ('a, 'p, 'e) result

    module Result : sig @@ portable
      type ('a, 'p, 'es) t = ('a, 'p, 'es) result
    end
  end

  (** The signature for effects with one type parameter *)
  module type S1 = sig @@ portable
    type 'p t

    (** [('a, 'p, 'e) ops] is the type of operations of effect ['p t]. ['a] is the return
        type of the given operation. ['e] will be filled in with ['p t] to tie the knot on
        recursive operations. *)
    type ('a, 'p, 'e) ops

    include S1_base with type 'p t := 'p t and type ('a, 'p, 'e) ops := ('a, 'p, 'e) ops

    module Contended : sig @@ portable
      include
        S1_base
        [@mode portable nonportable]
        with type 'p t := 'p t
         and type ('a, 'p, 'e) ops := ('a, 'p, 'e) ops
    end

    module Portable : sig @@ portable
      include
        S1_base
        [@mode portable portable]
        with type 'p t := 'p t
         and type ('a, 'p, 'e) ops := ('a, 'p, 'e) ops
    end

    module Handler : sig
      type nonrec 'p t = 'p t Handler.t
    end

    module Continuation : sig
      type ('a, 'b, 'p, 'es) t = ('a, ('b, 'p, 'es) result, 'es) Continuation.t
    end
  end

  (** The signature for effects with two type parameters at a combination of modes.

      This is not a truly mode-polymorphic signature. Only select instances may be used. *)
  module type%template
    [@mode
      (p_arg, c_arg) = ((nonportable, uncontended), (portable, contended))
      , (p_res, c_res) = ((nonportable, uncontended), (portable, contended))] S2_base = sig
    type ('p, 'q) t
    type ('a, 'p, 'q, 'e) ops

    type ('a, 'p, 'q, 'es) result =
      | Value : 'a @@ global many -> ('a, 'p, 'q, 'es) result
      | Exception : exn @@ global many -> ('a, 'p, 'q, 'es) result
      | Operation :
          ('o, 'p, 'q, ('p, 'q) t) ops @@ c_arg global many
          * ('o, ('a, 'p, 'q, 'es) result, 'es) Continuation.t @@ p_res
          -> ('a, 'p, 'q, 'es) result
      (** [('a, 'p, 'q, 'es) t] is the result of running a continuation until it either
          finishes and returns an ['a] value, raises an exception, or performs an
          operation. *)

    include
      S2_generic
      [@mode p_arg p_res]
      with type ('p, 'q) t := ('p, 'q) t
       and type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q, 'e) ops
       and type ('a, 'p, 'q, 'es) result := ('a, 'p, 'q, 'es) result

    module Result : sig @@ portable
      type ('a, 'p, 'q, 'es) t = ('a, 'p, 'q, 'es) result
    end
  end

  (** The signature for effects with two type parameters *)
  module type S2 = sig @@ portable
    type ('p, 'q) t

    (** [('a, 'p, 'q, 'e) ops] is the type of operations of the effect [('p, 'q) t]. ['a]
        is the return type of the given operation. ['e] will be filled in with
        [('p, 'q) t] to tie the knot on recursive operations. *)
    type ('a, 'p, 'q, 'e) ops

    include
      S2_base
      with type ('p, 'q) t := ('p, 'q) t
       and type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q, 'e) ops

    module Contended : sig @@ portable
      include
        S2_base
        [@mode portable nonportable]
        with type ('p, 'q) t := ('p, 'q) t
         and type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q, 'e) ops
    end

    module Portable : sig @@ portable
      include
        S2_base
        [@mode portable portable]
        with type ('p, 'q) t := ('p, 'q) t
         and type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q, 'e) ops
    end

    module Handler : sig
      type nonrec ('p, 'q) t = ('p, 'q) t Handler.t
    end

    module Continuation : sig
      type ('a, 'b, 'p, 'q, 'es) t = ('a, ('b, 'p, 'q, 'es) result, 'es) Continuation.t
    end
  end

  module type Operations = sig
    type 'a t
  end

  module type Operations_rec = sig
    type ('a, 'e) t
  end

  module type Operations1 = sig
    type ('a, 'p) t
  end

  module type Operations1_rec = sig
    type ('a, 'p, 'e) t
  end

  module type Operations2 = sig
    type ('a, 'p, 'q) t
  end

  module type Operations2_rec = sig
    type ('a, 'p, 'q, 'e) t
  end
end

module type Handled_effect = sig @@ portable
  module Handler : Handler
  module Continuation : Continuation

  (** [continue k v hs] resumes the continuation [k] with value [v]. [hs] are used to
      handle [k]'s additional effects. *)
  val continue
    :  ('a, 'b, 'es) Continuation.t @ unique
    -> 'a @ once unique
    -> 'es Handler.List.t @ local
    -> 'b @ once unique

  (** [discontinue k e hs] resumes the continuation [k] by raising the exception [e]. [hs]
      are used to handle [k]'s additional effects. *)
  val discontinue
    :  ('a, 'b, 'es) Continuation.t @ unique
    -> exn
    -> 'es Handler.List.t @ local
    -> 'b @ once unique

  (** [discontinue_with_backtrace k e bt hs] resumes the continuation [k] by raising the
      exception [e] using the raw backtrace [bt] as the origin of the exception. [hs] are
      used to handle [k]'s additional effects. *)
  val discontinue_with_backtrace
    :  ('a, 'b, 'es) Continuation.t @ unique
    -> exn
    -> Backtrace.t
    -> 'es Handler.List.t @ local
    -> 'b @ once unique

  include module type of struct
    include Definitions (Handler) (Continuation)
  end

  module Make (Ops : Operations) : S with type ('a, 'e) ops := 'a Ops.t
  module Make_rec (Ops : Operations_rec) : S with type ('a, 'e) ops := ('a, 'e) Ops.t
  module Make1 (Ops : Operations1) : S1 with type ('a, 'p, 'e) ops := ('a, 'p) Ops.t

  module Make1_rec (Ops : Operations1_rec) :
    S1 with type ('a, 'p, 'e) ops := ('a, 'p, 'e) Ops.t

  module Make2 (Ops : Operations2) :
    S2 with type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q) Ops.t

  module Make2_rec (Ops : Operations2_rec) :
    S2 with type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q, 'e) Ops.t

  (** Exception raised when a continuation is continued or discontinued more than once.

      Only achievable with [Obj.magic_unique] or similar escape hatches. *)
  exception Continuation_already_resumed
end
